use std::{
    ops::{Add, Deref, DerefMut},
    rc::Rc,
};

use im::hashmap;
use im::{HashMap, HashSet};
use imstr::data::Data;

use super::dsl_parser::*;
#[derive(Debug, PartialEq, Clone)]
struct Memory<'a>(im::HashMap<&'a str, &'a str>);
impl<'a> Memory<'a> {
    fn new() -> Self {
        Memory(im::HashMap::new())
    }
}
#[derive(Debug, PartialEq, Clone)]
struct Value<'a>(&'a str, Name<'a>);
#[derive(Debug, PartialEq, Clone)]
struct Branch<'a> {
    name: Name<'a>,
    memory: Memory<'a>,
    string: Rc<String>,
}

impl<'a> Branch<'a> {
    fn assigned(&self, value_name: &'a str, value: &'a str) -> Result<Self, BuildError<'a>> {
        Ok(Branch {
            memory: Memory(self.memory.0.update(value_name, value)),
            ..self.clone()
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
struct DefaultBranch<'a> {
    memory: Memory<'a>,
    string: Rc<String>,
}

impl<'a> DefaultBranch<'a> {
    fn to_branch(&self, name: Name<'a>) -> Branch<'a> {
        Branch {
            name: name,
            memory: self.memory.clone(),
            string: self.string.clone(),
        }
    }

    fn assigned(&self, value_name: &Name<'a>, value: &'a str) -> Result<Self, BuildError<'a>> {
        Ok(DefaultBranch {
            memory: Memory(self.memory.0.update(value_name.name, value)),
            ..self.clone()
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
struct BuildState<'a> {
    branches: im::HashMap<&'a str, Branch<'a>>,
    scope: im::HashSet<Name<'a>>,
    write_default: bool,
    default: DefaultBranch<'a>,
}

impl<'a> BuildState<'a> {
    fn declared(&self, name: Name<'a>) -> BuildState<'a> {
        BuildState {
            branches: self
                .branches
                .update(name.name, self.default.to_branch(name)),
            scope: self.scope.clone(),
            write_default: self.write_default,
            default: self.default.clone(),
        }
    }

    fn assigned(
        &self,
        value_name: &Name<'a>,
        to_include: &Name<'a>,
        value: &'a str,
    ) -> BuildResult<'a> {
        let out = if to_include.name == "DEFAULT" && self.write_default {
            let new_default = self.default.assigned(&value_name, value)?;
            Ok(BuildState {
                default: new_default,
                ..self.clone()
            })
        } else if let Some(branch) = self.branches.get(to_include.name) {
            let new_branch = branch.assigned(value_name.name, value)?;
            Ok(BuildState {
                branches: self.branches.update(to_include.name, new_branch),
                ..self.clone()
            })
        } else {
            Err(BuildError::NotDeclared(to_include.clone()))
        }?;
        Ok(out)
    }

    fn get_value(
        &self,
        branch_name: &Name<'a>,
        name: &Name<'a>,
    ) -> Result<&'a str, BuildError<'a>> {
        if name.name == "DEFAULT" {
            self.default
                .memory
                .0
                .get(name.name)
                .map_or(Err(BuildError::NotAssigned(name.clone())), |v| Ok(v))
        } else {
            self.branches.get(name.name).map_or(
                Err(BuildError::NotDeclared(branch_name.clone())),
                |branch| {
                    branch
                        .memory
                        .0
                        .get(name.name)
                        .map_or(Err(BuildError::NotAssigned(name.clone())), |v| Ok(*v))
                },
            )
        }
    }

    fn push_default_value(&self, name: &Name<'a>) -> Result<(), BuildError<'a>> {
        assert!(self.write_default);
        self.default.memory.0.get(name.name).map_or(
            Err(BuildError::NotAssigned(name.clone())),
            |v| {
                Rc::make_mut(&mut self.default.string.clone()).push_str(v);

                Ok(())
            },
        )
    }

    fn push_value(&self, branch_name: &Name<'a>, name: &Name<'a>) -> Result<(), BuildError<'a>> {
        self.branches.get(name.name).map_or(
            Err(BuildError::NotDeclared(branch_name.clone())),
            |branch| {
                branch.memory.0.get(name.name).map_or(
                    Err(BuildError::NotAssigned(name.clone())),
                    |v| {
                        Rc::make_mut(&mut branch.string.clone()).push_str(v);
                        Ok(())
                    },
                )
            },
        )
    }
}

type BuildResult<'a> = Result<BuildState<'a>, BuildError<'a>>;

#[derive(Debug, PartialEq, Clone)]
enum BuildError<'a> {
    DoubleDeclaration {
        already_declared: Name<'a>,
        attempted_declare: Name<'a>,
    },
    NotDeclared(Name<'a>),
    NotAssigned(Name<'a>),
    Unkown(),
}

impl<'a> BuildState<'a> {
    fn fold_declaration(&self, token: &Declaration<'a>) -> BuildResult<'a> {
        let mut out: BuildState<'a> = self.clone();
        for name in token.declared.iter() {
            out = out.declared(name.clone());
        }
        Ok(out)
    }

    fn fold_assignment(
        &self,
        Assignment {
            input,
            name,
            include,
            value,
        }: &Assignment<'a>,
    ) -> BuildResult<'a> {
        let mut out: BuildState = self.clone();
        for include_name in include {
            out = self.assigned(name, include_name, value)?;
        }
        Ok(out)
    }

    fn fold_if(&self, if_block: &If<'a>) -> BuildResult<'a> {
        let new_state = self.clone();
        let mut if_state = BuildState {
            scope: new_state
                .scope
                .intersection(if_block.include.iter().cloned().collect()),
            ..new_state
        };
        for token in if_block.if_block.iter() {
            if_state = if_state.fold_any(token)?;
        }
        if let Some(else_block) = &if_block.else_block {
            let new_state = self.clone();
            let mut else_state = BuildState {
                scope: new_state
                    .scope
                    .difference(if_block.include.iter().cloned().collect()),
                ..new_state
            };
            for token in else_block.iter() {
                else_state = else_state.fold_any(token)?;
            }
        }
        Ok(self.clone())
    }

    fn fold_read_value(&self, token: &ReadValue<'a>) -> BuildResult<'a> {
        if self.write_default {
            self.push_default_value(&token.name)?;
        }
        for branch_name in self.scope.iter() {
            self.push_value(branch_name, &token.name)?;
        }
        Ok(self.clone())
    }

    fn fold_free_text(&self, token: &str) -> BuildResult<'a> {
        if self.write_default {
            Rc::make_mut(&mut self.default.string.clone()).push_str(token);
        }

        for branch_name in self.scope.iter() {
            if let Some(branch) = self.branches.get(branch_name.name) {
                Rc::make_mut(&mut branch.string.clone()).push_str(token);
            } else {
                panic!("Should not happen");
            }
        }

        Ok(self.clone())
    }

    fn fold_any(&self, token: &Token<'a>) -> BuildResult<'a> {
        match token {
            Token::Declaration(declaration) => self.fold_declaration(&declaration),
            Token::Assignment(assingment) => self.fold_assignment(&assingment),
            Token::FreeText(text) => self.fold_free_text(text),
            Token::If(if_inner) => self.fold_if(&if_inner),
            Token::ReadValue(read_value) => self.fold_read_value(&read_value),
        }
    }
}

fn build<'a>(tokens: Vec<Token<'a>>) -> BuildResult<'a> {
    let mut next_state = BuildState {
        branches: im::HashMap::new(),
        scope: im::HashSet::new(),
        write_default: true,
        default: DefaultBranch {
            memory: Memory::new(),
            string: Rc::new(String::from("")),
        },
    };

    for token in tokens.iter() {
        next_state = next_state.fold_any(&token)?;
    }
    todo!()
}

#[cfg(test)]
mod tests {
    use im::hashset;

    use super::*;

    fn empty_state<'a>() -> BuildState<'a> {
        BuildState {
            branches: im::HashMap::new(),
            scope: im::HashSet::new(),
            write_default: true,
            default: DefaultBranch {
                memory: Memory::new(),
                string: Rc::new(String::from("")),
            },
        }
    }

    fn empty_branch<'a>(name: &'a str) -> Branch<'a> {
        Branch {
            name: simple_name(name),
            memory: Memory::new(),
            string: Rc::new(String::from("")),
        }
    }

    fn simple_name(name: &str) -> Name {
        Name {
            input: "Wont be used",
            name: name,
        }
    }

    #[test]
    fn test_fold_declaration_ok() {
        let name1 = "DETAILED";
        let name2 = "NO_DETAIL";
        let input = Declaration {
            input: "Some string",
            declared: vec![simple_name(name1), simple_name(name2)],
        };

        let expected_output: BuildResult = Ok(BuildState {
            branches: hashmap! {name1 => empty_branch(name1), name2 => empty_branch(name2)},
            ..empty_state()
        });

        assert_eq!(empty_state().fold_declaration(&input), expected_output);
    }

    #[test]
    fn test_free_text() {
        let input_name1 = "DETAIL";
        let input_name2 = "NO_DETAIL";
        let init_state = BuildState {
            scope: hashset![simple_name(input_name1), simple_name(input_name2)],
            branches: hashmap! {input_name1 => empty_branch(input_name1), input_name2 => empty_branch(input_name2)},
            ..empty_state()
        };

        let input_string = "Thing";
        let input = &Token::FreeText(input_string);

        println!("{:?}", init_state.fold_any(input));
    }
}
