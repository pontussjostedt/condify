use std::{
    ops::{Add, Deref},
    rc::Rc,
};

use im::hashmap;
use im::{HashMap, HashSet};

use super::dsl_parser::*;
#[derive(Debug, PartialEq, Clone)]
struct Value<'a>(&'a str, Name<'a>);
#[derive(Debug, PartialEq, Clone)]
struct Branch<'a> {
    name: Name<'a>,
    memory: im::HashMap<&'a str, Value<'a>>,
    string: Rc<String>,
}

#[derive(Debug, Clone, PartialEq)]
struct DefaultBranch<'a> {
    memory: im::HashMap<&'a str, Value<'a>>,
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
}

#[derive(Debug, PartialEq, Clone)]
struct BuildState<'a> {
    branches: im::HashMap<&'a str, Branch<'a>>,
    scope: im::HashSet<&'a str>,
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
}

type BuildResult<'a> = Result<BuildState<'a>, BuildError<'a>>;

#[derive(Debug, PartialEq, Clone)]
enum BuildError<'a> {
    DoubleDeclaration {
        already_declared: Name<'a>,
        attempted_declare: Name<'a>,
    },
}

impl<'a> BuildState<'a> {
    fn fold_declaration(&self, token: &Declaration<'a>) -> BuildResult<'a> {
        let mut out: BuildState<'a> = self.clone();
        for name in token.declared.iter() {
            out = out.declared(name.clone());
        }
        Ok(out)
    }

    fn fold_assignment(&self, token: &Assignment<'a>) -> BuildResult<'a> {
        todo!()
    }

    fn fold_if(&self, token: &If<'a>) -> BuildResult<'a> {
        todo!()
    }

    fn fold_read_value(&self, token: &ReadValue<'a>) -> BuildResult<'a> {
        todo!()
    }

    fn fold_free_text(&self, token: &str) -> BuildResult<'a> {
        todo!()
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
            memory: HashMap::new(),
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
    use super::*;

    fn empty_state<'a>() -> BuildState<'a> {
        BuildState {
            branches: im::HashMap::new(),
            scope: im::HashSet::new(),
            write_default: true,
            default: DefaultBranch {
                memory: HashMap::new(),
                string: Rc::new(String::from("")),
            },
        }
    }

    fn empty_branch<'a>(name: &'a str) -> Branch<'a> {
        Branch {
            name: simple_name(name),
            memory: im::HashMap::new(),
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
}
