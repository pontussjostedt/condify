use std::collections::{HashMap, HashSet};

use super::{dsl_parser::*, markers::DEFAULT};

#[derive(Debug, PartialEq, Clone)]
pub enum BuildError<'a> {
    AlreadyAssigned(&'a Assignment<'a>),
    AlreadyDeclared(&'a Declaration<'a>),
    NotDeclaredOnAssingment(&'a Assignment<'a>),
    NotDeclaredOnReadValue(&'a ReadValue<'a>),
    NotAssignedOnReadValue(&'a ReadValue<'a>),
    NoDefaultValueOnReadValue(&'a ReadValue<'a>),
    NotInScopeOnIf(&'a If<'a>),
    DeclareDefault(&'a Declaration<'a>),
}

pub trait Visitor<'a, T> {
    fn visit_declaration(&mut self, declaration: &'a Declaration) -> T;
    fn visit_assignment(&mut self, assignment: &'a Assignment) -> T;
    fn visit_if(&mut self, if_block: &'a If) -> T;
    fn visit_read_value(&mut self, read_value: &'a ReadValue) -> T;
    fn visit_free_text(&mut self, free_text: &'a str) -> T;
    fn visit_any(&mut self, token: &'a Token<'a>) -> T;
}

#[derive(Debug, PartialEq, Clone)]
pub struct Memory<'a>(HashMap<&'a str, &'a str>);

#[derive(Debug, PartialEq, Clone)]
pub struct Branch<'a> {
    name: Name<'a>,
    memory: Memory<'a>,
    string: String,
}

impl<'a> Branch<'a> {
    fn new(name: Name<'a>, default_branch: &DefaultBranch<'a>) -> Branch<'a> {
        Branch {
            name,
            memory: default_branch.memory.clone(),
            string: default_branch.string.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct DefaultBranch<'a> {
    memory: Memory<'a>,
    string: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BuildState<'a> {
    branches: HashMap<&'a str, Branch<'a>>,
    default: DefaultBranch<'a>,
    scope: HashSet<&'a str>,
    write_default: bool,
}

impl<'a> Visitor<'a, Result<(), BuildError<'a>>> for BuildState<'a> {
    fn visit_declaration(
        &mut self,
        decl @ Declaration { input, declared }: &'a Declaration,
    ) -> Result<(), BuildError<'a>> {
        for name in declared {
            let name_str = name.name;
            if name_str == DEFAULT {
                return Err(BuildError::DeclareDefault(decl));
            } else if self.branches.contains_key(name_str) {
                return Err(BuildError::AlreadyDeclared(decl));
            }

            self.branches
                .insert(name.name, Branch::new(name.clone(), &self.default));

            self.scope.insert(name.name);
        }
        Ok(())
    }

    fn visit_assignment(
        &mut self,
        assignment @ Assignment {
            input: _,
            name: Name {
                input: _,
                name: var_name,
            },
            include,
            value,
        }: &'a Assignment,
    ) -> Result<(), BuildError<'a>> {
        for Name {
            input: _,
            name: branch_name,
        } in include
        {
            if branch_name == &DEFAULT {
                assert!(self.write_default, "Fix later");
                self.default.memory.0.insert(var_name, value);
            } else if let Some(branch) = self.branches.get_mut(branch_name) {
                branch.memory.0.insert(var_name, value);
            } else {
                return Err(BuildError::NotDeclaredOnAssingment(assignment));
            }
        }
        Ok(())
    }

    //todo fix different traversal for non global scope
    fn visit_if(
        &mut self,
        if_block_struct @ If {
            input: _,
            include,
            if_block,
            else_block,
        }: &'a If,
    ) -> Result<(), BuildError<'a>> {
        let old_include = self.scope.clone();
        self.scope.clear();
        for Name { input: _, name } in include {
            if old_include.contains(name) {
                let already_present = !self.scope.insert(name);
                //if already_present {
                //    return Err(BuildError::TwiceDeclaredOnIf(if_block_struct));
                //}
            } else {
                return Err(BuildError::NotInScopeOnIf(if_block_struct));
            }
        }

        for token in if_block {
            self.visit_any(token)?;
        }
        //let include_set: HashSet<&'a str> = HashSet::from_iter(include.iter().map(|n| n.name));
        let else_set: HashSet<&'a str> = old_include.difference(&self.scope).cloned().collect();
        if !else_set.is_empty() {
            if let Some(else_block) = else_block {
                self.scope = else_set;
                for token in else_block {
                    self.visit_any(token)?;
                }
            }
        }
        self.scope = old_include;
        Ok(())
    }

    fn visit_read_value(
        &mut self,
        read_value @ ReadValue {
            input: _,
            name: Name {
                input: _,
                name: var_name,
            },
        }: &'a ReadValue,
    ) -> Result<(), BuildError<'a>> {
        for branch_name in self.scope.iter() {
            let branch = self
                .branches
                .get_mut(branch_name)
                .expect("In scope but not declared");
            //.ok_or(BuildError::NotDeclaredOnReadValue(read_value))?;

            let value = branch
                .memory
                .0
                .get(var_name)
                .or(self.default.memory.0.get(var_name))
                .ok_or(BuildError::NotAssignedOnReadValue(read_value))?;

            branch.string.push_str(&value);
        }

        if self.write_default {
            let value = self
                .default
                .memory
                .0
                .get_mut(var_name)
                .ok_or(BuildError::NoDefaultValueOnReadValue(read_value))?;

            self.default.string.push_str(value);
        }

        Ok(())
    }

    fn visit_free_text(&mut self, free_text: &'a str) -> Result<(), BuildError<'a>> {
        if self.write_default {
            self.default.string.push_str(free_text);
        }

        for name in self.scope.iter() {
            self.branches
                .get_mut(name)
                .expect("In scope but not declared")
                .string
                .push_str(free_text);
        }

        Ok(())
    }

    fn visit_any(&mut self, token: &'a Token<'a>) -> Result<(), BuildError<'a>> {
        match token {
            Token::Declaration(declaration) => self.visit_declaration(declaration)?,
            Token::Assignment(assignment) => self.visit_assignment(assignment)?,
            Token::FreeText(free_text) => self.visit_free_text(free_text)?,
            Token::If(if_block) => self.visit_if(if_block)?,
            Token::ReadValue(read_value) => self.visit_read_value(read_value)?,
        }
        Ok(())
    }
}

impl BuildState<'_> {
    fn empty() -> Self {
        let branches = HashMap::new();
        BuildState {
            branches,
            default: DefaultBranch {
                memory: Memory(HashMap::new()),
                string: String::new(),
            },
            scope: HashSet::new(),
            write_default: true,
        }
    }

    pub fn get_accumulated_strings<'a>(&self) -> HashMap<&str, String> {
        let mut out: HashMap<&str, String> = HashMap::with_capacity(self.branches.len() + 1);
        out.insert(DEFAULT, self.default.string.clone());
        self.branches.iter().for_each(|(k, v)| {
            out.insert(k, v.string.clone());
        });
        out
    }
}

pub fn build<'a>(tokens: &'a Vec<Token<'a>>) -> Result<BuildState<'a>, BuildError<'a>> {
    let mut build_state = BuildState::empty();
    for token in tokens.iter() {
        build_state.visit_any(token)?;
    }

    Ok(build_state)
}

#[cfg(test)]
mod tests {
    use super::*;
    const BRANCH_1_NAME: &'static str = "DETAILED";
    const BRANCH_2_NAME: &'static str = "NO_DETAIL";
    const VAR_1_NAME: &'static str = "x";
    const VAR_1_VALUE: &'static str = "2";
    fn build_state_with_non_empty_default<'a>() -> BuildState<'a> {
        let mut build_state = BuildState::empty();
        build_state.default.string.push_str("Here is some text");
        build_state.default.memory.0.insert(VAR_1_NAME, VAR_1_VALUE);
        build_state
    }

    fn build_state_with_branches<'a>() -> BuildState<'a> {
        let mut build_state = BuildState::empty();
        build_state.branches.insert(
            BRANCH_1_NAME,
            Branch::new(tname(BRANCH_1_NAME), &build_state.default),
        );
        build_state.branches.insert(
            BRANCH_2_NAME,
            Branch::new(tname(BRANCH_2_NAME), &build_state.default),
        );
        build_state.scope.insert(BRANCH_1_NAME);
        build_state.scope.insert(BRANCH_2_NAME);
        build_state
    }

    fn build_state_with_branches_and_memory<'a>() -> BuildState<'a> {
        let mut build_state = build_state_with_branches();
        build_state.default.memory.0.insert(VAR_1_NAME, VAR_1_VALUE);
        for name in vec![BRANCH_1_NAME, BRANCH_2_NAME] {
            build_state
                .branches
                .get_mut(name)
                .unwrap()
                .memory
                .0
                .insert(VAR_1_NAME, VAR_1_VALUE);
        }
        build_state
    }

    fn tname<'a>(name: &'a str) -> Name<'a> {
        Name {
            input: "TEST",
            name: name,
        }
    }

    #[test]
    fn test_build_free_text_ok() {
        let mut build_state = build_state_with_branches();
        let free_text = "This is a text";

        let result = build_state.visit_free_text(free_text);
        assert!(result.is_ok());

        assert_eq!(build_state.default.string, free_text);

        for name in build_state.scope.iter() {
            assert_eq!(build_state.branches.get(name).unwrap().string, free_text);
        }
    }

    #[test]
    fn test_build_read_value_ok() {
        let mut build_state = build_state_with_branches_and_memory();
        let read_value = ReadValue {
            input: "TEST",
            name: tname(VAR_1_NAME),
        };

        let result = build_state.visit_read_value(&read_value);
        assert!(result.is_ok());
        assert_eq!(build_state.default.string, VAR_1_VALUE);
        for name in build_state.scope.iter() {
            assert_eq!(build_state.branches.get(name).unwrap().string, VAR_1_VALUE);
        }
    }

    #[test]
    fn test_build_read_value_err_no_default() {
        let mut build_state = build_state_with_branches_and_memory();
        let read_value = ReadValue {
            input: "TEST",
            name: tname(VAR_1_NAME),
        };
        build_state.default.memory.0.clear();

        let result = build_state.visit_read_value(&read_value);
        assert_eq!(
            result,
            Err(BuildError::NoDefaultValueOnReadValue(&read_value))
        );
    }

    #[test]
    fn test_build_read_value_err_not_assinged() {
        let mut build_state = build_state_with_branches();
        let read_value = ReadValue {
            input: "TEST",
            name: tname("NOT_IN_MEMORY"),
        };

        let result = build_state.visit_read_value(&read_value);
        assert_eq!(result, Err(BuildError::NotAssignedOnReadValue(&read_value)));
    }

    #[test]
    fn test_build_assignment_default_ok() {
        let mut build_state = build_state_with_branches();
        let assignment = Assignment {
            input: "TEST",
            name: tname(VAR_1_NAME),
            include: vec![tname(DEFAULT)],
            value: VAR_1_VALUE,
        };
        let result = build_state.visit_assignment(&assignment);
        assert!(result.is_ok());

        let default_value = build_state
            .default
            .memory
            .0
            .get(VAR_1_NAME)
            .expect("Memory not set for default");

        assert_eq!(default_value, &VAR_1_VALUE);
    }

    #[test]
    fn test_build_assingment_specific_ok() {
        let mut build_state = build_state_with_branches();
        let assignment = Assignment {
            input: "TEST",
            name: tname(VAR_1_NAME),
            include: vec![tname(BRANCH_1_NAME)],
            value: VAR_1_VALUE,
        };

        let result = build_state.visit_assignment(&assignment);
        assert!(result.is_ok());

        let memory = &build_state
            .branches
            .get(BRANCH_1_NAME)
            .expect("Should exist not this tests fault")
            .memory
            .0;

        assert_eq!(memory.get(VAR_1_NAME).expect("var not set"), &VAR_1_VALUE);
        assert!(build_state
            .branches
            .get(BRANCH_2_NAME)
            .expect("Should exist not this test fault")
            .memory
            .0
            .get(VAR_1_NAME)
            .is_none());
    }

    //Todo: check errors
    #[test]
    fn test_build_declaration_ok() {
        let mut build_state = build_state_with_non_empty_default();
        let declaration = Declaration {
            input: "<DETAILED>",
            declared: vec![Name {
                input: "DETAILED>",
                name: "DETAILED",
            }],
        };

        let result = build_state.visit_declaration(&declaration);

        println!("{:?}", build_state);

        let detailed_branch = build_state
            .branches
            .get("DETAILED")
            .expect("Failed to add branch");

        let var = detailed_branch
            .memory
            .0
            .get(VAR_1_NAME)
            .expect("failed to set memory");

        assert_eq!(var, &VAR_1_VALUE, "value not same");
        assert_eq!(build_state.default.string, detailed_branch.string);
        assert!(build_state.scope.contains("DETAILED"));
        assert!(result.is_ok());
    }
}
