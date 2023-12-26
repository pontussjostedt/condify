use std::collections::{HashMap, HashSet};

use crate::lib::dsl_parser::Name;

use super::dsl_parser::{Declaration, Token};

#[derive(Debug, PartialEq, Clone)]
struct DefaultBranch<'a> {
    pub string: String,
    pub values: HashMap<&'a str, &'a str>,
}

#[derive(Debug, PartialEq, Clone)]
struct Branch<'a> {
    pub string: String,
    pub values: HashMap<&'a str, &'a str>,
    pub name: Name<'a>,
}

impl Branch<'_> {
    fn from_default_branch<'a>(branch: &DefaultBranch<'a>, name: Name<'a>) -> Branch<'a> {
        Branch {
            string: branch.string.clone(),
            values: HashMap::new(),
            name: name,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct BuildContext<'a> {
    input: &'a str,
    default: DefaultBranch<'a>,
    memory: HashMap<&'a str, Branch<'a>>,
    tokens: Vec<Token<'a>>,
    write_default: bool,
}

impl<'a> BuildContext<'a> {
    fn add_branch(&mut self, key: &'a str, branch: Branch<'a>) {
        self.memory.insert(key, branch);
    }

    fn declare_names(&mut self, declaration: Declaration<'a>) {
        for name in declaration.declared {
            self.add_branch(name.name, Branch::from_default_branch(&self.default, name))
        }
    }
}

enum BuildError<'a> {
    NameAlreadyDeclared {
        first_declaration: Name<'a>,
        second_declaration: Name<'a>,
    },
}

type BuildResult<'a> = Result<BuildContext<'a>, BuildError<'a>>;

fn build_vec(ctx: &mut BuildContext) {
    for token in ctx.tokens.iter_mut() {
        todo!()
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    fn get_empty_buildcontext<'a>() -> BuildContext<'a> {
        BuildContext {
            write_default: true,
            default: DefaultBranch {
                string: "".to_string(),
                values: HashMap::new(),
            },
            input: "",
            memory: HashMap::new(),
            tokens: vec![],
        }
    }
    #[test]
    fn test_add_branch() {
        let mut ctx = get_empty_buildcontext();
        let name = "MY_NAME";
        let new_branch = Branch {
            string: String::from(""),
            values: HashMap::new(),
            name: Name {
                input: "",
                name: name,
            },
        };
        ctx.add_branch(name, new_branch.clone());

        assert!(ctx.memory.contains_key(name), "Key not present");
        assert_eq!(
            ctx.memory.get(name).expect("Should be assigned"),
            &new_branch
        );
    }
}
