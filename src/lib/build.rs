use std::collections::{HashMap, HashSet};

use crate::lib::dsl_parser::Name;

use self::visit::Visitor;

use super::dsl_parser::{Assignment, Declaration, If, ReadValue, Token};

mod visit {
    use crate::lib::dsl_parser::*;
    pub trait Visitor<T> {
        fn visit_name(&mut self, name: &Name) -> T;
        fn visit_declaration(&mut self, declaration: &Declaration) -> T;
        fn visit_if(&mut self, ifblock: &If) -> T;
        fn visit_read_value(&mut self, read_value: &ReadValue) -> T;
        fn visit_assignment(&mut self, assignment: &Assignment) -> T;
        fn visit_free_text(&mut self, free_text: &str) -> T;
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct DefaultBranch<'a> {
    pub string: String,
    pub values: HashMap<&'a str, &'a str>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Branch<'a> {
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
pub struct BuildContext<'a> {
    input: &'a str,
    default: DefaultBranch<'a>,
    memory: HashMap<&'a str, Branch<'a>>,
    write_default: bool,
}

impl<'a> BuildContext<'a> {}

#[derive(Debug, PartialEq, Clone)]
pub enum BuildError<'a> {
    NameAlreadyDeclared {
        first_declaration: Name<'a>,
        second_declaration: Name<'a>,
    },
}

type BuildResult<'a> = Result<(), BuildError<'a>>;

impl<'a> Visitor<BuildResult<'a>> for BuildContext<'_> {
    fn visit_name(&mut self, name: &Name) -> BuildResult<'a> {
        println!("Visited name");
        Ok(())
    }

    fn visit_declaration(&mut self, declaration: &Declaration) -> BuildResult<'a> {
        println!("Visited declaration");
        Ok(())
    }

    fn visit_if(&mut self, ifblock: &If) -> BuildResult<'a> {
        println!("Visited if");
        for x in &ifblock.if_block {
            self.visit_any(x);
        }
        Ok(())
    }

    fn visit_read_value(&mut self, read_value: &ReadValue) -> BuildResult<'a> {
        println!("Visited read value");
        Ok(())
    }

    fn visit_assignment(&mut self, assignment: &Assignment) -> BuildResult<'a> {
        Ok(())
    }

    fn visit_free_text(&mut self, free_text: &str) -> BuildResult<'a> {
        Ok(())
    }
}

impl<'a> BuildContext<'_> {
    fn visit_any(&mut self, token: &Token<'a>) -> BuildResult<'a> {
        match token {
            Token::Declaration(declaration) => self.visit_declaration(&declaration),
            Token::Assignment(assignment) => self.visit_assignment(&assignment),
            Token::FreeText(free_text) => self.visit_free_text(free_text),
            Token::If(ifblock) => self.visit_if(&ifblock),
            Token::ReadValue(read_value) => self.visit_read_value(&read_value),
        }
    }
}

pub fn build<'a>(input: &'a str, tokens: Vec<Token<'a>>) -> BuildResult<'a> {
    let mut build_context = BuildContext {
        default: DefaultBranch {
            string: String::from(""),
            values: HashMap::new(),
        },
        input: input,
        memory: HashMap::new(),
        write_default: true,
    };
    for token in tokens {
        println!("{:?}", token);
        build_context.visit_any(&token)?;
    }
    Ok(())
}
