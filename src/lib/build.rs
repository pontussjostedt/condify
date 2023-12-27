use std::rc::Rc;

use im::{HashMap, HashSet};

use super::dsl_parser::*;

struct Value<'a>(&'a str, Name<'a>);
struct Branch<'a> {
    name: Name<'a>,
    memory: im::HashMap<&'a str, Value<'a>>,
}

struct BuildState<'a> {
    accumulator: Rc<HashMap<&'a str, Branch<'a>>>,
    scope: im::HashSet<&'a str>,
    write_default: bool,
}

type BuildResult<'a> = Result<BuildState<'a>, BuildError<'a>>;

enum BuildError<'a> {
    DoubleDeclaration { name1: Name<'a>, name: Name<'a> },
}

impl BuildState<'_> {
    fn fold_declaration(&self, token: &Declaration) -> BuildResult {
        todo!()
    }

    fn fold_assignment(&self, token: &Assignment) -> BuildResult {
        todo!()
    }

    fn fold_if(&self, token: &If) -> BuildResult {
        todo!()
    }

    fn fold_read_value(&self, token: &ReadValue) -> BuildResult {
        todo!()
    }

    fn fold_free_text(&self, token: &str) -> BuildResult {
        todo!()
    }

    fn fold_any(&self, token: Token) -> BuildResult {
        match token {
            Token::Declaration(declaration) => self.fold_declaration(&declaration),
            Token::Assignment(assingment) => self.fold_assignment(&assingment),
            Token::FreeText(text) => self.fold_free_text(text),
            Token::If(if_inner) => self.fold_if(&if_inner),
            Token::ReadValue(read_value) => self.fold_read_value(&read_value),
        }
    }
}

fn build(tokens: Vec<Token>) -> BuildResult {
    todo!()
}
