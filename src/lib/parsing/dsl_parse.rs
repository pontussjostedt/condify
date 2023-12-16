use std::marker;

use nom::branch::alt;
use nom::bytes::complete::*;
use nom::character::complete::{anychar, crlf, newline, space0};
use nom::combinator::*;
use nom::error::{context, VerboseError};
use nom::multi::{many0, separated_list0};
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::IResult;

use super::error::CondifyError;
//type ParserType<'a> = FnMut(&str)

static IF_MARKER: &'static str = "!IF";
static ELSE_MARKER: &'static str = "!ELSE";
static ENDIF_MARKER: &'static str = "!ENDIF";
static START_DECLARE_MARKER: &'static str = "<";
static END_DEDCLARE_MARKER: &'static str = ">";
static ASSINGMENT_MARKER: &'static str = "ASSIGNMENT";
static DEFAULT_NAME: &'static str = "DEFAULT";
static READ_VALUE_MARKER: &'static str = "<*>";

#[derive(Debug, PartialEq)]
pub enum AssignmentArm<'a> {
    Default(&'a str),
    SpecificArm {
        include: Vec<&'a str>,
        value: &'a str,
    },
}

#[derive(Debug, PartialEq)]
pub enum TokenType<'a> {
    Declaration(Vec<&'a str>),
    FreeChar(char),
    ReadValue(&'a str),
    AssigmentBlock(Vec<AssignmentArm<'a>>),
    IfBlock {
        include: Vec<&'a str>,
        body: Vec<TokenType<'a>>,
        else_body: Option<Vec<TokenType<'a>>>,
    },
}

struct AssignmentState<'a> {
    valid_names: Vec<&'a str>
}

type ParseResult<'a, T: 'a> = IResult<&'a str, T, CondifyError<&'a str>>;

fn whitespace1<'a>(input: &'a str) -> ParseResult<'a, &str> {
    fn is_whitespace(c: char) -> bool {
        return c == ' ' || c == '\n'
    }
    take_while1(is_whitespace)(input)
}

fn name1<'a>(input: &'a str) -> ParseResult<'a, &str> {
    fn is_valid_name_char(c: char) -> bool {
        c.is_alphanumeric() || c == '_'
    }
    take_while1(is_valid_name_char)(input)
}


#[cfg(test)]
mod tests {
    use super::*;

    fn test_name1_accept() {
        let input = "THIS_IS_A_name6 Not Included";
        let expected_output: ParseResult<&str> = Ok((" Not Included", "THIS_IS_A_name6"));
        assert_eq!(name1(input), expected_output)
    }

    #[test]
    fn test_whitespace1() {
        let input = " \n s";
        let expected_output: ParseResult<&str> = Ok(("s", " \n "));
        assert_eq!(whitespace1(input), expected_output);
    }
}
