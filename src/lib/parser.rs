use std::marker;

use nom::branch::alt;
use nom::bytes::complete::*;
use nom::character::complete::{space0, crlf, newline};
use nom::combinator::verify;
use nom::error::{ParseError, VerboseError, VerboseErrorKind, Error, ErrorKind};
use nom::sequence::preceded;
use nom::{Compare, Err, IResult, InputLength, InputTake, Parser};
//type ParserType<'a> = FnMut(&str)

#[derive(Debug)]
pub enum AssignmentArm<'a> {
    Default(&'a str),
    SpecificArm {
        include: Vec<&'a str>,
        value: &'a str,
    },
}

#[derive(Debug)]
pub enum ASTType<'a> {
    ArmDefinition(Vec<&'a str>),
    FreeChar(&'a char),
    FreeText(&'a char),
    AssigmentBlock {
        arms: Vec<AssignmentArm<'a>>,
    },
    IfBlock {
        include: Vec<&'a str>,
        body: Vec<ASTType<'a>>,
        else_block: Option<Vec<ASTType<'a>>>,
    },
}

static IF_MARKER: &'static str = "!IF";
static ELSE_MARKER: &'static str = "!ELSE";
static ENDIF_MARKER: &'static str = "!ENDIF";

type ParseResult<'a> = IResult<&'a str, &'a str, VerboseError<&'a str>>;

fn valid_front<'a>(input: &'a str) -> ParseResult<'a> {
    verify(take(1 as usize), |s: &str| s == "\n" || s == " ")(input) // TODO add other space cases
}

fn marker<'a>(marker: &'a str) -> impl Fn(&'a str) -> ParseResult<'a> {
    move |i: &str| preceded(valid_front, tag::<&'a str, &'a str, VerboseError<&str>>(marker))(i)
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_marker() {
        assert_eq!(tag::<&str, &str, VerboseError<&str>>("\n")("\n"), Ok(("", "\n")));
        let test_chars = vec![' ', '\n'];
        for test_char in test_chars {
            let input_str = format!("{}{}", test_char, IF_MARKER);
            {   
                let parser = marker(IF_MARKER);
                assert_eq!(parser(input_str.as_str()), Ok(("", IF_MARKER)));
            };

        }

    }
}
