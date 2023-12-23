use super::{
    error::*,
    markers::{self, *},
};

use nom::{
    bytes::complete::*,
    error::{Error, ParseError},
    multi::{many0, separated_list0},
    sequence::delimited,
    IResult, InputLength, Parser,
};
type ParseResult<'a, O, I = &'a str> = IResult<I, O, Error<I>>;

#[derive(Debug, PartialEq)]
struct Name<'a> {
    input: &'a str,
    name: &'a str,
}
#[derive(Debug, PartialEq)]
enum Token<'a> {
    Declare {
        input: &'a str,
        declared: Vec<Name<'a>>,
    },
    Assignment {
        input: &'a str,
        include: Vec<Name<'a>>,
        value: &'a str,
    },
    FreeText(&'a str),
}

fn whitespace0(input: &str) -> ParseResult<&str> {
    take_while(|c: char| c.is_whitespace())(input)
}

fn name1(input: &str) -> ParseResult<Name> {
    take_while(|c: char| c.is_alphanumeric() || c == '_')(input).map(|(rest, result)| {
        (
            rest,
            Name {
                input,
                name: result,
            },
        )
    })
}

mod tests {
    use super::*;

    #[test]
    fn test_name1() {
        let input = "c00l_NAME\" rest";
        let expected_output: ParseResult<Name> = Ok((
            "\" rest",
            Name {
                input,
                name: "c00l_NAME",
            },
        ));
        assert_eq!(name1(input), expected_output)
    }

    #[test]
    fn test_whitespace0() {
        let input = " \n\r rest";
        let expected_output: ParseResult<&str> = Ok(("rest", " \n\r "));
        assert_eq!(whitespace0(input), expected_output);
    }
}
