use super::{
    error::*,
    markers::{self, *},
};

use nom::{
    bytes::complete::*,
    error::{Error, ParseError},
    multi::{many0, separated_list0},
    sequence::{delimited, preceded, terminated, tuple},
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

fn str_litteral(input: &str) -> ParseResult<&str> {
    delimited(
        nom::character::complete::char('"'),
        take_while(|c| c != '"'),
        nom::character::complete::char('"'),
    )(input)
}

fn assignment(input: &str) -> ParseResult<Token> {
    let (rest, include) = separated_list0(
        terminated(nom::character::complete::char(','), whitespace0),
        name1,
    )(input)?;
    println!("rest after include: {:?}", rest);
    let (rest, _) = tuple((
        whitespace0,
        nom::character::complete::char('='),
        whitespace0,
    ))(rest)?;
    let (rest, str_litteral) = str_litteral(rest)?;
    Ok((
        rest,
        Token::Assignment {
            input: input,
            include: include,
            value: str_litteral,
        },
    ))
}

mod tests {
    use super::*;

    #[test]
    fn test_assignment() {
        let input = "ALL_THE_DETAIL, NO_DETAIL = \"MyString\" rest";
        let expected_output: ParseResult<Token> = Ok((
            " rest",
            Token::Assignment {
                input: input,
                include: vec![
                    Name {
                        input: input,
                        name: "ALL_THE_DETAIL",
                    },
                    Name {
                        input: "NO_DETAIL = \"MyString\" rest",
                        name: "NO_DETAIL",
                    },
                ],
                value: "MyString",
            },
        ));
        assert_eq!(assignment(input), expected_output);
    }

    #[test]
    fn test_str_litteral() {
        let input = "\"MyString \" rest";
        let expected_output: ParseResult<&str> = Ok((" rest", "MyString "));
        assert_eq!(str_litteral(input), expected_output);
    }

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
