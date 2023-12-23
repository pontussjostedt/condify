use super::{
    error::*,
    markers::{self, *},
};

use nom::{
    bytes::complete::*,
    combinator::cut,
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
        name: Name<'a>,
        include: Vec<Name<'a>>,
        value: &'a str,
    },
    FreeText(&'a str),
    If {
        input: &'a str,
        include: Vec<Name<'a>>,
        if_block: Vec<Token<'a>>,
        else_block: Option<Vec<Token<'a>>>,
    },
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

fn list0<'a>(
    delimiter_start: &'static str,
    delimiter_end: &'static str,
) -> impl Fn(&'a str) -> ParseResult<'a, Vec<Name<'a>>> {
    move |input: &'a str| {
        delimited(
            tag(delimiter_start),
            separated_list0(
                terminated(nom::character::complete::char(','), whitespace0),
                name1,
            ),
            cut(tag(delimiter_end)),
        )(input)
    }
}

fn declaration(input: &str) -> ParseResult<Token> {
    list0(DECLARATION_DELIMITER_START, DECLARATION_DELIMITER_END)(input).map(|(rest, names)| {
        (
            rest,
            Token::Declare {
                input: input,
                declared: names,
            },
        )
    })
}

fn assignment(input: &str) -> ParseResult<Token> {
    let (rest, name) = name1(input)?;
    let (rest, _) = tuple((whitespace0, tag("FOR"), whitespace0))(rest)?;
    let (rest, include) = list0(ASSIGNMENT_DELIMITER_START, ASSIGNMENT_DELIMITER_END)(rest)?;
    let (rest, _) = tuple((whitespace0, cut(tag("IS")), whitespace0))(rest)?;
    let (rest, str_litteral) = cut(str_litteral)(rest)?;
    Ok((
        rest,
        Token::Assignment {
            input,
            name,
            include,
            value: str_litteral,
        },
    ))
}

mod tests {
    use super::*;

    #[test]
    fn test_declaration() {
        let input = "<name1, name2> rest";
        let expected_output: ParseResult<Token> = Ok((
            " rest",
            Token::Declare {
                input,
                declared: vec![
                    Name {
                        input: "name1, name2> rest",
                        name: "name1",
                    },
                    Name {
                        input: "name2> rest",
                        name: "name2",
                    },
                ],
            },
        ));

        assert_eq!(declaration(input), expected_output);
    }

    #[test]
    fn test_list0_separated_whitespace() {
        let input = "(a, b,\n c) rest";
        let expected_output: ParseResult<Vec<Name>> = Ok((
            " rest",
            vec![
                Name {
                    input: &input[1..],
                    name: "a",
                },
                Name {
                    input: &input[4..],
                    name: "b",
                },
                Name {
                    input: &input[8..],
                    name: "c",
                },
            ],
        ));

        assert_eq!(list0("(", ")")(input), expected_output);
    }

    #[test]
    fn test_assignment() {
        let input = "number FOR ALL_THE_DETAIL, NO_DETAIL IS \"MyString\" rest";
        let expected_output: ParseResult<Token> = Ok((
            " rest",
            Token::Assignment {
                input: "number FOR ALL_THE_DETAIL, NO_DETAIL IS \"MyString\" rest",
                name: Name {
                    input,
                    name: "number",
                },
                include: vec![
                    Name {
                        input: "ALL_THE_DETAIL, NO_DETAIL IS \"MyString\" rest",
                        name: "ALL_THE_DETAIL",
                    },
                    Name {
                        input: "NO_DETAIL IS \"MyString\" rest",
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
