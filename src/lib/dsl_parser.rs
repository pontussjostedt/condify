use std::ptr::addr_of_mut;

use super::{
    error::*,
    markers::{self, *},
};

use nom::{
    branch::alt,
    bytes::complete::*,
    character::complete::space1,
    combinator::{cut, not, opt, peek},
    error::{Error, ParseError},
    multi::{many0, separated_list0},
    sequence::{delimited, preceded, terminated, tuple},
    IResult, InputLength, Offset, Parser,
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

fn if_parse(input: &str) -> ParseResult<Token> {
    let (rest, _) = tuple((tag(IF),))(input)?;
    let (rest, names) = list0("(", ")")(rest)?;
    let (rest, body) = parse_until(alt((tag(IF_ELSE), tag(IF_END))))(rest)?;
    let (rest, else_body) = opt(preceded(tag(IF_ELSE), parse_until(tag(IF_END))))(rest)?;
    let (rest, _) = tag(IF_END)(rest)?;
    Ok((
        rest,
        Token::If {
            input,
            include: names,
            if_block: body,
            else_block: else_body,
        },
    ))
}

///Consumes until either 'parser fails' or 'to_break' succeeds, if it does the result is kept.
fn many0_until<'a, P, B, O, O2>(
    mut parser: P,
    mut to_break: B,
) -> impl FnMut(&'a str) -> ParseResult<(Vec<O>, Option<O2>)>
where
    P: Parser<&'a str, O, Error<&'a str>>,
    B: Parser<&'a str, O2, Error<&'a str>>,
{
    move |input: &'a str| {
        let mut out_vec: Vec<O> = Vec::with_capacity(4); //4 is from what nom uses as initial capacity, seems like a reasonable number
        let mut outer_rest = input;
        loop {
            match to_break.parse(outer_rest) {
                Ok((rest, result)) => return Ok((rest, (out_vec, Some(result)))),
                Err(nom::Err::Failure(e)) => return Err(nom::Err::Failure(e)),
                _ => (),
            }

            match parser.parse(outer_rest) {
                Ok((rest, result)) => {
                    out_vec.push(result);
                    outer_rest = rest
                }
                Err(nom::Err::Error(_)) => return Ok((outer_rest, (out_vec, None))),
                Err(nom::Err::Failure(e)) => return Err(nom::Err::Failure(e)),
                Err(nom::Err::Incomplete(_)) => {
                    panic!("You should not be able to get here; Incomplete is not supported!")
                }
            }
        }
    }
}

fn free_text_until<'a, B, O>(
    mut to_break: B,
) -> impl FnMut(&'a str) -> ParseResult<(Token<'a>, Option<O>)>
where
    B: Parser<&'a str, O, nom::error::Error<&'a str>>,
{
    move |input: &'a str| {
        let mut i: usize = 0;
        if input.len() == 0 {
            return Err(nom::Err::Error(nom::error::Error::from_error_kind(
                input,
                nom::error::ErrorKind::LengthValue,
            )));
        }
        while i < input.len() {
            let sub_string = &input[i..];
            match to_break.parse(sub_string) {
                Ok((rest, result)) => {
                    return Ok((rest, (Token::FreeText(&input[..i]), Some(result))))
                }
                Err(nom::Err::Error(_)) => (),
                Err(nom::Err::Failure(e)) => return Err(nom::Err::Failure(e)),
                Err(nom::Err::Incomplete(_)) => {
                    panic!("You should not be able to get here; Incomplete is not supported!")
                }
            }

            i += 1;
        }
        Ok(("", (Token::FreeText(input), None)))
    }
}

fn parse_once_no_free_text(input: &str) -> ParseResult<Token> {
    alt((if_parse,))(input)
}

fn parse_until<'a, B, O>(mut to_break: B) -> impl FnMut(&'a str) -> ParseResult<'a, Vec<Token<'a>>>
where
    B: Parser<&'a str, O, nom::error::Error<&'a str>>,
{
    move |input: &'a str| {
        if input.is_empty() {
            return Ok((input, Vec::with_capacity(0)));
        }
        let mut out_vec: Vec<Token<'a>> = Vec::with_capacity(4);
        let mut substring = input;
        let mut first_str_index: Option<usize> = None;
        let mut current_str_index: Option<usize> = None;
        loop {
            if substring.is_empty() {
                let actual_current_str_index = current_str_index.expect("Should be set");
                if let Some(actual_first_str_index) = first_str_index {
                    if actual_first_str_index != actual_current_str_index {
                        out_vec.push(Token::FreeText(&input[actual_first_str_index..]))
                    }
                }
                return Ok(("", out_vec));
            }
            match to_break.parse(substring) {
                Ok(_) => {
                    if let Some(actual_first_str_index) = first_str_index {
                        let actual_current_str_index = current_str_index.expect("should be set");
                        out_vec.push(Token::FreeText(
                            &input[actual_first_str_index..actual_current_str_index - 1],
                        ));
                    }
                    return Ok((substring, out_vec));
                }
                Err(nom::Err::Error(_)) => (),
                Err(nom::Err::Failure(e)) => return Err(nom::Err::Failure(e)),
                Err(nom::Err::Incomplete(_)) => panic!("Should not happen"),
            }

            match parse_once_no_free_text(substring) {
                Ok((rest, result)) => {
                    substring = rest;
                    out_vec.push(result);
                    continue;
                }
                Err(nom::Err::Error(_)) => (),
                Err(nom::Err::Failure(e)) => return Err(nom::Err::Failure(e)),
                Err(nom::Err::Incomplete(_)) => panic!("Should not happen"),
            }
            if let Some(actual_first_str_index) = first_str_index {
                let actual_current_str_index =
                    current_str_index.expect("This should be set if first_str_index is set");

                current_str_index = Some(actual_current_str_index + 1);
                substring = &input[actual_current_str_index..];
            } else {
                first_str_index = Some(input.offset(substring));
                current_str_index = first_str_index;
            }
        }
    }
}
mod tests {
    use super::*;

    #[test]
    fn test_if_with_else() {
        use std::env;
        env::set_var("RUST_BACKTRACE", "1");
        let input = format!(
            "{IF_START}(a, b)Here is my string{IF_ELSE}Here is my else string{IF_END}",
            IF_START = IF,
            IF_ELSE = IF_ELSE,
            IF_END = IF_END,
        );
        let expected_first_name_input = format!(
            "a, b)Here is my string{IF_ELSE}Here is my else string{IF_END}",
            IF_ELSE = IF_ELSE,
            IF_END = IF_END,
        );
        let expected_second_name_input = format!(
            "b)Here is my string{IF_ELSE}Here is my else string{IF_END}",
            IF_ELSE = IF_ELSE,
            IF_END = IF_END,
        );
        let expected_output: ParseResult<Token> = Ok((
            "",
            Token::If {
                input: &input,
                include: vec![
                    Name {
                        input: &expected_first_name_input,
                        name: "a",
                    },
                    Name {
                        input: &expected_second_name_input,
                        name: "b",
                    },
                ],
                if_block: vec![Token::FreeText("Here is my string")],
                else_block: Some(vec![Token::FreeText("Here is my else string")]),
            },
        ));
        assert_eq!(if_parse(&input), expected_output);
    }

    #[test]
    fn test_if_no_else() {
        let input = format!(
            "{IF_START}(a, b)Here is my string{IF_END}",
            IF_START = IF,
            IF_END = IF_END
        );
        let expected_first_name_input = format!("a, b)Here is my string{IF_END}", IF_END = IF_END);
        let expected_second_name_input = format!("b)Here is my string{IF_END}", IF_END = IF_END);
        let expected_output: ParseResult<Token> = Ok((
            "",
            Token::If {
                input: &input,
                include: vec![
                    Name {
                        input: &expected_first_name_input,
                        name: "a",
                    },
                    Name {
                        input: &expected_second_name_input,
                        name: "b",
                    },
                ],
                if_block: vec![Token::FreeText("Here is my string")],
                else_block: None,
            },
        ));
        assert_eq!(if_parse(&input), expected_output);
    }

    #[test]
    fn test_free_text_until_accepts_all_if_empty() {
        let input = "This is a cool text that you can accept";
        let expected_output: ParseResult<(Token, Option<&str>)> =
            Ok(("", (Token::FreeText(input), None)));
        assert_eq!(free_text_until(tag("Never occurs"))(input), expected_output)
    }

    #[test]
    fn test_free_text_until_breaks() {
        let input = "This is a cool text that you can accept rest";
        let expected_output: ParseResult<(Token, Option<&str>)> = Ok((
            " rest",
            (
                Token::FreeText("This is a cool text that you can "),
                Some(&"accept"),
            ),
        ));

        assert_eq!(free_text_until(tag("accept"))(input), expected_output);
    }

    #[test]
    fn test_that_many0_accepts_if_no_break_and_no_infinite_loop() {
        let input = "This is a cool text that you can accept";
        let expected_output: ParseResult<(Vec<char>, Option<&str>)> =
            Ok(("", (input.chars().collect(), None)));
        assert_eq!(
            many0_until(nom::character::complete::anychar, tag("Never happends"))(input),
            expected_output
        );
    }

    #[test]
    fn test_that_many0_breaks() {
        let input = "This is a cool text that you can accept rest";
        let expected_output: ParseResult<(Vec<char>, Option<&str>)> = Ok((
            " rest",
            (
                "This is a cool text that you can ".chars().collect(),
                Some(&"accept"),
            ),
        ));
        assert_eq!(
            many0_until(nom::character::complete::anychar, tag("accept"))(input),
            expected_output
        );
    }

    #[test]
    fn test_declaration() {
        //TODO: make test work even if marker symbols are changed
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
        //TODO: make test work even if marker symbols are changed
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
