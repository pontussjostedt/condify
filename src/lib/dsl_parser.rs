use std::ptr::addr_of_mut;

use super::{
    error::*,
    markers::{self, *},
};

use itertools::Itertools;
use nom::{
    branch::alt,
    bytes::complete::*,
    combinator::{cut, not, opt, peek, success},
    error::{self, Error, ParseError},
    multi::{many0, separated_list0},
    sequence::{delimited, preceded, terminated, tuple},
    IResult, InputLength, Offset, Parser,
};
type ParseResult<'a, O, I = &'a str> = IResult<I, O, Error<I>>;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Name<'a> {
    pub input: &'a str,
    pub name: &'a str,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Declaration<'a> {
    pub input: &'a str,
    pub declared: Vec<Name<'a>>,
}

/// Implementation used to protect the sanity of the person writing test
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Assignment<'a> {
    pub input: &'a str,
    pub name: Name<'a>,
    pub include: Vec<Name<'a>>,
    pub value: &'a str,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct If<'a> {
    pub input: &'a str,
    pub include: Vec<Name<'a>>,
    pub if_block: Vec<Token<'a>>,
    pub else_block: Option<Vec<Token<'a>>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ReadValue<'a> {
    pub input: &'a str,
    pub name: Name<'a>,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Token<'a> {
    Declaration(Declaration<'a>),
    Assignment(Assignment<'a>),
    FreeText(&'a str),
    If(If<'a>),
    ReadValue(ReadValue<'a>),
}

impl Token<'_> {
    pub fn short_form(&self) -> String {
        match self {
            Token::Declaration(declaration) => format!(
                "Declaration([{}])",
                declaration
                    .declared
                    .iter()
                    .map(|name| name.name)
                    .intersperse(", ")
                    .collect::<String>()
            ),
            Token::Assignment(assignment) => format!(
                "Assignment(name: {}, for: [{}], value: {})",
                assignment.name.name,
                assignment
                    .include
                    .iter()
                    .map(|name| name.name)
                    .intersperse(", ")
                    .collect::<String>(),
                assignment.value
            ),
            Token::FreeText(text) => format!("FreeText(\"{}\")", text),
            Token::If(ifbody) => format!(
                "If(include: [{}], body: {}, else: {:?})",
                ifbody
                    .include
                    .iter()
                    .map(|name| name.name)
                    .intersperse(", ")
                    .collect::<String>(),
                ifbody
                    .if_block
                    .iter()
                    .map(|token| token.short_form())
                    .collect::<String>(),
                ifbody.else_block.clone().map(|tokens| tokens
                    .iter()
                    .map(|token| token.short_form() + "\n")
                    .collect::<String>())
            ),
            Token::ReadValue(read_value) => format!("ReadValue(name: {})", read_value.name.name),
        }
    }
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
            Token::Declaration(Declaration {
                input: input,
                declared: names,
            }),
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
        Token::Assignment(Assignment {
            input,
            name,
            include,
            value: str_litteral,
        }),
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
        Token::If(If {
            input,
            include: names,
            if_block: body,
            else_block: else_body,
        }),
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
    alt((if_parse, read_value))(input)
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
                if let Some(actual_first_str_index) = first_str_index {
                    let actual_current_str_index = current_str_index.expect("Should be set");
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
                    if let Some(actual_first_str_index) = first_str_index {
                        let actual_current_str_index =
                            current_str_index.expect("should be set parce_once_no_free text");
                        out_vec.push(Token::FreeText(
                            &input[actual_first_str_index..actual_current_str_index - 1],
                        ));
                    }
                    substring = rest;
                    out_vec.push(result);
                    first_str_index = None;
                    current_str_index = None;
                    continue;
                }
                Err(nom::Err::Error(_)) => (),
                Err(nom::Err::Failure(e)) => return Err(nom::Err::Failure(e)),
                Err(nom::Err::Incomplete(_)) => panic!("Should not happen"),
            }
            if let Some(_) = first_str_index {
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

fn read_value(input: &str) -> ParseResult<Token> {
    delimited(tag(READ_VALUE), name1, tag(READ_VALUE))(input)
        .map(|(rest, name)| (rest, Token::ReadValue(ReadValue { input, name })))
}

fn error_parser(input: &str) -> ParseResult<()> {
    return Err(nom::Err::Error(error::Error::from_error_kind(
        input,
        error::ErrorKind::Not,
    )));
}

pub fn parse<'a>(input: &'a str) -> ParseResult<Vec<Token<'a>>> {
    let (rest, declaration_opt) = opt(declaration)(input)?;
    let (rest, assignments) = many0(preceded(whitespace0, assignment))(rest)?;
    let (rest, body) = parse_until(error_parser)(rest)?; //TODO: make this an actual function without a weird bodge
    let mut out = Vec::with_capacity(1 + assignments.len() + body.len());
    //TODO: Fix so it concats in a better way.
    if let Some(declaration) = declaration_opt {
        out.push(declaration)
    };
    for token in assignments {
        out.push(token);
    }
    for token in body {
        out.push(token);
    }
    Ok((rest, out))
}

mod tests {
    use super::*;

    #[derive(Debug, Clone, PartialEq)]
    enum SimpleToken<'a> {
        Declaration(Vec<&'a str>),
        Assignment {
            name: &'a str,
            include: Vec<&'a str>,
            value: &'a str,
        },
        FreeText(&'a str),
        If {
            include: Vec<&'a str>,
            if_block: Vec<SimpleToken<'a>>,
            else_block: Option<Vec<SimpleToken<'a>>>,
        },
        ReadValue(&'a str),
    }

    impl SimpleToken<'_> {
        fn from<'a>(other: &Token<'a>) -> SimpleToken<'a> {
            match other {
                Token::Declaration(Declaration { input: _, declared }) => {
                    SimpleToken::Declaration(declared.iter().map(|name| name.name).collect())
                }
                Token::Assignment(Assignment {
                    input: _,
                    name,
                    include,
                    value,
                }) => SimpleToken::Assignment {
                    name: name.name,
                    include: include.iter().map(|name| name.name).collect(),
                    value,
                },
                Token::FreeText(text) => SimpleToken::FreeText(text),
                Token::If(If {
                    input,
                    include,
                    if_block,
                    else_block,
                }) => SimpleToken::If {
                    include: include.iter().map(|name| name.name).collect(),
                    if_block: if_block.iter().map(SimpleToken::from).collect(),
                    else_block: else_block
                        .clone()
                        .map(|inner| inner.iter().map(SimpleToken::from).collect()),
                },
                Token::ReadValue(ReadValue { input: _, name }) => SimpleToken::ReadValue(name.name),
            }
        }
    }

    #[test]
    fn test_parse() {
        use SimpleToken::*;
        let input = format!(
            "{DECLARATION_DELIMITER_START}DETAIL, NO_DETAIL{DECLARATION_DELIMITER_END}
name1 FOR {ASSIGNMENT_DELIMITER_START}DETAIL, NO_DETAIL{ASSIGNMENT_DELIMITER_END} IS \"name1value\"
name2 FOR {ASSIGNMENT_DELIMITER_START}DETAIL{ASSIGNMENT_DELIMITER_END} IS \"name2value\"
here is some free text with a value {READ_VALUE}name1{READ_VALUE}
        "
        );

        let expected_output: ParseResult<Vec<SimpleToken>> = Ok((
            "",
            vec![
                Declaration(vec!["DETAIL", "NO_DETAIL"]),
                Assignment {
                    name: "name1",
                    include: vec!["DETAIL", "NO_DETAIL"],
                    value: "name1value",
                },
                Assignment {
                    name: "name2",
                    include: vec!["DETAIL"],
                    value: "name2value",
                },
                FreeText("\nhere is some free text with a value "),
                ReadValue("name1"),
                FreeText("\n        "),
            ],
        ));
        assert_eq!(
            parse(&input)
                .map(|(rest, result)| (rest, result.iter().map(SimpleToken::from).collect_vec())),
            expected_output
        );
    }

    #[test]
    fn test_parse_until_can_parse_two_consectuive_tokens() {
        use SimpleToken::*;
        let input = format!("here is a string{READ_VALUE}name1{READ_VALUE}{READ_VALUE}name2{READ_VALUE}{READ_VALUE}name3{READ_VALUE}");
        let expect_output: ParseResult<Vec<SimpleToken>> = Ok((
            "",
            vec![
                FreeText("here is a string"),
                ReadValue("name1"),
                ReadValue("name2"),
                ReadValue("name3"),
            ],
        ));
        assert_eq!(
            parse_until(error_parser)(&input)
                .map(|(rest, tokens)| (rest, tokens.iter().map(SimpleToken::from).collect())),
            expect_output
        );
    }

    #[test]
    fn test_parse_until() {
        use SimpleToken::*;
        let input = "free text <*>name1<*> more text";
        let expected_output: ParseResult<Vec<SimpleToken>> = Ok((
            "",
            vec![
                FreeText("free text "),
                ReadValue("name1"),
                FreeText(" more text"),
            ],
        ));

        assert_eq!(
            parse_until(error_parser)(input)
                .map(|(rest, result)| (rest, result.iter().map(SimpleToken::from).collect_vec())),
            expected_output
        );
    }

    #[test]
    fn test_read_value() {
        let input = format! {"{marker}Name{marker} rest", marker = READ_VALUE};
        let expected_input = format! {"Name{marker} rest", marker = READ_VALUE};
        let expected_output: ParseResult<Token> = Ok((
            " rest",
            Token::ReadValue(ReadValue {
                input: &input,
                name: Name {
                    input: &expected_input,
                    name: "Name",
                },
            }),
        ));
        assert_eq!(read_value(&input), expected_output);
    }

    #[test]
    fn test_if_nested() {
        let input = format!(
            "{IF_START}(a, b)Here is my string{IF_START}(a,c)Inner String{IF_END}{IF_END}",
            IF_START = IF,
            IF_END = IF_END
        );

        let expected_input_name1 = format!(
            "a, b)Here is my string{IF_START}(a,c)Inner String{IF_END}{IF_END}",
            IF_START = IF,
            IF_END = IF_END
        );

        let expected_input_name2 = format!(
            "b)Here is my string{IF_START}(a,c)Inner String{IF_END}{IF_END}",
            IF_START = IF,
            IF_END = IF_END
        );

        let expected_input_name3 = format!("a,c)Inner String{IF_END}{IF_END}", IF_END = IF_END);
        let expected_input_name4 = format!("c)Inner String{IF_END}{IF_END}", IF_END = IF_END);
        let expected_inpout_inner_if = format!(
            "{IF_START}(a,c)Inner String{IF_END}{IF_END}",
            IF_START = IF,
            IF_END = IF_END
        );

        let expected_inner_if: Token = Token::If(If {
            input: &expected_inpout_inner_if,
            include: vec![
                Name {
                    input: &expected_input_name3,
                    name: "a",
                },
                Name {
                    input: &expected_input_name4,
                    name: "c",
                },
            ],
            if_block: vec![Token::FreeText("Inner String")],
            else_block: None,
        });
        let expected_output: ParseResult<Token> = Ok((
            "",
            Token::If(If {
                input: &input,
                include: vec![
                    Name {
                        input: &expected_input_name1,
                        name: "a",
                    },
                    Name {
                        input: &expected_input_name2,
                        name: "b",
                    },
                ],
                if_block: vec![Token::FreeText("Here is my string"), expected_inner_if],
                else_block: None,
            }),
        ));
    }

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
            Token::If(If {
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
            }),
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
            Token::If(If {
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
            }),
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
            Token::Declaration(Declaration {
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
            }),
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
        let input = "number FOR (ALL_THE_DETAIL, NO_DETAIL) IS \"MyString\" rest";
        let expected_output: ParseResult<Token> = Ok((
            " rest",
            Token::Assignment(Assignment {
                input: "number FOR (ALL_THE_DETAIL, NO_DETAIL) IS \"MyString\" rest",
                name: Name {
                    input,
                    name: "number",
                },
                include: vec![
                    Name {
                        input: "ALL_THE_DETAIL, NO_DETAIL) IS \"MyString\" rest",
                        name: "ALL_THE_DETAIL",
                    },
                    Name {
                        input: "NO_DETAIL) IS \"MyString\" rest",
                        name: "NO_DETAIL",
                    },
                ],
                value: "MyString",
            }),
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
