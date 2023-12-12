use std::marker;

use nom::branch::alt;
use nom::bytes::complete::*;
use nom::character::complete::{crlf, newline, space0};
use nom::combinator::*;
use nom::error::{context, Error, ErrorKind, ParseError, VerboseError, VerboseErrorKind};
use nom::multi::{many0, separated_list0};
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::{Compare, Err, IResult, InputLength, InputTake, Parser};
//type ParserType<'a> = FnMut(&str)

static IF_MARKER: &'static str = "!IF";
static ELSE_MARKER: &'static str = "!ELSE";
static ENDIF_MARKER: &'static str = "!ENDIF";
static START_DECLARE_MARKER: &'static str = "<";
static END_DEDCLARE_MARKER: &'static str = ">";
static ASSINGMENT_MARKER: &'static str = "ASSIGNMENT";
static DEFAULT_NAME: &'static str = "DEFAULT";

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
    FreeChar(&'a char),
    FreeText(&'a char),
    AssigmentBlock(Vec<AssignmentArm<'a>>),
    IfBlock {
        include: Vec<&'a str>,
        body: Vec<TokenType<'a>>,
    },
    Else(Vec<TokenType<'a>>)
}

type ParseResult<'a, T: 'a> = IResult<&'a str, T, VerboseError<&'a str>>;

fn valid_front<'a>(input: &'a str) -> ParseResult<'a, &str> {
    take_while(|c: char| c == ' ' || c == '\n')(input) // TODO add other space cases
}

fn marker<'a>(marker: &'a str) -> impl Fn(&'a str) -> ParseResult<'a, &str> {
    move |i: &str| {
        preceded(
            valid_front,
            tag::<&'a str, &'a str, VerboseError<&str>>(marker),
        )(i)
    }
}

fn name_parser<'a>(input: &'a str) -> ParseResult<'a, &'a str> {
    take_while(|c: char| c.is_alphanumeric() || c == '_')(input)
}

fn list_parser<'a>(input: &'a str) -> ParseResult<'a, Vec<&'a str>> {  //TODO: make general for any parser, will allow to also add verify
    terminated(separated_list0(tag(","), preceded(opt(space0), name_parser)), opt(tag(",")))(input)
}

fn string_parser<'a>(input: &'a str) -> ParseResult<'a, &str> {
    delimited(
        tag("\""),
        take_while(|c: char| c != '"'),
        context("Cant find(\")", tag("\"")),
    )(input)
}

fn declaration_parser<'a>(input: &'a str) -> ParseResult<'a, TokenType> {
    let parser = terminated(
        separated_list0(
            tag(","),
            preceded(
                space0,
                verify(
                    take_while(|c: char| c.is_alphanumeric() || c == '_'),
                    |res: &str| res != DEFAULT_NAME,
                ),
            ),
        ),
        opt(tag(",")),
    );
    delimited(tag(START_DECLARE_MARKER), parser, tag(END_DEDCLARE_MARKER))(input)
        .map(|(next_input, res)| (next_input, TokenType::Declaration(res)))
}

// mby decompose into more functions so they can all be tested separatley?
fn assignment_specific_arm_parser<'a>(input: &'a str) -> ParseResult<'a, AssignmentArm> {
    let include = delimited(
        tag("("),
        terminated(
            separated_list0(
                tag(","),
                preceded(
                    opt(space0),
                    take_while(|c: char| c.is_alphanumeric() || c == '_'),
                ),
            ),
            opt(tag(",")),
        ),
        tag(")"),
    );
    tuple((
        terminated(include, marker("=")),
        preceded(opt(space0), string_parser),
    ))(input)
    .map(|(next_input, (include, value))| {
        (
            next_input,
            AssignmentArm::SpecificArm {
                include: include,
                value: value,
            },
        )
    })
}

fn assignment_default_arm_parser<'a>(input: &'a str) -> ParseResult<'a, AssignmentArm> {
    preceded(
        tuple((tag(DEFAULT_NAME), opt(space0), tag("="), opt(space0))),
        string_parser,
    )(input)
    .map(|(next_input, res)| (next_input, AssignmentArm::Default(res)))
}
fn assignment_parser<'a>(input: &'a str) -> ParseResult<'a, TokenType> {
    let inner = many0(alt((
        preceded(opt(valid_front), assignment_default_arm_parser),
        preceded(opt(valid_front), assignment_specific_arm_parser),
    )));
    preceded(tag(ASSINGMENT_MARKER), delimited(marker("{"), inner, marker("}")))(input)
        .map(|(next_input, res)| (next_input, TokenType::AssigmentBlock(res)))
    //preceded(tag(ASSINGMENT_MARKER), delimited(marker("{"), , marker("}")))
}

fn parse_ast<'a>(input: &'a str) -> ParseResult<'a, Vec<TokenType>> {
    todo!()
}

fn if_parser<'a>(input: &'a str) -> ParseResult<'a, TokenType> {
    let (rest, include) = delimited(tag("("), list_parser, tag(")"))(input)?;
    //let after

    //delimited(marker(IF_MARKER), todo!(), cut(marker(ENDIF_MARKER)))(input)
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_list_parser(){
        let input = "Name1, Name2";
        let output_vec = vec!["Name1", "Name2"];
        assert_eq!(list_parser(input), Ok(("", output_vec.clone())));
        let input = "Name1,Name2";
        assert_eq!(list_parser(input), Ok(("", output_vec.clone())), "No space between");
        // TODO!
        //let input = "Name1, Name2,";
        //assert_eq!(list_parser(input), Ok(("", output_vec.clone())), "Trailing comma");
    }

    #[test]
    fn test_assignment_parser() {
        let input = format!(
            "{} {{
                {} = \"This is a string\"
                (NO_DETAIL, ALL_THE_DETAIL) = \"This is a string\"
            }}",
            ASSINGMENT_MARKER,
            DEFAULT_NAME
        );

        println!("{}", input);
        let expected_output = TokenType::AssigmentBlock(vec![
            AssignmentArm::Default("This is a string"),
            AssignmentArm::SpecificArm {
                include: vec!["NO_DETAIL", "ALL_THE_DETAIL"],
                value: "This is a string",
            },
        ]);

        assert_eq!(assignment_parser(input.as_str()), Ok(("", expected_output)));
    }

    #[test]
    fn test_default_arm_parser() {
        let input = format!("{} = \"This is a string\"", DEFAULT_NAME);
        let expected_output = AssignmentArm::Default("This is a string");
        assert_eq!(
            assignment_default_arm_parser(input.as_str()),
            Ok(("", expected_output))
        );
    }

    #[test]
    fn test_specific_arm_parser() {
        let input = "(NO_DETAIL, ALL_THE_DETAIL) = \"This is a string\"";
        let expected_output = AssignmentArm::SpecificArm {
            include: vec!["NO_DETAIL", "ALL_THE_DETAIL"],
            value: "This is a string",
        };
        assert_eq!(
            assignment_specific_arm_parser(input),
            Ok(("", expected_output))
        );
    }

    //TODO fix so test updates with MARKER
    #[test]
    fn test_arm_declaration_parser() {
        let input = "<NO_DETAIL, ALL_THE_DETAIL>";
        let expected_answer = TokenType::Declaration(vec!["NO_DETAIL", "ALL_THE_DETAIL"]);
        assert_eq!(declaration_parser(input), Ok(("", expected_answer)));
    }

    #[test]
    fn test_marker() {
        assert_eq!(
            tag::<&str, &str, VerboseError<&str>>("\n")("\n"),
            Ok(("", "\n"))
        );
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
