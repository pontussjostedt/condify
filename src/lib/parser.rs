use std::marker;

use nom::branch::alt;
use nom::bytes::complete::*;
use nom::character::complete::{anychar, crlf, newline, space0};
use nom::combinator::*;
use nom::error::{context, VerboseError};
use nom::multi::{many0, separated_list0};
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::IResult;
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

fn list_parser<'a>(input: &'a str) -> ParseResult<'a, Vec<&'a str>> {
    //TODO: make general for any parser, will allow to also add verify
    terminated(
        separated_list0(tag(","), preceded(opt(space0), name_parser)),
        opt(tag(",")),
    )(input)
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
    preceded(
        tag(ASSINGMENT_MARKER),
        delimited(marker("{"), inner, marker("}")),
    )(input)
    .map(|(next_input, res)| (next_input, TokenType::AssigmentBlock(res)))
    //preceded(tag(ASSINGMENT_MARKER), delimited(marker("{"), , marker("}")))
}

fn parse_char<'a>(input: &'a str) -> ParseResult<'a, TokenType> {
    nom::character::complete::anychar(input).map(|(rest, ch)| (rest, TokenType::FreeChar(ch)))
}

fn parse_ast_once<'a>(input: &'a str) -> ParseResult<'a, TokenType> {
    let parsers = (if_parser, read_value_parse, parse_char);
    alt(parsers)(input)
}

fn if_parser<'a>(input: &'a str) -> ParseResult<'a, TokenType> {
    let branch_parse = alt((marker(ELSE_MARKER), marker(ENDIF_MARKER)));

    let (rest, _) = marker(IF_MARKER)(input)?;
    let (rest, include) = delimited(tag("("), list_parser, cut(tag(")")))(rest)?;
    let (rest, body) = many0(preceded(peek(not(branch_parse)), parse_ast_once))(rest)?; // EAT UNTIL ELSE, ENDIF MARKER OR EOF
    let (rest, else_body) = opt(preceded(
        marker(ELSE_MARKER),
        many0(preceded(peek(not(marker(ENDIF_MARKER))), parse_ast_once)), //EAT UNTIL ENDIF OR EOF
    ))(rest)?;
    let (rest, _) = context("Expected: end of if", cut(marker(ENDIF_MARKER)))(rest)?; //syntax error

    let output = TokenType::IfBlock {
        include,
        body,
        else_body,
    };

    Ok((rest, output))
}

fn read_value_parse<'a>(input: &'a str) -> ParseResult<'a, TokenType> {
    preceded(tag(READ_VALUE_MARKER), name_parser)(input).map(|(rest, name)| {
        (rest, TokenType::ReadValue(name))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_value() {
        let name = "COOL_name";
        let input = format!("<*>{} ", name);
        let expected_output = TokenType::ReadValue(name);
        assert_eq!(read_value_parse(&input), Ok((" ", expected_output)));
    }

    #[test]
    fn test_else_if() {
        let inner_free_text = "bob bob bob";
        let else_inner_free_text = "Holla!";
        let input = format!(
            " {}(a,b){} {}{} {}",
            IF_MARKER, inner_free_text, ELSE_MARKER, else_inner_free_text, ENDIF_MARKER
        );
        let expected_output = TokenType::IfBlock {
            include: vec!["a", "b"],
            body: inner_free_text.chars().map(TokenType::FreeChar).collect(),
            else_body: Some(
                else_inner_free_text
                    .chars()
                    .map(TokenType::FreeChar)
                    .collect(),
            ),
        };

        assert_eq!(if_parser(&input), Ok(("", expected_output)));
    }

    #[test]
    fn test_nested_if() {
        let inner_free_text = "bob bob bob";
        let input = format!(
            " {}(a,b) {}(a){} {} {}",
            IF_MARKER, IF_MARKER, inner_free_text, ENDIF_MARKER, ENDIF_MARKER
        );
        let inner_if = TokenType::IfBlock {
            include: vec!["a"],
            body: inner_free_text.chars().map(TokenType::FreeChar).collect(),
            else_body: None,
        };

        let expected_output = TokenType::IfBlock {
            include: vec!["a", "b"],
            body: vec![inner_if],
            else_body: None,
        };

        assert_eq!(if_parser(&input), Ok(("", expected_output)));
    }

    #[test]
    fn test_if_parser_free_text() {
        let inner_free_text = "bob bob bob";
        let input = format!(" {}(a,b){}{}", IF_MARKER, inner_free_text, ENDIF_MARKER);
        let expected_output = TokenType::IfBlock {
            include: vec!["a", "b"],
            body: inner_free_text.chars().map(TokenType::FreeChar).collect(),
            else_body: None,
        };

        assert_eq!(if_parser(&input), Ok(("", expected_output)));
    }

    #[test]
    fn test_list_parser() {
        let input = "Name1, Name2";
        let output_vec = vec!["Name1", "Name2"];
        assert_eq!(list_parser(input), Ok(("", output_vec.clone())));
        let input = "Name1,Name2";
        assert_eq!(
            list_parser(input),
            Ok(("", output_vec.clone())),
            "No space between"
        );
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
            ASSINGMENT_MARKER, DEFAULT_NAME
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
