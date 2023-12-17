use std::marker;

use nom::branch::alt;
use nom::bytes::complete::*;
use nom::character::complete::{anychar, crlf, newline, space0};
use nom::character;
use nom::combinator::*;
use nom::error::{context, VerboseError};
use nom::multi::{many0, separated_list0};
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::IResult;

use super::error::CondifyError;
use super::parse_utils::condify_tag;
//type ParserType<'a> = FnMut(&str)

static IF_MARKER: &'static str = "!IF";
static ELSE_MARKER: &'static str = "!ELSE";
static ENDIF_MARKER: &'static str = "!ENDIF";
static START_DECLARATION_MARKER: &'static str = "<";
static END_DEDCLARATION_MARKER: &'static str = ">";
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
#[derive(PartialEq, Debug)]
struct DeclarationState<'a> {
    valid_names: Vec<&'a str>
}

impl DeclarationState<'_> {
    fn is_valid_name(&self, name: &str) -> bool {
        self.valid_names.contains(&name)
    }
}

#[derive(PartialEq, Debug)]
struct AssignmentState<'a> {
    declaration_state: &'a DeclarationState<'a>,
    assigned: Vec<&'a str>,
    default_declared: bool
}
impl AssignmentState<'_> {
    fn with_default(&self) -> AssignmentState {
        assert!(!self.default_declared, "Attempting to declare default twice");
        AssignmentState {
            default_declared: true,
            declaration_state: self.declaration_state,
            assigned: self.assigned.clone(),
        }
    }

    fn is_valid_name(&self, name: &str) -> bool {
        self.declaration_state.is_valid_name(&name)
    }
}


type ParseResult<'a, O, I = &'a str> = IResult<I, O, CondifyError<&'a str>>;

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

fn str_literal<'a>(input: &'a str) -> ParseResult<&'a str> {
    //Todo allow escape char
    let (rest, value) = delimited(character::complete::char('"'), take_while(|c| c != '"'), character::complete::char('"'))(input)?;
    Ok((rest, value))
}

fn list1<'a> (start_delim: &'static str, end_delim: &'static str, sep: char) -> impl Fn(&'a str) -> ParseResult<Vec<&'a str>> {
    move |input: &'a str| {
        let (rest, _) = condify_tag(start_delim)(input)?;
        let (rest, list) = separated_list0(nom::character::complete::char(sep), preceded(opt(whitespace1), name1))(rest)?;
        let (rest, _) = context("Looking for end of list", cut(condify_tag(end_delim)))(rest)?;
        Ok((rest, list))
    }
}

fn parse_declaration<'a>(input: &'a str) -> ParseResult<TokenType> {
    let (rest, list) = list1(START_DECLARATION_MARKER, END_DEDCLARATION_MARKER, ',')(input)?;
    Ok((rest, TokenType::Declaration(list)))
}

fn parse_default_arm<'a>(assignment_state: &'a AssignmentState<'a>, input: &'a str) -> ParseResult<'a, (AssignmentState<'a>, AssignmentArm<'a>)> {
    let (rest, value) = preceded(tuple((tag(DEFAULT_NAME), opt(whitespace1), nom::character::complete::char('='), opt(whitespace1))), str_literal)(input)?;
    if assignment_state.default_declared {
        Err(nom::Err::Error(CondifyError::from_condify_error_kind(input, super::error::CondifyErrorKind::AlreadyAssigned("DEFAULT".to_owned()))))
    } else {
        Ok((rest, (assignment_state.with_default(), AssignmentArm::Default(value))))
    }
}

#[cfg(test)]
mod tests {
    use crate::lib::parsing::error::{CondifyErrorKind, convert_condify_error};

    use super::*;

    #[test]
    fn test_parse_default_arm_error() {
        let input = "DEFAULT = \"MyString\" rest";
        let declaration_state = DeclarationState {
            valid_names: vec![]
        };
        let assignment_state = AssignmentState {
            declaration_state: &declaration_state,
            assigned: vec![],
            default_declared: true
        };

        let expected_error = CondifyError::from_condify_error_kind(input, CondifyErrorKind::AlreadyAssigned("DEFAULT".to_owned()));

        match parse_default_arm(&assignment_state, input) {
            Ok((rest, (new_state, result))) => {
                assert!(false, "Should fail");
            },
            Err(err) => {
                let condify_error: CondifyError<_> = match err {
                    nom::Err::Error(e) | nom::Err::Failure(e) => e,
                _ => CondifyError { errors: vec![] },
                };
                assert_eq!(condify_error, expected_error);
                //let res = convert_condify_error(input, condify_error);
                //println!("{}", res);
            }
        }
    }

    #[test]
    fn test_parse_default_arm() {
        let input = "DEFAULT = \"MyString\" rest";
        let declaration_state = DeclarationState {valid_names: vec![]};
        let assignment_state = AssignmentState {
            declaration_state: &declaration_state,
            assigned: vec![],
            default_declared: false
        };
        let expected_output: ParseResult<(AssignmentState, AssignmentArm)> = Ok((" rest", (assignment_state.with_default(), AssignmentArm::Default("MyString"))));
        assert_eq!(parse_default_arm(&assignment_state, input), expected_output);
    }

    #[test]
    fn test_str_literal() {
        let input = "\"MyString\" rest";
        let expected_output: ParseResult<&str> = Ok((" rest", "MyString"));
        assert_eq!(str_literal(input), expected_output);
    }

    #[test]
    fn test_parse_declaration() {
        let input = format!("{start_delim}NO_DETAIL, ALL_THE_DETAIL{end_delim} rest", start_delim= START_DECLARATION_MARKER, end_delim=END_DEDCLARATION_MARKER);
        let expected_output: ParseResult<TokenType> = Ok((" rest", TokenType::Declaration(vec!["NO_DETAIL", "ALL_THE_DETAIL"])));
        assert_eq!(parse_declaration(input.as_str()), expected_output);
    }

    #[test]
    fn test_list1_fail_on_no_delimiter() {
        let input = "<AoA,DETAILED_NAME,NO_DETAIL> rest";
        let expected_output: ParseResult<Vec<&str>> = Err(nom::Err::Error(CondifyError { errors: vec![(input, CondifyErrorKind::Tag("[".to_owned()))] }));
        assert_eq!(list1("[", "]", ',')(input), expected_output);
    }

    #[test]
    fn test_list1() {
        let input = "[AoA,DETAILED_NAME,NO_DETAIL] rest";
        let expected_output: ParseResult<Vec<&str>> = Ok((" rest", vec!["AoA","DETAILED_NAME","NO_DETAIL"]));
        assert_eq!(list1("[", "]", ',')(input), expected_output);
        let input = "[AoA, DETAILED_NAME, NO_DETAIL] rest";
        assert_eq!(list1("[", "]", ',')(input), expected_output);
    }

    #[test]
    fn test_name1() {
        let input = "THIS_IS_A_name6] Not Included";
        let expected_output: ParseResult<&str> = Ok(("] Not Included", "THIS_IS_A_name6"));
        assert_eq!(name1(input), expected_output)
    }

    #[test]
    fn test_whitespace1() {
        let input = " \n s";
        let expected_output: ParseResult<&str> = Ok(("s", " \n "));
        assert_eq!(whitespace1(input), expected_output);
    }
}
