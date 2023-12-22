use super::{error::*, markers::*};

use nom::{bytes::complete::*, error::Error, multi::separated_list0, sequence::delimited, IResult};
type ParseResult<'a, O, I = &'a str> = IResult<I, O, Error<I>>;

struct AssignmentArm<'a> {
    input: &'a str,
    include: Vec<&'a str>,
    value: &'a str,
}
enum Token<'a> {
    Declare {
        input: &'a str,
        declared: Vec<&'a str>,
    },
    Assignment {
        input: &'a str,
        name: &'a str,
        arms: Vec<AssignmentArm<'a>>,
    },
    FreeText(&'a str),
}

fn whitespace0(input: &str) -> ParseResult<&str> {
    take_while(|c: char| c.is_whitespace())(input)
}

fn name1(input: &str) -> ParseResult<&str> {
    take_while(|c: char| c.is_alphanumeric() || c == '_')(input)
}

mod tests {
    use super::*;

    #[test]
    fn test_name1() {
        let input = "c00l_NAME\" rest";
        let expected_output: ParseResult<&str> = Ok(("\" rest", "c00l_NAME"));
        assert_eq!(name1(input), expected_output)
    }

    #[test]
    fn test_whitespace0() {
        let input = " \n\r rest";
        let expected_output: ParseResult<&str> = Ok(("rest", " \n\r "));
        assert_eq!(whitespace0(input), expected_output);
    }
}
