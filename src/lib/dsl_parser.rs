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

/*
fn list1<'a>(
    start_delim: &'static str,
    end_delim: &'static str,
    sep: char,
) -> impl Fn(Span<'a>) -> ParseResult<Vec<Span<'a>>> {
    move |input: &'a Span<'a>| {
        delimited(tag(start_delim), separated_list0(tuple((opt())), f), tag(end_delim))
    }
}
*/
