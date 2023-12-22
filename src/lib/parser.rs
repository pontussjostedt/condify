use super::markers::*;
use nom_locate::LocatedSpan;
type Span<'a> = LocatedSpan<&'a str>;

struct AssignmentArm<'a> {
    include: Vec<&'a str>,
    value: &'a str,
}
enum Token<'a> {
    Declare(Span<'a>, Vec<&'a str>),
    Assignment(Span<'a>, AssignmentArm<'a>),
    FreeText(Span<'a>, &'a str),
}
