use nom::{IResult, Compare, InputTake, InputLength, CompareResult};

use super::error::{CondifyError, CondifyErrorKind};

pub fn tag<Input>(tag: &'static str) -> impl Fn(Input) -> IResult<Input, Input, CondifyError<Input>>
where
    Input: InputTake + Compare<&'static str>,
{
    move |i: Input| {
        let tag_len = tag.input_len();
        let t = tag.clone();
        let res: IResult<_, _, CondifyError<Input>> = match i.compare(t) {
            CompareResult::Ok => Ok(i.take_split(tag_len)),
            _ => {
                let custom_error = CondifyError::from_condify_error_kind(i, CondifyErrorKind::Tag(tag.to_owned()));
                Err(nom::Err::Error(custom_error))
            }
        };
        res
    }
}