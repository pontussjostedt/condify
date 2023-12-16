use nom::{IResult, Compare, InputTake, InputLength, CompareResult};
use super::error::{CondifyError, CondifyErrorKind};

pub fn condify_tag<Input>(tag: &'static str) -> impl Fn(Input) -> IResult<Input, Input, CondifyError<Input>>
where
    Input: InputTake + Compare<&'static str>,
{
    move |i: Input| {
        let tag_len = tag.input_len();
        let res: IResult<_, _, CondifyError<Input>> = match i.compare(tag) {
            CompareResult::Ok => Ok(i.take_split(tag_len)),
            _ => {
                let custom_error = CondifyError::from_condify_error_kind(i, CondifyErrorKind::Tag(tag.to_owned()));
                Err(nom::Err::Error(custom_error))
            }
        };
        res
    }
}

#[cfg(test)]
mod tests {
    use nom::error::Error;

    use super::*;

    #[test]
    fn test_tag() {
        let parser = condify_tag("[]");
        let input = "[] rest";
        let expected_output: IResult<&str, &str, CondifyError<&str>> = Ok((" rest", "[]"));
        assert_eq!(parser(input), expected_output);
    }
}