use nom::error::{ParseError, ErrorKind};

#[derive(Debug)]
pub struct CondifyError<I> {
    pub kind: CondifyErrorKind<I>,
    backtrace: Vec<CondifyErrorKind<I>>,
}

#[derive(Debug)]
pub enum CondifyErrorKind<I> {
    Nom(I, nom::error::ErrorKind),
}

impl<I> ParseError<I> for CondifyError<I> {
    fn from_error_kind(input: I, kind: nom::error::ErrorKind) -> Self {
        CondifyError {
            kind: CondifyErrorKind::Nom(input, kind),
            backtrace: Vec::new(),
        }
    }

    fn append(input: I, kind: nom::error::ErrorKind, mut other: Self) -> Self {
        other.backtrace.push(CondifyErrorKind::Nom(input, kind));
        other
    }
}
