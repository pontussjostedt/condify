use std::fmt::{self, Write};

use nom::{
    error::{ContextError, ErrorKind, FromExternalError, ParseError, VerboseErrorKind},
    Offset,
};

// SEE https://github.com/rust-bakery/nom/blob/main/src/error.rs

#[derive(Debug, PartialEq)]
pub struct CondifyError<I> {
    pub errors: Vec<(I, CondifyErrorKind)>,
}
#[derive(Debug, PartialEq)]
pub enum CondifyErrorKind {
    Context(&'static str),
    Tag(String),
    NotDeclared(String),
    AlreadyAssigned(String),
    Char(char),
    Nom(ErrorKind),
}

impl<I> CondifyError<I> {
    pub fn from_condify_error_kind(input: I, kind: CondifyErrorKind) -> Self {
        CondifyError::<I> {
            errors: vec![(input, kind)],
        }
    }

    pub fn new() -> Self {
        CondifyError::<I> { errors: vec![] }
    }
}

impl<I> ParseError<I> for CondifyError<I> {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        CondifyError {
            errors: vec![(input, CondifyErrorKind::Nom(kind))],
        }
    }

    fn append(input: I, kind: ErrorKind, mut other: Self) -> Self {
        other.errors.push((input, CondifyErrorKind::Nom(kind)));
        other
    }

    fn from_char(input: I, c: char) -> Self {
        CondifyError {
            errors: vec![(input, CondifyErrorKind::Char(c))],
        }
    }
}

impl<I> ContextError<I> for CondifyError<I> {
    fn add_context(input: I, ctx: &'static str, mut other: Self) -> Self {
        other.errors.push((input, CondifyErrorKind::Context(ctx)));
        other
    }
}

impl<I, E> FromExternalError<I, E> for CondifyError<I> {
    /// Create a new error from an input position and an external error
    fn from_external_error(input: I, kind: ErrorKind, _e: E) -> Self {
        Self::from_error_kind(input, kind)
    }
}

impl<I: fmt::Display> fmt::Display for CondifyError<I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Parse error:")?;
        for (input, error) in &self.errors {
            match error {
                CondifyErrorKind::Nom(e) => writeln!(f, "{:?} at: {}", e, input)?,
                CondifyErrorKind::Char(c) => writeln!(f, "expected '{}' at: {}", c, input)?,
                CondifyErrorKind::Context(s) => writeln!(f, "in section '{}', at: {}", s, input)?,
                CondifyErrorKind::Tag(t) => writeln!(f, "expected '{}', at: {}", t, input)?,
                CondifyErrorKind::NotDeclared(name) => {
                    writeln!(f, "{} not declared, at {}", name, input)?
                }
                CondifyErrorKind::AlreadyAssigned(name) => {
                    writeln!(f, "{} already assigned, at {}", name, input)?
                }
            }
        }

        Ok(())
    }
}

impl<I: fmt::Debug + fmt::Display> std::error::Error for CondifyError<I> {}

pub fn convert_condify_error<I: core::ops::Deref<Target = str>>(
    input: I,
    e: CondifyError<I>,
) -> String {
    let mut result = String::new();

    for (i, (substring, kind)) in e.errors.iter().enumerate() {
        let offset = input.offset(substring);

        if input.is_empty() {
            match kind {
                CondifyErrorKind::Char(c) => {
                    write!(&mut result, "{}: expected '{}', got empty input\n\n", i, c)
                }
                CondifyErrorKind::Tag(tag) => {
                    write!(
                        &mut result,
                        "{}: expected '{}', got empty input\n\n",
                        i, tag
                    )
                }
                CondifyErrorKind::Context(s) => {
                    write!(&mut result, "{}: in {}, got empty input\n\n", i, s)
                }
                CondifyErrorKind::Nom(e) => {
                    write!(&mut result, "{}: in {:?}, got empty input\n\n", i, e)
                }
                CondifyErrorKind::NotDeclared(name) => {
                    panic!(
                        "I don't think you should be able to get here, please notify me if I do"
                    );
                    write!(&mut result, "{}: in {}, got empty input\n\n", i, name)
                }
                CondifyErrorKind::AlreadyAssigned(name) => {
                    panic!(
                        "I don't think you should be able to get here, please notify me if I do"
                    );
                }
            }
        } else {
            let prefix = &input.as_bytes()[..offset];

            // Count the number of newlines in the first `offset` bytes of input
            let line_number = prefix.iter().filter(|&&b| b == b'\n').count() + 1;

            // Find the line that includes the subslice:
            // Find the *last* newline before the substring starts
            let line_begin = prefix
                .iter()
                .rev()
                .position(|&b| b == b'\n')
                .map(|pos| offset - pos)
                .unwrap_or(0);

            // Find the full line after that newline
            let line = input[line_begin..]
                .lines()
                .next()
                .unwrap_or(&input[line_begin..])
                .trim_end();

            // The (1-indexed) column number is the offset of our substring into that line
            let column_number = line.offset(substring) + 1;

            match kind {
                CondifyErrorKind::Char(c) => {
                    if let Some(actual) = substring.chars().next() {
                        write!(
                            &mut result,
                            "{i}: at line {line_number}:\n\
                 {line}\n\
                 {caret:>column$}\n\
                 expected '{expected}', found {actual}\n\n",
                            i = i,
                            line_number = line_number,
                            line = line,
                            caret = '^',
                            column = column_number,
                            expected = c,
                            actual = actual,
                        )
                    } else {
                        write!(
                            &mut result,
                            "{i}: at line {line_number}:\n\
                 {line}\n\
                 {caret:>column$}\n\
                 expected '{expected}', got end of input\n\n",
                            i = i,
                            line_number = line_number,
                            line = line,
                            caret = '^',
                            column = column_number,
                            expected = c,
                        )
                    }
                }
                CondifyErrorKind::Tag(tag) => {
                    if !substring.is_empty() {
                        write!(
                            &mut result,
                            "{i}: at line {line_number}\n\
                            {line}\n\
                            {carets:>column$}\n\
                            expected '{expected}'\n\n",
                            i = i,
                            line_number = line_number,
                            line = line,
                            carets = '^',
                            column = column_number,
                            expected = tag,
                        )
                    } else {
                        write!(
                            &mut result,
                            "{i}: at line {line_number}\n\
                            {line}\n\
                            {carets:>column$}\n\
                            expected '{expected} got end of input'\n\n",
                            i = i,
                            line_number = line_number,
                            line = line,
                            carets = '^',
                            column = column_number,
                            expected = tag,
                        )
                    }
                }
                CondifyErrorKind::NotDeclared(name) => {
                    writeln!(
                        &mut result,
                        "{i}: at line {line_number}\n\
                    {line}\n\
                    {carets:>column$}\n\
                    '{name}' is not declared\n\n",
                        i = i,
                        line_number = line_number,
                        line = line,
                        carets = "^".repeat(name.len()),
                        column = column_number,
                        name = name,
                    )
                }
                CondifyErrorKind::AlreadyAssigned(name) => {
                    writeln!(
                        &mut result,
                        "{i}: at line {line_number}\n\
                    {line}\n\
                    {carets:>column$}\n\
                    {name} already assigned before\n\n",
                        i = i,
                        line_number = line_number,
                        line = line,
                        carets = "^".repeat(name.len()),
                        column = column_number,
                        name = name,
                    )
                }
                CondifyErrorKind::Context(s) => write!(
                    &mut result,
                    "{i}: at line {line_number}, in {context}:\n\
               {line}\n\
               {caret:>column$}\n\n",
                    i = i,
                    line_number = line_number,
                    context = s,
                    line = line,
                    caret = '^',
                    column = column_number,
                ),
                CondifyErrorKind::Nom(e) => write!(
                    &mut result,
                    "{i}: at line {line_number}, in {nom_err:?}:\n\
               {line}\n\
               {caret:>column$}\n\n",
                    i = i,
                    line_number = line_number,
                    nom_err = e,
                    line = line,
                    caret = '^',
                    column = column_number,
                ),
            }
        }
        // Because `write!` to a `String` is infallible, this `unwrap` is fine.
        .unwrap();
    }

    result
}
