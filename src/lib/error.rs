use nom::Offset;

use super::build::BuildError;

#[derive(Debug, PartialEq, Clone)]
struct InputInfo<'a> {
    line: &'a str,
    line_number: usize,
    column: usize,
}

/// * `input` the full string
/// * `substring` a substring of input
fn get_input_line<'a>(input: &'a str, substring: &'a str) -> InputInfo<'a> {
    // https://github.com/rust-bakery/nom/blob/main/src/error.rs
    assert!(!input.is_empty());

    let offset = input.offset(substring);

    let prefix = &input.as_bytes()[..offset];
    let line_number = prefix.iter().filter(|&&b| b == b'\n').count();
    let line_begin = prefix
        .iter()
        .rev()
        .position(|&b| b == b'\n')
        .map(|pos| offset - pos)
        .unwrap_or(0);

    let line = input[line_begin..]
        .lines()
        .next()
        .unwrap_or(&input[line_begin..])
        .trim_end();

    let column_number = line.offset(substring);

    InputInfo::<'a> {
        line,
        line_number,
        column: column_number,
    }
}

pub fn convert_build_error<'a>(build_error: BuildError<'a>, original_string: &'a str) -> String {
    match build_error {
        BuildError::AlreadyAssigned(assignment) => todo!(),
        BuildError::AlreadyDeclared(_) => todo!(),
        BuildError::NotDeclaredOnAssingment(_) => todo!(),
        BuildError::NotDeclaredOnReadValue(_) => todo!(),
        BuildError::NotAssignedOnReadValue(_) => todo!(),
        BuildError::NoDefaultValueOnReadValue(_) => todo!(),
        BuildError::NotInScopeOnIf(_) => todo!(),
        BuildError::DeclareDefault(_) => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::lib::error::{get_input_line, InputInfo};

    #[test]
    fn test_get_input_line() {
        let input = "
x = 3
y = 8
z = 23
        "
        .trim();

        assert_eq!(
            get_input_line(input, &input[6..]),
            InputInfo {
                column: 0,
                line_number: 1,
                line: "y = 8"
            }
        );

        assert_eq!(
            get_input_line(input, &input[8..]),
            InputInfo {
                column: 2,
                line_number: 1,
                line: "y = 8"
            }
        )
    }
}
