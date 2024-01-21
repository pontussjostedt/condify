use nom::{combinator::fail, Offset};

use crate::lib::markers::READ_VALUE;

use super::{build::BuildError, dsl_parser::Name};

#[derive(Debug, PartialEq, Clone)]
struct InputInfo<'a> {
    line: &'a str,
    line_number: usize,
    column_number: usize,
}

/// * `input` the full string
/// * `substring` a substring of input
fn get_input_line<'a>(input: &'a str, substring: &'a str) -> InputInfo<'a> {
    // https://github.com/rust-bakery/nom/blob/main/src/error.rs
    assert!(!input.is_empty());

    let offset = input.offset(substring);

    let prefix = &input.as_bytes()[..offset];
    let line_number = prefix.iter().filter(|&&b| b == b'\n').count() + 1;
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

    let column_number = line.offset(substring) + 1;

    InputInfo::<'a> {
        line,
        line_number,
        column_number,
    }
}

fn error_from_name<'a>(name: &Name<'a>, full_input: &'a str, prefix_message: &'a str) -> String {
    let fail_info = get_input_line(full_input, name.input);
    let caret = "^".repeat(name.name.len());
    format!(
        "{prefix_message}: at line {line_number}\n\
                {line}\n\
                {caret:>column$}\n",
        prefix_message = prefix_message,
        line_number = fail_info.line_number,
        line = fail_info.line,
        caret = caret,
        column = fail_info.column_number + caret.len() - 1
    )
}

pub fn convert_build_error<'a>(build_error: BuildError<'a>, full_input: &'a str) -> String {
    match build_error {
        BuildError::AlreadyDeclared(failed_name) => {
            error_from_name(failed_name, full_input, "Attempted to declare twice")
        }
        BuildError::NotDeclaredOnAssingment(failed_name) => error_from_name(
            failed_name,
            full_input,
            "Attempted to assign without declaration",
        ),
        BuildError::NotAssignedOnReadValue(read_value) => {
            let not_assigned = &read_value.name;
            let read_value_info = get_input_line(full_input, read_value.input);
            let caret = "^".repeat(not_assigned.name.len() + READ_VALUE.len() * 2);
            format!(
                "Attempting to read unassigned variable for branch: \"{not_assigned}\", at line: {line_number}\n\
                {line}\n\
                {caret:>column$}\n",
                not_assigned = not_assigned.name,
                line_number = read_value_info.line_number,
                line = read_value_info.line,
                caret = caret,
                column = read_value_info.column_number + caret.len() - 1
            )
        }
        BuildError::NoDefaultValueOnReadValue(read_value) => {
            let read_value_info = get_input_line(full_input, read_value.input);
            let caret = "^".repeat(read_value.name.name.len() + READ_VALUE.len() * 2);
            format!(
                "Attempting to read a variable without DEFAULT assigned when DEFAULT is in scope, at line: {line_number}\n\
                {line}\n\
                {caret:>column$}\n",
                line_number = read_value_info.line_number,
                line = read_value_info.line,
                column = read_value_info.column_number + caret.len() - 1

            )
        }
        BuildError::NotInScopeOnIf { body, fail_name } => error_from_name(
            fail_name,
            full_input,
            "Attempting to include branch which is not in scope",
        ),
        BuildError::DeclareDefault(name) => error_from_name(
            name,
            full_input,
            "Attempting to declare DEFAULT which is a reserved keyword",
        ),
    }
}

#[cfg(test)]
mod tests {
    use crate::lib::dsl_parser::Name;

    use super::*;

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
                column_number: 1,
                line_number: 2,
                line: "y = 8"
            }
        );

        assert_eq!(
            get_input_line(input, &input[8..]),
            InputInfo {
                column_number: 3,
                line_number: 2,
                line: "y = 8"
            }
        )
    }
}
