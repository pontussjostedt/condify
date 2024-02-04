use clap::Parser;
use itertools::Itertools;
use lib::dsl_parser::parse;
use nom::error::convert_error;
use std::{
    collections::HashMap,
    fs::{self, File},
    io::{self, Read, Write},
    path::PathBuf,
    process::Output,
};

use crate::lib::{
    build::build,
    error::{convert_build_error, convert_syntax_error},
};

mod lib;

#[derive(Parser, Debug)]
struct Cli {
    input: std::path::PathBuf,

    #[arg(short, long)]
    output: Option<std::path::PathBuf>,

    //#[arg(short, long)]
    //suffix: Option<std::path::PathBuf>,
    #[arg(long)]
    watch: bool,
}

impl Cli {
    fn target_output(&self) -> PathBuf {
        self.output.clone().unwrap_or_else(|| {
            let input_path = self.input.to_str().expect("Input is not valid unicode");
            let mut out = String::new();

            if let Some(first_dot_index) = input_path.find('.') {
                let (prefix, suffix) = input_path.split_at(first_dot_index);
                out.push_str(prefix);
                out.push_str("%s.");
                out.push_str(&suffix[1..]); // Skip the dot
            } else {
                out.push_str(input_path);
                out.push_str("%s");
            }

            PathBuf::from(out)
        })
    }
}

fn get_output_map<'a>(input: &'a str) -> Result<HashMap<String, String>, String> {
    let (_, token_tree) = parse(input).map_err(|err_wrapped| match err_wrapped {
        nom::Err::Incomplete(_) => panic!("Should not happen"),
        nom::Err::Error(error) => convert_syntax_error(error, input),
        nom::Err::Failure(error) => convert_syntax_error(error, input),
    })?;

    let output = build(&token_tree)
        .map_err(|build_error: lib::build::BuildError<'_>| convert_build_error(build_error, input))?
        .get_accumulated_strings();

    Ok(output)
}

fn write_hashmap(to_write: HashMap<String, String>, target: std::path::PathBuf) -> io::Result<()> {
    for (key, value) in to_write {
        //let path = target.join(key).join(suffix);
        //fs::File::create(path)?.write(value.as_bytes())?;
    }
    Ok(())
}

fn do_try_write<'a>(input: &'a str, target: PathBuf) -> io::Result<()> {
    let result = get_output_map(input);
    match result {
        Ok(to_write) => write_hashmap(to_write, target)?,
        Err(error_msg) => println!("{}", error_msg),
    }
    Ok(())
}

fn main() -> io::Result<()> {
    let cli = Cli::parse();
    let mut file =
        File::open(&cli.input).expect(&format!("Could not read cli.input: {:?}", cli.input));
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    println!("{:?}", "Hello".split(".").collect_vec());
    while cli.watch {}
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;
    #[test]
    fn test_insert_branch_name_no_output_one_dot() {
        let cli = Cli {
            input: PathBuf::from_str("text.txt").unwrap(),
            output: None,
            watch: false,
        };

        let expected_output = PathBuf::from_str("text%s.txt").unwrap();

        assert_eq!(cli.target_output(), expected_output);
    }

    #[test]
    fn test_insert_branch_name_no_output_many_dots() {
        let cli = Cli {
            input: PathBuf::from_str("text.txt.png").unwrap(),
            output: None,
            watch: false,
        };

        let expected_output = PathBuf::from_str("text%s.txt.png").unwrap();

        assert_eq!(cli.target_output(), expected_output);
    }

    #[test]
    fn test_insert_branch_name_no_output_no_dots() {
        let cli = Cli {
            input: PathBuf::from_str("text").unwrap(),
            output: None,
            watch: false,
        };

        let expected_output = PathBuf::from_str("text%s").unwrap();

        assert_eq!(cli.target_output(), expected_output);
    }

    #[test]
    fn test_insert_branch_name_with_output() {
        let cli = Cli {
            input: PathBuf::from_str("text.txt").unwrap(),
            output: Some(PathBuf::from_str("output%s.txt").unwrap()),
            watch: false,
        };

        assert_eq!(cli.target_output(), cli.output.unwrap());
    }

    #[test]
    fn test_insert_branch_name_with_invalid_output() {
        let cli = Cli {
            input: PathBuf::from_str("text.txt").unwrap(),
            output: Some(PathBuf::from_str("output.txt").unwrap()),
            watch: false,
        };

        assert_eq!(cli.target_output(), cli.output.unwrap());
    }
}
