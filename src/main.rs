use clap::Parser;
use lib::dsl_parser::parse;
use nom::error::convert_error;
use std::{
    collections::HashMap,
    fs::{self, File},
    io::{self, Read, Write},
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

    #[arg(short, long)]
    suffix: Option<std::path::PathBuf>,

    #[arg(long)]
    watch: bool,
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

fn write_hashmap(
    to_write: HashMap<String, String>,
    target: std::path::PathBuf,
    suffix: &str,
) -> io::Result<()> {
    for (key, value) in to_write {
        let path = target.join(key).join(suffix);
        fs::File::create(path)?.write(value.as_bytes())?;
    }
    Ok(())
}

fn do_try_write<'a>(input: &'a str, target: std::path::PathBuf, suffix: &'a str) -> io::Result<()> {
    let result = get_output_map(input);
    match result {
        Ok(to_write) => write_hashmap(to_write, target, suffix)?,
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
    while cli.watch {}
    Ok(())
}
