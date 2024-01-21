use std::{io, ops::Deref, rc::Rc};

use clap::Parser;
use lib::dsl_parser::parse;

use crate::lib::build::build;

mod lib;

#[derive(Parser, Debug)]
struct Cli {
    input: std::path::PathBuf,

    #[arg(short, long)]
    output: Option<std::path::PathBuf>,

    #[arg(long)]
    watch: bool,
}

fn main() -> io::Result<()> {
    let cli = Cli::parse();
    //println!("{:?}", cli);
    Ok(())
}
