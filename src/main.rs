use std::ops::Add;

use crate::lib::build::build;
use imstr::*;
use lib::dsl_parser::parse;
mod lib;
fn main() {
    let input = "<DETAIL>
name1 FOR DETAIL IS \"VALUE\"
name1 FOR DETAIL IS \"VALUE2\"
<*>name1<*>";

    let (_, result) = parse(input).unwrap();
    let debug_string = result
        .iter()
        .map(|token| token.short_form() + "\n")
        .collect::<String>();

    println!("SHORTFORM TRACE: \n*******\n {} \n******", debug_string);

    let x = build(input, result).unwrap();
}
