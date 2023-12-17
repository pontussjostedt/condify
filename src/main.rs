mod lib;
use std::fmt::Display;

use lib::parsing::{error::*};

use nom::{
    error::{context, VerboseError, convert_error, VerboseErrorKind},
    sequence::{preceded, terminated},
    combinator::cut,
    bytes::complete::take_while,
    multi::many1,
};

fn main() {

}
