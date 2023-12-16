mod lib;
use std::fmt::Display;

use lib::parsing::{error::*};
use nom::{
    bytes::complete::take_while,
    combinator::cut,
    error::{context, convert_error, ErrorKind, ParseError, VerboseError, VerboseErrorKind},
    multi::many1,
    sequence::{preceded, terminated, delimited},
    Compare, CompareResult, IResult, InputLength, InputTake,
};



fn main() {
 
}
