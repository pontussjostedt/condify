use std::marker;

use nom::branch::alt;
use nom::bytes::complete::*;
use nom::character::complete::{anychar, crlf, newline, space0};
use nom::combinator::*;
use nom::error::{context, VerboseError};
use nom::multi::{many0, separated_list0};
use nom::sequence::{delimited, preceded, terminated, tuple};
use nom::IResult;
//type ParserType<'a> = FnMut(&str)
