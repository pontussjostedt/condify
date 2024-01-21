use std::{ops::Deref, rc::Rc};

use lib::dsl_parser::parse;

use crate::lib::build::build;

mod lib;
fn main() {
    let input = "
<DETAILED, NO_DETAIL>
x FOR DEFAULT, DETAILED IS \"Bob\"
x FOR NO_DETAIL IS \"Not bob\"
here goes text <*>x<*>
!IF(DETAILED)ONly for detailed!ENDIF

    "
    .trim();

    let (_, result) = parse(input).unwrap();
    let bob = build(&result);
    println!("{:?}", result);

    println!("\n\n{:?}", bob);

    for (k, v) in bob.unwrap().get_accumulated_strings().iter() {
        println!("Key: \"{}\" \n Value: {}", k, v)
    }
}
