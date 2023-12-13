use lib::parser::parse;


mod lib;
fn main() {
    let res = parse("<Detail, NoDetail>
    ASSIGNMENT {
        DEFAULT = \"Bob\"
    }
    Here there be stuffs happening
    ");
    println!("{:?}", res);
}
