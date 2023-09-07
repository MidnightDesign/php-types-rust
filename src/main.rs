mod parser;
mod lexer;
mod scope;
mod r#type;

use crate::parser::parse_type;
use crate::r#type::ToType;
use crate::scope::Scope;


fn main() {
    println!("{}", parse_type("array{int, string}", &Scope::global()).unwrap());
}
