mod lexer;
mod parser;
mod scope;
mod r#type;

use crate::parser::parse_type;
use crate::r#type::{ToType, Type};
use crate::scope::Scope;

fn main() {
    let mut scope = Scope::global();
    let foo_interface = Type::ClassLike {
        name: String::from("FooInterface"),
        parameters: vec![],
        parents: vec![],
    };
    let foo = Type::ClassLike {
        name: String::from("Foo"),
        parameters: vec![],
        parents: vec![foo_interface.clone()],
    };
    let bar = Type::ClassLike {
        name: String::from("Bar"),
        parameters: vec![],
        parents: vec![foo_interface.clone()],
    };
    scope.register(String::from("FooInterface"), foo_interface);
    scope.register(String::from("Foo"), foo);
    scope.register(String::from("Bar"), bar);
    let sub = parse_type("FooInterface", &scope).unwrap();
    let sup = parse_type("FooInterface", &scope).unwrap();
    println!("{}", sub.is_subtype_of(&sup));
}
