use crate::r#type::{plain_int, union_from_types, Type};
use std::collections::HashMap;
use std::string::String;

pub struct Scope {
    types: HashMap<String, Type>,
    parent: Option<Box<Scope>>,
}

impl Scope {
    pub fn global() -> Scope {
        let mut types = HashMap::new();
        types.insert(
            String::from("array-key"),
            union_from_types(vec![plain_int(), Type::String(None)]),
        );
        Self {
            types,
            parent: None,
        }
    }

    pub fn register(&mut self, name: String, ty: Type) {
        self.types.insert(name, ty);
    }

    pub fn lookup(&self, name: &String) -> Option<&Type> {
        match self.types.get(name) {
            Some(ty) => Some(ty),
            None => match &self.parent {
                Some(parent) => parent.lookup(name),
                None => None,
            },
        }
    }

    pub fn sub(self) -> Scope {
        Scope {
            types: HashMap::new(),
            parent: Some(Box::new(self)),
        }
    }
}
