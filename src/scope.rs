use std::collections::HashMap;
use crate::r#type::Type;
use std::string::String;

pub struct Scope<'a> {
    types: HashMap<String, Type>,
    parent: Option<&'a Scope<'a>>,
}

impl Scope<'static> {
    pub fn global() -> Scope<'static> {
        let mut types = HashMap::new();
        types.insert(String::from("array-key"), Type::Union(Box::new(Type::Int {min: None, max: None}), Box::new(Type::String(None))));
        return Self { types, parent: None };
    }

    fn register(&mut self, name: String, ty: Type) {
        self.types.insert(name, ty);
    }

    fn lookup(&self, name: &String) -> Option<&Type> {
        match self.types.get(name) {
            Some(ty) => Some(ty),
            None => match self.parent {
                Some(parent) => parent.lookup(name),
                None => None,
            }
        }
    }

    pub fn sub(&self) -> Scope {
        return Scope { types: HashMap::new(), parent: Some(self) }
    }
}
