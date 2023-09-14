use crate::parser::{Node, Parameter, StructMember as NodeStructMember, ToNode};
use crate::scope::Scope;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::string::String;

#[derive(PartialEq, Clone)]
pub struct Map {
    pub key: Box<Type>,
    pub value: Box<Type>,
    pub non_empty: bool,
}

#[derive(PartialEq, Clone)]
pub struct Callable {
    pub parameters: Vec<Param>,
    pub return_type: Box<Type>,
}

#[derive(PartialEq, Clone)]
pub struct Int {
    pub min: Option<isize>,
    pub max: Option<isize>,
}

#[derive(PartialEq, Clone)]
pub struct Iterable {
    pub key: Box<Type>,
    pub value: Box<Type>,
}

#[derive(PartialEq, Clone)]
pub struct List {
    pub type_: Box<Type>,
    pub non_empty: bool,
}

#[derive(PartialEq, Clone)]
pub struct StructMember {
    pub type_: Type,
    pub optional: bool,
}

#[derive(PartialEq, Clone)]
pub enum Type {
    Bool(Option<bool>),
    Callable(Callable),
    ClassLike {
        name: String,
        parameters: Vec<Type>,
        parents: Vec<Type>,
    },
    ClassString(Option<Box<Type>>),
    Float,
    Int(Int),
    Intersection(Box<Type>, Box<Type>),
    IntLiteral(isize),
    Iterable(Iterable),
    List(List),
    Map(Map),
    Mixed,
    Never,
    Null,
    Resource,
    Scalar,
    String(Option<StringFlag>),
    StringLiteral(String),
    Struct(HashMap<String, StructMember>),
    Tuple(Vec<Type>),
    Union(Box<Type>, Box<Type>),
    Void,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::ClassString(type_) => match type_ {
                None => write!(f, "class-string"),
                Some(type_) => write!(f, "class-string<{}>", type_),
            },
            Type::ClassLike {
                name,
                parameters,
                parents: _,
            } => {
                if parameters.is_empty() {
                    write!(f, "{}", name)
                } else {
                    write!(
                        f,
                        "{}<{}>",
                        name,
                        parameters
                            .iter()
                            .map(|type_| type_.to_string())
                            .collect::<Vec<String>>()
                            .join(", ")
                    )
                }
            }
            Type::Intersection(left, right) => write!(f, "{} & {}", left, right),
            Type::Map(Map {
                key,
                value,
                non_empty,
            }) => {
                if *non_empty {
                    write!(f, "non-empty-array<")?;
                } else {
                    write!(f, "array<")?;
                }
                write!(f, "{}, {}>", key, value)?;
                Ok(())
            }
            Type::Union(left, right) => write!(f, "{} | {}", left, right),
            Type::Mixed => write!(f, "mixed"),
            Type::String(flag) => match flag {
                None => write!(f, "string"),
                Some(StringFlag::NonEmpty) => write!(f, "non-empty-string"),
                Some(StringFlag::Numeric) => write!(f, "numeric-string"),
            },
            Type::Int(Int { min, max }) => match (min, max) {
                (None, None) => write!(f, "int"),
                (Some(min), None) => write!(f, "int<{}, max>", min),
                (None, Some(max)) => write!(f, "int<max, {}>", max),
                (Some(min), Some(max)) => write!(f, "int<{}, {}>", min, max),
            },
            Type::Bool(value) => match value {
                None => write!(f, "bool"),
                Some(value) => write!(f, "bool<{}>", value),
            },
            Type::Scalar => write!(f, "scalar"),
            Type::Float => write!(f, "float"),
            Type::IntLiteral(value) => write!(f, "{}", value),
            Type::Iterable(Iterable { key, value }) => write!(f, "iterable<{}, {}>", key, value),
            Type::List(List { type_, non_empty }) => {
                if *non_empty {
                    write!(f, "non-empty-list<")?;
                } else {
                    write!(f, "list<")?;
                }
                write!(f, "{}>", type_)
            }
            Type::Never => write!(f, "never"),
            Type::Null => write!(f, "null"),
            Type::Resource => write!(f, "resource"),
            Type::Void => write!(f, "void"),
            Type::Callable(Callable {
                parameters,
                return_type,
            }) => {
                write!(f, "callable(")?;
                for (i, param) in parameters.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param.type_)?;
                    if param.optional {
                        write!(f, "=")?;
                    }
                }
                write!(f, "): {}", return_type)
            }
            Type::Struct(members) => {
                write!(f, "array{{")?;
                for (i, (name, StructMember { type_, optional })) in members.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", name)?;
                    if *optional {
                        write!(f, "?")?;
                    }
                    write!(f, ": {}", type_)?;
                }
                write!(f, "}}")
            }
            Type::StringLiteral(value) => write!(f, "'{}'", value),
            Type::Tuple(elements) => {
                write!(f, "array{{")?;
                for (i, element) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", element)?;
                }
                write!(f, "}}")
            }
        }
    }
}

fn int(parameters: Vec<Node>) -> Result<Type, InvalidType> {
    match parameters.as_slice() {
        [] => Ok(Type::Int(Int {
            min: None,
            max: None,
        })),
        [min, max] => match (min, max) {
            (Node::IntLiteral(min), Node::IntLiteral(max)) => Ok(Type::Int(Int {
                min: Some(*min),
                max: Some(*max),
            })),
            (Node::Identifier(min, _), Node::IntLiteral(max)) => match min.as_str() {
                "min" => Ok(Type::Int(Int {
                    min: None,
                    max: Some(*max),
                })),
                _ => Err(InvalidType::new(format!("int<{}, {}>", min, max))),
            },
            (Node::IntLiteral(min), Node::Identifier(max, _)) => match max.as_str() {
                "max" => Ok(Type::Int(Int {
                    min: Some(*min),
                    max: None,
                })),
                _ => Err(InvalidType::new(format!("int<{}, {}>", min, max))),
            },
            _ => Err(InvalidType::new(format!("int<{}, {}>", min, max))),
        },
        _ => Err(InvalidType::new(format!(
            "int<{}>",
            comma_separated_nodes(parameters)
        ))),
    }
}

fn comma_separated_nodes(nodes: Vec<Node>) -> String {
    nodes
        .iter()
        .map(|node| node.to_string())
        .collect::<Vec<String>>()
        .join(", ")
}

fn array(mut parameters: Vec<Node>, non_empty: bool, scope: &Scope) -> Result<Type, InvalidType> {
    let array_key = scope
        .lookup(&String::from("array-key"))
        .ok_or(InvalidType::new(
            "array-key not found in global scope".to_string(),
        ))?;
    let (key, value) = match parameters.len() {
        0 => (array_key.clone(), Type::Mixed),
        1 => {
            let value = parameters.pop().unwrap();
            (array_key.clone(), Type::from_node(value, scope)?)
        }
        2 => {
            let value = parameters.pop().unwrap();
            let key = parameters.pop().unwrap();
            (Type::from_node(key, scope)?, Type::from_node(value, scope)?)
        }
        _ => {
            return Err(InvalidType::new(format!(
                "array<{}>",
                comma_separated_nodes(parameters)
            )));
        }
    };
    Ok(Type::Map(Map {
        key: Box::new(key),
        value: Box::new(value),
        non_empty,
    }))
}

fn list<'a>(
    mut parameters: Vec<Node>,
    non_empty: bool,
    scope: &Scope,
) -> Result<Type, InvalidType> {
    let value_type = match parameters.len() {
        0 => Type::Mixed,
        1 => {
            let value_type = parameters.pop().unwrap();
            Type::from_node(value_type, scope)?
        }
        _ => {
            return Err(InvalidType::new(format!(
                "list<{}>",
                comma_separated_nodes(parameters)
            )));
        }
    };
    Ok(Type::List(List {
        type_: Box::new(value_type),
        non_empty,
    }))
}

pub fn iterable<'a>(mut parameters: Vec<Node>, scope: &Scope) -> Result<Type, InvalidType> {
    let (key, value) = match parameters.len() {
        0 => (Type::Mixed, Type::Mixed),
        1 => {
            let value = parameters.pop().unwrap();
            (Type::Mixed, Type::from_node(value, scope)?)
        }
        2 => {
            let value = parameters.pop().unwrap();
            let key = parameters.pop().unwrap();
            (Type::from_node(key, scope)?, Type::from_node(value, scope)?)
        }
        _ => {
            return Err(InvalidType::new(format!(
                "iterable<{}>",
                comma_separated_nodes(parameters)
            )));
        }
    };
    Ok(Type::Iterable(Iterable {
        key: Box::new(key),
        value: Box::new(value),
    }))
}

pub fn callable<'a>(
    parameters: Vec<Parameter>,
    return_type: Option<Node>,
    scope: &Scope,
) -> Result<Type, InvalidType> {
    let mut params = Vec::new();
    for parameter in parameters {
        params.push(Param::new(
            Type::from_node(parameter.type_node, scope)?,
            parameter.optional,
        ));
    }
    Ok(Type::Callable(Callable {
        return_type: Box::new(match return_type {
            Some(return_type) => Type::from_node(return_type, scope)?,
            None => Type::Void,
        }),
        parameters: vec![],
    }))
}

fn no_params<'a>(type_: Type, parameters: Vec<Node>) -> Result<Type, InvalidType> {
    match parameters.as_slice() {
        [] => Ok(type_),
        _ => Err(InvalidType::new(format!(
            "{}<{}>",
            type_,
            comma_separated_nodes(parameters)
        ))),
    }
}

pub fn flatten(t: Type) -> Vec<Type> {
    match t {
        Type::Union(left, right) | Type::Intersection(left, right) => {
            let mut left = flatten(*left);
            let mut right = flatten(*right);
            left.append(&mut right);
            left
        }
        _ => vec![t],
    }
}

fn union_from_types<'a>(types: Vec<Type>) -> Type {
    match types.as_slice() {
        [] => Type::Never,
        [type_] => (*type_).clone(),
        [left, rest @ ..] => {
            let right = union_from_types(rest.to_vec());
            Type::Union(Box::new(left.clone()), Box::new(right))
        }
    }
}

pub fn create_union(left: Type, right: Type) -> Type {
    let mut types = flatten(left);
    types.append(&mut flatten(right));
    if types.contains(&&Type::Bool(Some(false))) && types.contains(&&Type::Bool(Some(true))) {
        types.retain(|type_| match type_ {
            Type::Bool(_) => false,
            _ => true,
        });
        types.push(Type::Bool(None));
    }
    union_from_types(types)
}

fn create_struct(
    members: HashMap<String, NodeStructMember>,
    scope: &Scope,
) -> Result<Type, InvalidType> {
    let mut struct_members = HashMap::new();
    for (name, member) in members {
        struct_members.insert(
            name,
            StructMember {
                type_: Type::from_node(member.type_node, scope)?,
                optional: member.optional,
            },
        );
    }
    Ok(Type::Struct(struct_members))
}

fn class_string(mut parameters: Vec<Node>, scope: &Scope) -> Result<Type, InvalidType> {
    match parameters.len() {
        0 => Ok(Type::ClassString(None)),
        1 => {
            let type_ = parameters.pop().unwrap();
            Ok(Type::ClassString(Some(Box::new(Type::from_node(
                type_, scope,
            )?))))
        }
        _ => Err(InvalidType::new(format!(
            "class-string<{}>",
            comma_separated_nodes(parameters)
        ))),
    }
}

impl Type {
    pub fn from_node(node: Node, scope: &Scope) -> Result<Self, InvalidType> {
        match node {
            Node::Identifier(name, parameters) => match name.as_str() {
                "array" => array(parameters, false, scope),
                "bool" => no_params(Type::Bool(None), parameters),
                "class-string" => class_string(parameters, scope),
                "false" => no_params(Type::Bool(Some(false)), parameters),
                "float" => no_params(Type::Float, parameters),
                "int" => int(parameters),
                "iterable" => iterable(parameters, scope),
                "list" => list(parameters, false, scope),
                "mixed" => no_params(Type::Mixed, parameters),
                "negative-int" => no_params(
                    Type::Int(Int {
                        min: None,
                        max: Some(-1),
                    }),
                    parameters,
                ),
                "never" => no_params(Type::Never, parameters),
                "non-empty-array" => array(parameters, true, scope),
                "non-empty-list" => list(parameters, true, scope),
                "non-empty-string" => {
                    no_params(Type::String(Some(StringFlag::NonEmpty)), parameters)
                }
                "null" => no_params(Type::Null, parameters),
                "numeric-string" => no_params(Type::String(Some(StringFlag::Numeric)), parameters),
                "resource" => no_params(Type::Resource, parameters),
                "positive-int" => no_params(
                    Type::Int(Int {
                        min: Some(1),
                        max: None,
                    }),
                    parameters,
                ),
                "scalar" => no_params(Type::Scalar, parameters),
                "string" => no_params(Type::String(None), parameters),
                "true" => no_params(Type::Bool(Some(true)), parameters),
                "void" => no_params(Type::Void, parameters),
                _ => match scope.lookup(&name) {
                    Some(tipe) => Ok(tipe.clone()),
                    None => Err(InvalidType::new(name.to_string())),
                },
            },
            Node::IntLiteral(value) => Ok(Type::IntLiteral(value)),
            Node::Union(left, right) => Ok(create_union(
                Type::from_node(*left, scope)?,
                Type::from_node(*right, scope)?,
            )),
            Node::Callable(return_type, parameters) => {
                callable(parameters, return_type.map(|t| *t), scope)
            }
            Node::Struct(members) => create_struct(members, scope),
            Node::StringLiteral(value) => Ok(Type::StringLiteral(value)),
            Node::Tuple(elements) => {
                let mut types = Vec::new();
                for element in elements {
                    types.push(Type::from_node(element, scope)?);
                }
                Ok(Type::Tuple(types))
            }
            Node::Intersection(left, right) => Ok(Type::Intersection(
                Box::new(Type::from_node(*left, scope)?),
                Box::new(Type::from_node(*right, scope)?),
            )),
        }
    }
}

#[derive(PartialEq, Clone)]
pub enum StringFlag {
    NonEmpty,
    Numeric,
}

#[derive(PartialEq, Clone)]
pub struct Param {
    pub type_: Type,
    optional: bool,
}

impl Param {
    pub fn new(type_: Type, optional: bool) -> Param {
        Param { type_, optional }
    }
}

pub struct InvalidType {
    type_string: String,
}

impl Debug for InvalidType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "InvalidType: {}", self.type_string)
    }
}

impl Display for InvalidType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "InvalidType: {}", self.type_string)
    }
}

impl Error for InvalidType {}

impl InvalidType {
    pub fn new(type_string: std::string::String) -> InvalidType {
        InvalidType { type_string }
    }
}

pub trait ToType {
    fn to_type(&self, scope: &Scope) -> Result<Type, InvalidType>;
}

impl ToType for str {
    fn to_type(&self, scope: &Scope) -> Result<Type, InvalidType> {
        Type::from_node(
            self.to_node()
                .map_err(|parse_error| InvalidType::new(format!("{}", parse_error)))?,
            scope,
        )
    }
}

pub fn intersection_to_map(left: &Type, right: &Type) -> Type {
    let mut elements = flatten(left.clone());
    elements.append(&mut flatten(right.clone()));
    let mut key = Type::Never;
    let mut value = Type::Never;
    let mut non_empty = false;
    for element in elements {
        if let Type::Struct(members) = element {
            for (name, StructMember { type_, optional }) in members {
                key = Type::Union(Box::new(key), Box::new(Type::StringLiteral(name)));
                value = Type::Union(Box::new(value), Box::new(type_));
                if !optional {
                    non_empty = true;
                }
            }
        } else {
            return Type::Never;
        }
    }
    Type::Map(Map {
        key: Box::new(key),
        value: Box::new(value),
        non_empty,
    })
}

fn create_int() -> Type {
    Type::Int(Int {
        min: None,
        max: None,
    })
}

pub fn to_iterable(t: &Type) -> Option<Iterable> {
    let (key, value) = match t {
        Type::List(List { type_, .. }) => (create_int(), *type_.clone()),
        Type::Map(Map { key, value, .. }) => (*key.clone(), *value.clone()),
        Type::Iterable(Iterable { key, value }) => (*key.clone(), *value.clone()),
        _ => return None,
    };
    Some(Iterable {
        key: Box::new(key),
        value: Box::new(value),
    })
}

fn struct_key_value_types(members: HashMap<String, StructMember>) -> (Type, Type) {
    let mut key = Type::Never;
    let mut value = Type::Never;
    for (name, StructMember { type_, optional }) in members {
        key = Type::Union(Box::new(key), Box::new(Type::StringLiteral(name)));
        value = Type::Union(Box::new(value), Box::new(type_));
        if !optional {
            value = Type::Union(Box::new(value), Box::new(Type::Null));
        }
    }
    (key, value)
}

pub fn to_map(t: &Type) -> Option<Map> {
    match t {
        Type::Map(map) => Some(map.clone()),
        Type::List(List { type_, non_empty }) => Some(Map {
            key: Box::new(create_int()),
            value: type_.clone(),
            non_empty: *non_empty,
        }),
        Type::Struct(members) => {
            let (key, value) = struct_key_value_types(members.clone());
            Some(Map {
                key: Box::new(key),
                value: Box::new(value),
                non_empty: false,
            })
        }
        _ => None,
    }
}

pub fn to_list(t: &Type) -> Option<List> {
    match t {
        Type::List(list) => Some(list.clone()),
        Type::Tuple(elements) => Some(List {
            type_: Box::new(union_from_types(elements.clone())),
            non_empty: !elements.is_empty(),
        }),
        Type::Struct(members) if members.is_empty() => Some(List {
            type_: Box::new(Type::Never),
            non_empty: false,
        }),
        _ => None,
    }
}
