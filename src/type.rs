use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use crate::parser::{Node, Parameter, ToNode};
use crate::scope::Scope;

#[derive(PartialEq, Clone)]
pub enum Type {
    Bool(Option<bool>),
    Callable {
        parameters: Vec<Param>,
        return_type: Box<Type>,
    },
    Float,
    Int {
        min: Option<isize>,
        max: Option<isize>,
    },
    IntLiteral(isize),
    Iterable {
        key: Box<Type>,
        value: Box<Type>,
    },
    List {
        type_: Box<Type>,
        non_empty: bool,
    },
    Map {
        key: Box<Type>,
        value: Box<Type>,
        non_empty: bool,
    },
    Mixed,
    Never,
    Null,
    Resource,
    Scalar,
    String(Option<StringFlag>),
    Union(Box<Type>, Box<Type>),
    Void,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Map {
                key,
                value,
                non_empty,
            } => {
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
            Type::Int { min, max } => match (min, max) {
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
            Type::Iterable { key, value } => write!(f, "iterable<{}, {}>", key, value),
            Type::List { type_, non_empty } => {
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
            Type::Callable {
                parameters,
                return_type,
            } => {
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
        }
    }
}

fn int<'a>(parameters: Vec<Node>) -> Result<Type, InvalidType> {
    match parameters.as_slice() {
        [] => Ok(Type::Int {
            min: None,
            max: None,
        }),
        [min, max] => match (min, max) {
            (Node::IntLiteral(min), Node::IntLiteral(max)) => Ok(Type::Int {
                min: Some(*min),
                max: Some(*max),
            }),
            (Node::Identifier(min, _), Node::IntLiteral(max)) => match min.as_str() {
                "min" => Ok(Type::Int {
                    min: None,
                    max: Some(*max),
                }),
                _ => Err(InvalidType::new(format!("int<{}, {}>", min, max))),
            },
            (Node::IntLiteral(min), Node::Identifier(max, _)) => match max.as_str() {
                "max" => Ok(Type::Int {
                    min: Some(*min),
                    max: None,
                }),
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

fn array<'a>(mut parameters: Vec<Node>, non_empty: bool, scope: &Scope) -> Result<Type, InvalidType> {
    let array_key = Type::Union(
        Box::new(Type::Int {
            min: None,
            max: None,
        }),
        Box::new(Type::String(None)),
    );
    let (key, value) = match parameters.len() {
        0 => (array_key, Type::Mixed),
        1 => {
            let value = parameters.pop().unwrap();
            (array_key, Type::from_node(value, scope)?)
        },
        2 => {
            let key = parameters.pop().unwrap();
            let value = parameters.pop().unwrap();
            (Type::from_node(key, scope)?, Type::from_node(value, scope)?)
        },
        _ => {
            return Err(InvalidType::new(format!(
                "array<{}>",
                comma_separated_nodes(parameters)
            )));
        }
    };
    Ok(Type::Map { key: Box::new(key), value: Box::new(value), non_empty })
}

fn list<'a>(mut parameters: Vec<Node>, non_empty: bool, scope: &Scope) -> Result<Type, InvalidType> {
    let value_type = match parameters.len() {
        0 => Type::Mixed,
        1 => {
            let value_type = parameters.pop().unwrap();
            Type::from_node(value_type, scope)?
        },
        _ => {
            return Err(InvalidType::new(format!(
                "list<{}>",
                comma_separated_nodes(parameters)
            )));
        }
    };
    Ok(Type::List {
        type_: Box::new(value_type),
        non_empty,
    })
}

pub fn iterable<'a>(mut parameters: Vec<Node>, scope: &Scope) -> Result<Type, InvalidType> {
    let (key, value) = match parameters.len() {
        0 => (Type::Mixed, Type::Mixed),
        1 => {
            let value = parameters.pop().unwrap();
            (Type::Mixed, Type::from_node(value, scope)?)
        },
        2 => {
            let key = parameters.pop().unwrap();
            let value = parameters.pop().unwrap();
            (Type::from_node(key, scope)?, Type::from_node(value, scope)?)
        },
        _ => {
            return Err(InvalidType::new(format!(
                "iterable<{}>",
                comma_separated_nodes(parameters)
            )));
        }
    };
    Ok(Type::Iterable {
        key: Box::new(key),
        value: Box::new(value),
    })
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
    Ok(Type::Callable {
        return_type: Box::new(match return_type {
            Some(return_type) => Type::from_node(return_type, scope)?,
            None => Type::Void,
        }),
        parameters: vec![],
    })
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

fn flatten(type_: Type) -> Vec<Type> {
    match type_ {
        Type::Union(left, right) => {
            let mut left = flatten(*left);
            let mut right = flatten(*right);
            left.append(&mut right);
            left
        }
        _ => vec![type_],
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

fn create_union<'a>(left: Type, right: Type) -> Type {
    let mut types = flatten(left);
    types.append(&mut flatten(right));
    if types.contains(&&Type::Bool(Some(false))) && types.contains(&&Type::Bool(Some(true))) {
        types.retain(|type_| match type_ {
            Type::Bool(_) => false,
            _ => true
        });
        types.push(Type::Bool(None));
    }
    union_from_types(types)
}

impl Type {
    pub fn from_node(node: Node, scope: &Scope) -> Result<Self, InvalidType> {
        match node {
            Node::Identifier(name, parameters) => match name.as_str() {
                "array" => array(parameters, false, scope),
                "bool" => no_params(Type::Bool(None), parameters),
                "false" => no_params(Type::Bool(Some(false)), parameters),
                "float" => no_params(Type::Float, parameters),
                "int" => int(parameters),
                "iterable" => iterable(parameters, scope),
                "list" => list(parameters, false, scope),
                "mixed" => no_params(Type::Mixed, parameters),
                "negative-int" => no_params(
                    Type::Int {
                        min: None,
                        max: Some(-1),
                    },
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
                    Type::Int {
                        min: Some(1),
                        max: None,
                    },
                    parameters,
                ),
                "scalar" => no_params(Type::Scalar, parameters),
                "string" => no_params(Type::String(None), parameters),
                "true" => no_params(Type::Bool(Some(true)), parameters),
                "void" => no_params(Type::Void, parameters),
                _ => Err(InvalidType::new(format!("{}", name))),
            },
            Node::IntLiteral(value) => Ok(Type::IntLiteral(value)),
            Node::Union(left, right) => Ok(create_union(
                Type::from_node(*left, scope)?,
                Type::from_node(*right, scope)?,
            )),
            Node::Callable(return_type, parameters) => callable(parameters, return_type.map(|t| *t), scope),
            _ => Err(InvalidType::new(format!("{}", node))),
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
        return write!(f, "InvalidType: {}", self.type_string);
    }
}

impl Display for InvalidType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        return write!(f, "InvalidType: {}", self.type_string);
    }
}

impl Error for InvalidType {}

impl InvalidType {
    pub fn new(type_string: String) -> InvalidType {
        InvalidType { type_string }
    }
}

pub trait ToType {
    fn to_type(&self, scope: &Scope) -> Result<Type, InvalidType>;
}

impl ToType for str {
    fn to_type(&self, scope: &Scope) -> Result<Type, InvalidType> {
        Type::from_node(self.to_node().map_err(|parse_error| InvalidType::new(format!("{}", parse_error)))?, scope)
    }
}
