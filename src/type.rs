use crate::parser::{Node, Parameter, StructMember, ToNode};
use crate::scope::Scope;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::string::String;

#[derive(PartialEq, Clone)]
pub enum Type {
    Bool(Option<bool>),
    Callable {
        parameters: Vec<Param>,
        return_type: Box<Type>,
    },
    ClassLike {
        name: String,
        parameters: Vec<Type>,
        parents: Vec<Type>,
    },
    ClassString(Option<Box<Type>>),
    Float,
    Int {
        min: Option<isize>,
        max: Option<isize>,
    },
    Intersection(Box<Type>, Box<Type>),
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
    StringLiteral(String),
    Struct(Vec<(String, Type, bool)>),
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
            Type::Struct(members) => {
                write!(f, "array{{")?;
                for (i, (name, type_, optional)) in members.iter().enumerate() {
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

fn array(mut parameters: Vec<Node>, non_empty: bool, scope: &Scope) -> Result<Type, InvalidType> {
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
        }
        2 => {
            let key = parameters.pop().unwrap();
            let value = parameters.pop().unwrap();
            (Type::from_node(key, scope)?, Type::from_node(value, scope)?)
        }
        _ => {
            return Err(InvalidType::new(format!(
                "array<{}>",
                comma_separated_nodes(parameters)
            )));
        }
    };
    Ok(Type::Map {
        key: Box::new(key),
        value: Box::new(value),
        non_empty,
    })
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
        }
        2 => {
            let key = parameters.pop().unwrap();
            let value = parameters.pop().unwrap();
            (Type::from_node(key, scope)?, Type::from_node(value, scope)?)
        }
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
        Type::Union(left, right) | Type::Intersection(left, right) => {
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
            _ => true,
        });
        types.push(Type::Bool(None));
    }
    union_from_types(types)
}

fn create_struct(members: HashMap<String, StructMember>) -> Result<Type, InvalidType> {
    let mut struct_members = Vec::new();
    for (name, member) in members {
        struct_members.push((
            name,
            Type::from_node(member.type_node, &Scope::global())?,
            member.optional,
        ));
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
            Node::Struct(members) => create_struct(members),
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

pub fn compare_bool(sub_value: &Option<bool>, upper_value: &Option<bool>) -> bool {
    match (sub_value, upper_value) {
        (_, None) => true,
        (None, Some(_)) => false,
        (Some(sub_value), Some(uper_value)) => sub_value == uper_value,
    }
}

pub fn compare_callable(
    sub_parameters: &[Param],
    super_parameters: &[Param],
    sub_return_type: &Type,
    super_return_type: &Type,
) -> bool {
    if !sub_return_type.is_subtype_of(super_return_type) {
        return false;
    }
    for (i, sub_param) in sub_parameters.iter().enumerate() {
        if i >= super_parameters.len() {
            return false;
        }
        let super_param = &super_parameters[i];
        if !super_param.type_.is_subtype_of(&sub_param.type_) {
            return false;
        }
    }
    return true;
}

fn compare_int(
    sub_min: &Option<isize>,
    sub_max: &Option<isize>,
    sup_min: &Option<isize>,
    sup_max: &Option<isize>,
) -> bool {
    let sub = (
        sub_min.map_or(isize::MIN, |v| v),
        sub_max.map_or(isize::MAX, |v| v),
    );
    let sup = (
        sup_min.map_or(isize::MIN, |v| v),
        sup_max.map_or(isize::MAX, |v| v),
    );
    sub.0 >= sup.0 && sub.1 <= sup.1
}

fn compare_struct_map(sub_members: &Vec<(String, Type, bool)>, sup_value: &Type) -> bool {
    for (_, sub_type, _) in sub_members {
        if sub_type.is_subtype_of(sup_value) {
            continue;
        }
        return false;
    }
    true
}

fn intersection_to_map(left: &Type, right: &Type) -> Type {
    let mut elements = flatten(left.clone());
    elements.append(&mut flatten(right.clone()));
    let mut key = Type::Never;
    let mut value = Type::Never;
    let mut non_empty = false;
    for element in elements {
        if let Type::Struct(members) = element {
            for (name, type_, optional) in members {
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
    Type::Map {
        key: Box::new(key),
        value: Box::new(value),
        non_empty,
    }
}

fn compare_map_map(sub: &Type, sup: &Type) -> bool {
    if let Type::Map {
        key: sub_key,
        value: sub_value,
        non_empty: sub_non_empty,
    } = sub
    {
        if let Type::Map {
            key: sup_key,
            value: sup_value,
            non_empty: sup_non_empty,
        } = &sup
        {
            if *sup_non_empty && !*sub_non_empty {
                return false;
            }
            return sub_key.is_subtype_of(sup_key) && sub_value.is_subtype_of(sup_value);
        }
    }
    false
}

fn compare_classlike_classlike(sub: &Type, sup: &Type) -> bool {
    match (sub, sup) {
        (
            Type::ClassLike {
                name: sub_name,
                parameters: sub_parameters,
                ..
            },
            Type::ClassLike {
                name: sup_name,
                parameters: sup_parameters,
                ..
            },
        ) if sub_name == sup_name => {
            for (sub_parameter, sup_parameter) in sub_parameters.iter().zip(sup_parameters) {
                if !sub_parameter.is_subtype_of(sup_parameter) {
                    return false;
                }
            }
            true
        }
        (Type::ClassLike { parents, .. }, _) => {
            parents.iter().any(|parent| parent.is_subtype_of(sup))
        }
        _ => false,
    }
}

impl Type {
    pub fn is_subtype_of(&self, sup: &Type) -> bool {
        if self == sup {
            return true;
        }
        match (self, sup) {
            (_, Type::Mixed) => true,
            (Type::Bool(sub_value), Type::Bool(upper_value)) => {
                compare_bool(sub_value, upper_value)
            }
            (_, Type::Union(left, right)) => self.is_subtype_of(left) || self.is_subtype_of(right),
            (
                Type::Bool(_)
                | Type::Int { .. }
                | Type::Float
                | Type::String(_)
                | Type::IntLiteral(_)
                | Type::StringLiteral(_)
                | Type::ClassString(_),
                Type::Scalar,
            ) => true,
            (Type::String(_), Type::String(None)) => true,
            (
                Type::Callable {
                    parameters: sub_parameters,
                    return_type: sub_return_type,
                },
                Type::Callable {
                    parameters: super_parameters,
                    return_type: super_return_type,
                },
            ) => compare_callable(
                sub_parameters,
                super_parameters,
                sub_return_type,
                super_return_type,
            ),
            (_, Type::Void) => true,
            (Type::Int { min: _, max: _ }, Type::Float) => true,
            (
                Type::Int {
                    min: sub_min,
                    max: sub_max,
                },
                Type::Int {
                    min: sup_min,
                    max: sup_max,
                },
            ) => compare_int(sub_min, sub_max, sup_min, sup_max),
            (Type::Int { min, max }, Type::IntLiteral(value)) => {
                min.map_or(false, |min| min == *value) && max.map_or(false, |max| max == *value)
            }
            (Type::IntLiteral(_), Type::Float | Type::Int { .. }) => true,
            (
                Type::Struct(sub_members),
                Type::Map {
                    key: _,
                    value,
                    non_empty: _,
                },
            ) => compare_struct_map(sub_members, value),
            (
                Type::Intersection(left, right),
                Type::Map {
                    key: _,
                    value: _,
                    non_empty: _,
                },
            ) => compare_map_map(&intersection_to_map(left, right), sup),
            (Type::ClassLike { .. }, Type::ClassLike { .. }) => {
                compare_classlike_classlike(self, sup)
            }
            (_, _) => false,
        }
    }
}
