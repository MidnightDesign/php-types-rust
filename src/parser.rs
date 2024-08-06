use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::iter::Peekable;
use std::slice::Iter;

use crate::lexer::{lex, Token};
use crate::r#type::{InvalidType, Type};
use crate::scope::Scope;

type Tokens<'a> = Peekable<Iter<'a, Token>>;

#[derive(Debug)]
pub struct Parameter {
    pub type_node: Node,
    pub optional: bool,
}

#[derive(Debug)]
pub struct StructMember {
    pub type_node: Node,
    pub optional: bool,
}

#[derive(Debug)]
pub enum Node {
    Callable(Option<Box<Node>>, Vec<Parameter>),
    Identifier(String, Vec<Node>),
    Intersection(Box<Node>, Box<Node>),
    IntLiteral(isize),
    StringLiteral(String),
    Struct(HashMap<String, StructMember>, Option<Option<Box<Node>>>),
    Tuple(Vec<Node>),
    Union(Box<Node>, Box<Node>),
}

pub struct ParseError {
    message: String,
}

impl Debug for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl Error for ParseError {}

fn write_comma_separated(f: &mut Formatter, nodes: &Vec<Node>) -> Result<(), fmt::Error> {
    for (i, type_param) in nodes.iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        write!(f, "{}", type_param)?;
    }
    Ok(())
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Node::StringLiteral(string) => write!(f, "'{}'", string),
            Node::IntLiteral(integer) => write!(f, "{}", integer),
            Node::Identifier(name, type_params) => {
                write!(f, "{}", name)?;
                if !type_params.is_empty() {
                    write!(f, "<")?;
                    write_comma_separated(f, type_params)?;
                    write!(f, ">")?;
                }
                Ok(())
            }
            Node::Union(left, right) => write!(f, "{} | {}", left, right),
            Node::Intersection(left, right) => write!(f, "{} & {}", left, right),
            Node::Tuple(elements) => {
                write!(f, "array{{")?;
                write_comma_separated(f, elements)?;
                write!(f, "}}")
            }
            Node::Struct(members, other) => {
                write!(f, "array{{")?;
                for (i, (name, member)) in members.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", name)?;
                    if member.optional {
                        write!(f, "?")?;
                    }
                    write!(f, ": {}", member.type_node)?;
                }
                if let Some(other_type) = other {
                    match other_type {
                        Some(other_type) => write!(f, ", ...{}", other_type)?,
                        None => write!(f, ", ...")?,
                    }
                }
                write!(f, "}}")
            }
            Node::Callable(return_type, parameters) => {
                write!(f, "callable(")?;
                for (i, parameter) in parameters.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", parameter.type_node)?;
                    if parameter.optional {
                        write!(f, "=")?;
                    }
                }
                write!(f, ")")?;
                if let Some(return_type) = return_type {
                    write!(f, ": {}", return_type)?;
                }
                Ok(())
            }
        }
    }
}

impl ParseError {
    fn unexpected_end_of_input() -> Self {
        ParseError {
            message: "Unexpected end of input".to_string(),
        }
    }
    fn unexpected_token(token: &Token, expected: Option<Vec<&Token>>) -> Self {
        let mut message = format!("Unexpected token {:?}", token);
        if let Some(expected) = expected {
            message.push_str(", expected one of: ");
            for (i, token) in expected.iter().enumerate() {
                if i > 0 {
                    message.push_str(", ");
                }
                message.push_str(token.name());
            }
        }
        ParseError { message }
    }
}

fn skip_whitespace(tokens: &mut Peekable<Iter<Token>>) {
    loop {
        match tokens.peek() {
            None => break,
            Some(token) => {
                if let Token::Whitespace = token {
                    tokens.next();
                } else {
                    break;
                }
            }
        }
    }
}

// callable(string, int=, bool=): void
//                  ====
fn parse_param(tokens: &mut Peekable<Iter<Token>>) -> Option<Parameter> {
    let type_node = match node_from_tokens(tokens) {
        Ok(node) => node,
        Err(_) => return None,
    };
    let optional = match tokens.peek() {
        Some(Token::Equals) => {
            tokens.next();
            true
        }
        _ => false,
    };
    Some(Parameter {
        type_node,
        optional,
    })
}

// callable(string, int): void
//          ===========
fn parse_params(tokens: &mut Peekable<Iter<Token>>) -> Vec<Parameter> {
    let mut parameters = Vec::new();
    loop {
        skip_whitespace(tokens);
        if matches!(tokens.peek(), None | Some(Token::CloseParen)) {
            break;
        }
        match parse_param(tokens) {
            Some(parameter) => parameters.push(parameter),
            None => break,
        };
        skip(Token::Comma, tokens);
    }
    parameters
}

fn expect(expected: Token, tokens: &mut Peekable<Iter<Token>>) -> Result<(), ParseError> {
    match tokens.peek() {
        None => Err(ParseError::unexpected_end_of_input()),
        Some(actual) if **actual == expected => {
            tokens.next();
            Ok(())
        }
        Some(token) => Err(ParseError::unexpected_token(token, Some(vec![&expected]))),
    }
}

// callable(string, int): void
// ===========================
fn parse_callable(tokens: &mut Peekable<Iter<Token>>) -> Result<Node, ParseError> {
    expect(Token::Identifier("callable".to_string()), tokens)?;
    let token = tokens.peek();
    match token {
        Some(Token::OpenParen) => {}
        _ => return Ok(Node::Callable(None, Vec::new())),
    };
    tokens.next();
    let parameters = parse_params(tokens);
    expect(Token::CloseParen, tokens)?;
    skip_whitespace(tokens);
    let return_type = match tokens.peek() {
        Some(Token::Colon) => {
            tokens.next();
            Some(Box::new(node_from_tokens(tokens)?))
        }
        _ => None,
    };
    Ok(Node::Callable(return_type, parameters))
}

// Parses this:
// array{string, foo, ... }
//              =====
fn parse_tuple_member(tokens: &mut Peekable<Iter<Token>>) -> Option<Node> {
    skip_whitespace(tokens);
    if let Token::CloseBrace = tokens.peek()? {
        return None;
    }
    let member_type = match node_from_tokens(tokens) {
        Ok(node) => node,
        Err(_) => return None,
    };
    skip_whitespace(tokens);
    Some(match tokens.peek() {
        Some(Token::Comma) => {
            tokens.next();
            member_type
        }
        _ => member_type,
    })
}

// Parses this:
// array{string, int, ... }
//              ===========
fn parse_tuple(first_type: Node, tokens: &mut Peekable<Iter<Token>>) -> Result<Node, ParseError> {
    skip_whitespace(tokens);
    let mut types = vec![first_type];
    loop {
        let member = parse_tuple_member(tokens);
        match member {
            None => break,
            Some(member) => types.push(member),
        }
        skip_whitespace(tokens);
    }
    expect(Token::CloseBrace, tokens)?;
    Ok(Node::Tuple(types))
}

fn skip(expected: Token, tokens: &mut Peekable<Iter<Token>>) {
    let token = match tokens.peek() {
        None => return,
        Some(token) => token,
    };
    if expected != **token {
        return;
    }
    tokens.next();
}

// Parses this:
// array{ foo?: string, bar: string, ... }
//                     =============
fn parse_struct_member(
    tokens: &mut Peekable<Iter<Token>>,
) -> Result<Option<(String, StructMember)>, ParseError> {
    skip_whitespace(tokens);
    let name = match tokens.peek() {
        Some(Token::Identifier(name)) => name,
        _ => return Ok(None),
    };
    tokens.next();
    let optional = match tokens.peek() {
        Some(Token::QuestionMark) => {
            tokens.next();
            true
        }
        Some(Token::Colon) => false,
        Some(token) => {
            return Err(ParseError::unexpected_token(
                token,
                Some(vec![&Token::Colon, &Token::QuestionMark]),
            ))
        }
        None => return Err(ParseError::unexpected_end_of_input()),
    };
    skip_whitespace(tokens);
    expect(Token::Colon, tokens)?;
    skip_whitespace(tokens);
    let member_type = node_from_tokens(tokens)?;
    skip_whitespace(tokens);
    skip(Token::Comma, tokens);
    Ok(Some((
        name.clone(),
        StructMember {
            type_node: member_type,
            optional,
        },
    )))
}

// Parses this:
// array{ foo?: string, bar: string, ... }
//             ===========================
fn parse_struct(
    first_name: String,
    first_is_optional: bool,
    tokens: &mut Peekable<Iter<Token>>,
) -> Result<Node, ParseError> {
    skip_whitespace(tokens);
    let first_type = node_from_tokens(tokens)?;
    let mut members = HashMap::new();
    members.insert(
        first_name,
        StructMember {
            type_node: first_type,
            optional: first_is_optional,
        },
    );
    skip_whitespace(tokens);
    skip(Token::Comma, tokens);
    loop {
        match parse_struct_member(tokens)? {
            None => break,
            Some((name, member)) => {
                members.insert(name, member);
            }
        }
    }
    skip_whitespace(tokens);
    let other = match tokens.peek() {
        Some(Token::Ellipsis) => {
            tokens.next();
            skip_whitespace(tokens);
            let other_type_node = node_from_tokens(tokens);
            match other_type_node {
                Ok(other_type_node) => Some(Some(Box::new(other_type_node))),
                Err(_) => Some(None),
            }
        }
        _ => None,
    };
    skip_whitespace(tokens);
    expect(Token::CloseBrace, tokens)?;
    Ok(Node::Struct(members, other))
}

// Parses this:
// array{ ... }
//      =======
fn parse_braced_array(tokens: &mut Peekable<Iter<Token>>) -> Result<Node, ParseError> {
    skip(Token::OpenBrace, tokens);
    skip_whitespace(tokens);
    match tokens.peek() {
        None => return Err(ParseError::unexpected_end_of_input()),
        Some(Token::CloseBrace) => {
            tokens.next();
            return Ok(Node::Tuple(Vec::new()));
        }
        Some(token) => token,
    };
    let first_type = node_from_tokens(tokens)?;
    match first_type {
        Node::Identifier(name, params) => match params.is_empty() {
            true => match tokens.next() {
                None => Err(ParseError::unexpected_end_of_input()),
                Some(Token::Comma) => parse_tuple(Node::Identifier(name, params), tokens),
                Some(Token::QuestionMark) => {
                    expect(Token::Colon, tokens)?;
                    parse_struct(name, true, tokens)
                }
                Some(Token::Colon) => parse_struct(name, false, tokens),
                Some(Token::CloseBrace) => Ok(Node::Tuple(vec![Node::Identifier(name, params)])),
                Some(token) => Err(ParseError::unexpected_token(
                    token,
                    Some(vec![
                        &Token::Comma,
                        &Token::QuestionMark,
                        &Token::Colon,
                        &Token::CloseBrace,
                    ]),
                )),
            },
            false => parse_tuple(Node::Identifier(name, params), tokens),
        },
        _ => match tokens.next() {
            None => Err(ParseError::unexpected_end_of_input()),
            Some(token) => match token {
                Token::Comma => parse_tuple(first_type, tokens),
                Token::CloseBrace => Ok(Node::Tuple(vec![first_type])),
                _ => Err(ParseError::unexpected_token(
                    token,
                    Some(vec![&Token::Comma, &Token::CloseBrace]),
                )),
            },
        },
    }
}

// Parses this:
// Foo<string, int , float>
//            =====
fn parse_type_param(tokens: &mut Peekable<Iter<Token>>) -> Option<Node> {
    skip_whitespace(tokens);
    let node = match node_from_tokens(tokens) {
        Ok(type_param) => Some(type_param),
        Err(_) => None,
    };
    skip_whitespace(tokens);
    node
}

// Parses this:
// array<string, int>
//      =============
fn parse_type_params(tokens: &mut Tokens) -> Result<Vec<Node>, ParseError> {
    expect(Token::OpenAngle, tokens)?;
    let mut type_params = Vec::new();
    loop {
        match parse_type_param(tokens) {
            Some(type_param) => type_params.push(type_param),
            None => break,
        };
        match tokens.peek() {
            Some(Token::Comma) => {
                tokens.next();
                continue;
            }
            Some(Token::CloseAngle) => break,
            Some(token) => {
                return Err(ParseError::unexpected_token(
                    token,
                    Some(vec![&Token::Comma, &Token::CloseAngle]),
                ))
            }
            None => return Err(ParseError::unexpected_end_of_input()),
        }
    }
    expect(Token::CloseAngle, tokens)?;
    Ok(type_params)
}

// array<string, int>
// ==================
fn parse_identifier(tokens: &mut Peekable<Iter<Token>>) -> Result<Node, ParseError> {
    let name = match tokens.peek() {
        None => return Err(ParseError::unexpected_end_of_input()),
        Some(Token::Identifier(name)) => name.clone(),
        Some(token) => {
            return Err(ParseError::unexpected_token(
                token,
                Some(vec![&Token::Identifier("".to_string())]),
            ))
        }
    };
    tokens.next();
    if let Some(Token::OpenAngle) = tokens.peek() {
        return Ok(Node::Identifier(name, parse_type_params(tokens)?));
    }
    Ok(Node::Identifier(name, Vec::new()))
}

// array[...]
// ==========
fn parse_array(tokens: &mut Peekable<Iter<Token>>) -> Result<Node, ParseError> {
    skip(Token::Identifier("array".to_string()), tokens);
    let token = tokens.peek();
    match token {
        Some(Token::OpenAngle) => Ok(Node::Identifier(
            "array".to_string(),
            parse_type_params(tokens)?,
        )),
        Some(Token::OpenBrace) => parse_braced_array(tokens),
        _ => Ok(Node::Identifier("array".to_string(), Vec::new())),
    }
}

fn parse_integer(tokens: &mut Tokens) -> Result<isize, ParseError> {
    match tokens.next() {
        None => Err(ParseError::unexpected_end_of_input()),
        Some(Token::Integer(value)) => Ok(*value),
        Some(token) => Err(ParseError::unexpected_token(
            token,
            Some(vec![&Token::Integer(0)]),
        )),
    }
}

fn parse_union(left: Option<Node>, tokens: &mut Tokens) -> Result<Node, ParseError> {
    skip(Token::Pipe, tokens);
    skip_whitespace(tokens);
    match left {
        None => Err(ParseError::unexpected_token(&Token::Pipe, None)),
        Some(left) => Ok(Node::Union(
            Box::new(left),
            Box::new(node_from_tokens(tokens)?),
        )),
    }
}

fn parse_intersection(left: Option<Node>, tokens: &mut Tokens) -> Result<Node, ParseError> {
    skip(Token::Ampersand, tokens);
    skip_whitespace(tokens);
    match left {
        None => Err(ParseError::unexpected_token(&Token::Ampersand, None)),
        Some(left) => Ok(Node::Intersection(
            Box::new(left),
            Box::new(node_from_tokens(tokens)?),
        )),
    }
}

pub fn node_from_tokens(tokens: &mut Peekable<Iter<Token>>) -> Result<Node, ParseError> {
    skip_whitespace(tokens);
    let mut node: Option<Node> = None;
    loop {
        let token = match tokens.peek() {
            None => return Err(ParseError::unexpected_end_of_input()),
            Some(token) => token,
        };
        node = Some(match token {
            Token::Identifier(value) => match value.as_str() {
                "callable" => parse_callable(tokens)?,
                "array" => parse_array(tokens)?,
                _ => parse_identifier(tokens)?,
            },
            Token::Integer(value) => {
                tokens.next();
                Node::IntLiteral(*value)
            }
            Token::Minus => {
                tokens.next();
                Node::IntLiteral(-parse_integer(tokens)?)
            }
            Token::String(value) => {
                tokens.next();
                Node::StringLiteral((*value).clone())
            }
            Token::Pipe => parse_union(node, tokens)?,
            Token::Ampersand => parse_intersection(node, tokens)?,
            _ => {
                return Err(ParseError::unexpected_token(
                    token,
                    Some(vec![
                        &Token::Identifier("".to_string()),
                        &Token::Integer(0),
                        &Token::Minus,
                        &Token::String("".to_string()),
                        &Token::Pipe,
                        &Token::Ampersand,
                    ]),
                ))
            }
        });
        skip_whitespace(tokens);
        match tokens.peek() {
            None => return Ok(node.unwrap()),
            Some(token) => match token {
                Token::Pipe => continue,
                Token::Ampersand => continue,
                _ => return Ok(node.unwrap()),
            },
        }
    }
}

pub fn parse(type_string: &str) -> Result<Node, ParseError> {
    let tokens = lex(&mut type_string.chars().peekable());
    node_from_tokens(&mut tokens.iter().peekable())
}

pub trait ToNode {
    fn to_node(&self) -> Result<Node, ParseError>;
}

impl ToNode for str {
    fn to_node(&self) -> Result<Node, ParseError> {
        parse(self)
    }
}

impl ToNode for String {
    fn to_node(&self) -> Result<Node, ParseError> {
        parse(self)
    }
}

pub fn parse_type(str: &str, scope: &Scope) -> Result<Type, InvalidType> {
    let node = parse(str).map_err(|e| InvalidType::new(format!("{}", e)))?;
    Type::from_node(node, scope)
}
