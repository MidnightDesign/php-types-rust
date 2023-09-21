use std::fmt::{Debug, Display, Formatter};
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq)]
pub enum Token {
    String(String),
    Integer(isize),
    Whitespace,
    OpenAngle,
    CloseAngle,
    Colon,
    Comma,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Pipe,
    Ampersand,
    QuestionMark,
    Identifier(String),
    Equals,
    Minus,
}

impl Token {
    fn from_char(c: char) -> Option<Token> {
        match c {
            '<' => Some(Token::OpenAngle),
            '>' => Some(Token::CloseAngle),
            ':' => Some(Token::Colon),
            ',' => Some(Token::Comma),
            '(' => Some(Token::OpenParen),
            ')' => Some(Token::CloseParen),
            '{' => Some(Token::OpenBrace),
            '}' => Some(Token::CloseBrace),
            '|' => Some(Token::Pipe),
            '&' => Some(Token::Ampersand),
            '?' => Some(Token::QuestionMark),
            '=' => Some(Token::Equals),
            '-' => Some(Token::Minus),
            _ => None,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::String(string) => write!(f, "'{}'", string),
            Token::Integer(integer) => write!(f, "{}", integer),
            Token::Whitespace => write!(f, " "),
            Token::OpenAngle => write!(f, "<"),
            Token::CloseAngle => write!(f, ">"),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::OpenParen => write!(f, "("),
            Token::CloseParen => write!(f, ")"),
            Token::OpenBrace => write!(f, "{{"),
            Token::CloseBrace => write!(f, "}}"),
            Token::Pipe => write!(f, "|"),
            Token::Ampersand => write!(f, "&"),
            Token::QuestionMark => write!(f, "?"),
            Token::Identifier(identifier) => write!(f, "{}", identifier),
            Token::Equals => write!(f, "="),
            Token::Minus => write!(f, "-"),
        }
    }
}

impl Token {
    pub fn name(&self) -> &'static str {
        match self {
            Token::String(_) => "string",
            Token::Integer(_) => "integer",
            Token::Whitespace => "whitespace",
            Token::OpenAngle => "open angle bracket",
            Token::CloseAngle => "close angle bracket",
            Token::Colon => "colon",
            Token::Comma => "comma",
            Token::OpenParen => "open parenthesis",
            Token::CloseParen => "close parenthesis",
            Token::OpenBrace => "open curly brace",
            Token::CloseBrace => "close curly brace",
            Token::Pipe => "pipe",
            Token::Ampersand => "ampersand",
            Token::QuestionMark => "question mark",
            Token::Identifier(_) => "identifier",
            Token::Equals => "equals",
            Token::Minus => "minus",
        }
    }
}

fn parse_integer(chars: &mut Peekable<Chars>) -> Token {
    let mut integer = String::new();
    loop {
        match chars.peek() {
            None => break,
            Some(char) => {
                if char.is_numeric() {
                    integer.push(*char);
                    chars.next();
                } else {
                    break;
                }
            }
        }
    }
    Token::Integer(integer.parse().unwrap())
}

fn parse_whitespace(chars: &mut Peekable<Chars>) -> Token {
    loop {
        match chars.peek() {
            None => break,
            Some(char) => {
                if char.is_whitespace() {
                    chars.next();
                } else {
                    break;
                }
            }
        }
    }
    Token::Whitespace
}

fn parse_string(chars: &mut Peekable<Chars>) -> Token {
    let mut string = String::new();
    let quote = chars.next().unwrap();
    loop {
        match chars.peek() {
            None => break,
            Some(char) => {
                if char == &quote {
                    chars.next();
                    break;
                } else {
                    string.push(*char);
                    chars.next();
                }
            }
        }
    }
    Token::String(string)
}

const NON_IDENTIFIER_CHARS: [char; 12] =
    ['<', '>', ':', ',', '(', ')', '{', '}', '|', '&', '?', '='];

fn is_identifier_char(char: char) -> bool {
    !NON_IDENTIFIER_CHARS.contains(&char)
}

fn parse_identifier(chars: &mut Peekable<Chars>) -> Token {
    let mut identifier = String::new();
    loop {
        match chars.peek() {
            None => break,
            Some(char) => {
                if char.is_whitespace() || !is_identifier_char(*char) {
                    break;
                }
                identifier.push(*char);
                chars.next();
            }
        }
    }
    Token::Identifier(identifier)
}

pub fn lex(chars: &mut Peekable<Chars>) -> Vec<Token> {
    let mut tokens = Vec::new();
    loop {
        match chars.peek() {
            None => break,
            Some(char) if char.is_numeric() => {
                tokens.push(parse_integer(chars));
            }
            Some(char) if char.is_whitespace() => {
                tokens.push(parse_whitespace(chars));
            }
            Some(char) if char == &'"' || char == &'\'' => {
                tokens.push(parse_string(chars));
            }
            Some(char) => {
                if let Some(token) = Token::from_char(*char) {
                    tokens.push(token);
                    chars.next();
                } else {
                    tokens.push(parse_identifier(chars));
                }
            }
        }
    }
    tokens
}
