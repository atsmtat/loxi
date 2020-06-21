use crate::token::*;
use crate::token::TokenType::*;
use std::str::Chars;

pub struct Scanner<'a> {
    pub tokens: Vec<Token>,
    line_num: u32,
    iter: Chars<'a>,
    error: bool,
}

impl<'a> Scanner<'a> {
    pub fn new(src: &'a str) -> Self {
        Scanner {
            tokens: Vec::new(),
            line_num: 1,
            iter: src.chars(),
	    error: false,
        }
    }

    pub fn scan_tokens(&mut self) {
	while !self.is_eof() {
	    self.scan_token()
	}
	self.tokens.push(Token::new(Eof, self.line_num));
    }

    fn scan_token(&mut self) {
	let curr_char = self.advance().unwrap();
	let token_type = match curr_char {
	    '(' => LeftParen,
	    ')' => RightParen,
	    '{' => LeftBrace,
	    '}' => RightBrace,
	    ',' => Comma,
	    '.' => Dot,
	    '-' => Minus,
	    '+' => Plus,
	    ';' => Semicolon,
	    '*' => Star,

	    '!' => {
		if self.advance_if('=') {
		    BangEqual
		} else {
		    Bang
		}
	    },
	    '=' => {
		if self.advance_if('=')  {
		    EqualEqual
		} else {
		    Equal
		}
	    }
	    '>' => {
		if self.advance_if('=') {
		    GreaterEqual
		} else {
		    Greater
		}
	    }
	    '<' => {
		if self.advance_if('=') {
		    LessEqual
		} else {
		    Less
		}
	    }
	    '/' => {
		if self.advance_if('/') {
		    // comment
		    self.eat_comment();
		    WhiteSpace
		} else {
		    Slash
		}
	    }
	    ' ' | '\r' | '\t' => WhiteSpace,
	    '\n' => {
		self.line_num += 1;
		WhiteSpace
	    }
	    '"' => Str{ val: self.string_literal() },
	    n @ '0'..='9' => Number{ val: self.number_literal(n) },
	    c if Self::is_ident_start(c) => self.identifier(c),
	    _ => Unknown,
	};

	match token_type {
	    Unknown => {
		self.report_error(format!("Unknown character {}", curr_char).as_str());
	    }

	    WhiteSpace => {},
	    _ => self.tokens.push(Token::new(token_type, self.line_num)),
	}
    }

    // consumes next char and returns it
    fn advance(&mut self) -> Option<char> {
	self.iter.next()
    }

    // peeks next char without consuming it, and returns it
    fn peek(&mut self) -> Option<char> {
	if self.is_eof() {
	    return None;
	}
	self.iter.clone().next()
    }

    // peeks next to next char without consuming it, and returns it
    fn peek_next(&mut self) -> Option<char> {
	if self.is_eof() {
	    return None;
	}
	self.iter.clone().nth(1)
    }

    // advance if next char matches with a given char c
    fn advance_if(&mut self, c: char) -> bool {
	if let Some(nc) = self.peek() {
	    if nc == c {
		self.advance();
		true
	    } else {
		false
	    }
	} else {
	    false
	}
    }

    fn is_eof(&self) -> bool {
	self.iter.as_str().is_empty()
    }

    // eat chars till newline '\n' or eof is hit
    fn eat_comment(&mut self) {
	while let Some(nc) = self.peek() {
	    if nc != '\n' {
		self.advance();
	    } else {
		break;
	    }
	}
    }

    fn string_literal(&mut self) -> String {
	let mut result = String::new();

	let mut next_char = self.peek();
	let mut terminated = false;

	while let Some(c) = next_char {
	    self.advance();
	    match c {
		'"' => {
		    terminated = true;
		    break;
		}
		'\n' => {
		    result.push(c);
		    self.line_num += 1;
		}
		_ => {
		    result.push(c);
		}
	    }
	    next_char = self.peek();
	}

	if !terminated {
	    self.report_error("Unterminated string");
	}
	result
    }

    fn number_literal(&mut self, first_char: char) -> f64 {
	let mut result = String::new();
	result.push(first_char);

	while let Some(next_char) = self.peek() {
	    if next_char.is_digit(10) {
		result.push( next_char );
		self.advance();
	    } else {
		break;
	    }
	}

	if let Some(nc @ '.') = self.peek() {
	    // check the char next to '.'; if it's a digit,
	    // we continue parsing number; else we return.
	    if let Some(nnc) = self.peek_next() {
		if nnc.is_digit(10) {
		    result.push(nc);
		    self.advance();
		    
		    while let Some(next_char) = self.peek() {
			if next_char.is_digit(10) {
			    result.push(next_char);
			    self.advance();
			} else {
			    break;
			}
		    }
		}
	    }
	    
	}
	return result.parse::<f64>().unwrap();
    }

    fn is_ident_start(c: char) -> bool {
	( 'a' <= c && c <= 'z' )
	    || ( 'A' <= c && c <= 'Z')
	    || c == '_'
    }

    fn is_ident_continue(c: char) -> bool {
	Self::is_ident_start(c) ||
	    c.is_digit(10)
    }

    fn identifier(&mut self, first_char: char) -> TokenType {
	let mut ident = String::new();
	ident.push(first_char);
	while let Some(c) = self.peek() {
	    if Self::is_ident_continue(c) {
		self.advance();
		ident.push(c);
	    } else {
		break;
	    }
	}

	match ident.as_str() {
	    "and" => And,
	    "class" => Class,
	    "else" => Else,
	    "false" => False,
	    "fun" => Fun,
	    "for" => For,
	    "if" => If,
	    "nil" => Nil,
	    "or" => Or,
	    "print" => Print,
	    "return" => Return,
	    "super" => Super,
	    "this" => This,
	    "true" => True,
	    "var" => Var,
	    "while" => While,
	    _ => Identifier{ ident }
	}
    }

    fn report_error(&mut self, msg: &str) {
	self.error = true;
	println!("{}: error: {}", self.line_num, msg);
    }
}
