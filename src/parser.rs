// expression -> equality
// equality -> comparison (( '==' | '!=' ) comparison)*
// comparison -> addition (( '>' | '>=' | '<' | '<=') addition)*
// addition -> multiplication (('+' | '-' ) multiplication)*
// multiplication -> unary (('*' | '/') unary)*
// unary -> ('!' | '-') unary
//          | primary
// primary -> literal | grouping
// literal -> Str | Number | Nil | True | False
// grouping -> '(' expression ')'

use crate::token::TokenType;
use crate::token::Token;
use crate::ast;
use std::slice;

pub struct Parser<'a> {
    pub tokens: slice::Iter<'a, Token>,
    has_error: bool,
}

#[derive(Debug)]
enum ParseError {
    MismatchedToken,
    UnexpectedToken,
}

impl<'a> Parser<'a> {
    pub fn new(tok_vec: &'a Vec<Token>) -> Self {
	Parser {
	    tokens: tok_vec.iter(),
	    has_error: false
	}
    }

    pub fn parse(&mut self) -> Option<Box<ast::Expr>> {
	if let Ok(expr) = self.expression() {
	    return Some(expr);
	}
	return None;
    }

    fn expression(&mut self) -> Result<Box<ast::Expr>, ParseError> {
	self.equality()
    }

    fn equality(&mut self) -> Result<Box<ast::Expr>, ParseError> {
	let mut expr = self.comparison()?;

	loop {
	    let tok = self.peek();
	    match &tok.token_type {
		TokenType::BangEqual |
		TokenType::EqualEqual => {
		    self.advance();
		    let rhs = self.comparison()?;
		    let expr_kind = ast::ExprKind::BinaryExpr(expr, tok, rhs);
		    expr = Box::new(ast::Expr{expr_kind});
		}
		_ => break,
	    }
	}
	Ok(expr)
    }

    fn comparison(&mut self) -> Result<Box<ast::Expr>, ParseError> {
	let mut expr = self.addition()?;

	loop {
	    let tok = self.peek();
	    match &tok.token_type {
		TokenType::Greater |
		TokenType::GreaterEqual |
		TokenType::Less |
		TokenType::LessEqual => {
		    self.advance();
		    let rhs = self.addition()?;
		    let expr_kind = ast::ExprKind::BinaryExpr(expr, tok, rhs);
		    expr = Box::new(ast::Expr{expr_kind});
		}
		_ => break,
	    }
	}
	Ok(expr)
    }

    fn addition(&mut self) -> Result<Box<ast::Expr>, ParseError> {
	let mut expr = self.multiplication()?;

	loop {
	    let tok = self.peek();
	    match &tok.token_type {
		TokenType::Plus |
		TokenType::Minus => {
		    self.advance();
		    let rhs = self.multiplication()?;
		    let expr_kind = ast::ExprKind::BinaryExpr(expr, tok, rhs);
		    expr = Box::new(ast::Expr{expr_kind});
		}
		_ => break,
	    }
	}
	Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Box<ast::Expr>, ParseError> {
	let mut expr = self.unary()?;

	loop {
	    let tok = self.peek();
	    match &tok.token_type {
		TokenType::Star |
		TokenType::Slash => {
		    self.advance();
		    let rhs = self.unary()?;
		    let expr_kind = ast::ExprKind::BinaryExpr(expr, tok, rhs);
		    expr = Box::new(ast::Expr{expr_kind});
		}
		_ => break,
	    }
	}
	Ok(expr)
    }

    fn unary(&mut self) -> Result<Box<ast::Expr>, ParseError> {
	let tok = self.peek();
	match &tok.token_type {
	    TokenType::Bang |
	    TokenType::Minus => {
		self.advance();
		let rhs = self.unary()?;
		let expr_kind = ast::ExprKind::UnaryExpr(tok, rhs);
		Ok(Box::new(ast::Expr{ expr_kind }))
	    }
	    _ => self.primary()
	}
    }

    fn primary(&mut self) -> Result<Box<ast::Expr>, ParseError> {
	let tok = self.peek();
	match &tok.token_type {
	    TokenType::LeftParen => {
		self.advance();
		let expr = self.expression()?;
		self.expect(TokenType::RightParen)?;
		let expr_kind = ast::ExprKind::ParenExpr(expr);
		Ok(Box::new(ast::Expr{ expr_kind }))
	    }
	    TokenType::Str{ val: v } => {
		self.advance();
		let lit = ast::Lit::Str(String::from(v));
		let expr_kind = ast::ExprKind::LitExpr(lit);
		Ok(Box::new(ast::Expr{ expr_kind }))
	    }
	    TokenType::Number{ val: v } => {
		self.advance();
		let lit = ast::Lit::Double(*v);
		let expr_kind = ast::ExprKind::LitExpr(lit);
 		Ok(Box::new(ast::Expr{ expr_kind }))
	    }
	    TokenType::Nil => {
		self.advance();
		let lit = ast::Lit::Nil;
		let expr_kind = ast::ExprKind::LitExpr(lit);
 		Ok(Box::new(ast::Expr{ expr_kind }))
	    }
	    TokenType::True => {
		self.advance();
		let lit = ast::Lit::Boolean(true);
		let expr_kind = ast::ExprKind::LitExpr(lit);
 		Ok(Box::new(ast::Expr{ expr_kind }))
	    }
	    TokenType::False => {
		self.advance();
		let lit = ast::Lit::Boolean(false);
		let expr_kind = ast::ExprKind::LitExpr(lit);
 		Ok(Box::new(ast::Expr{ expr_kind }))
	    }
	    _ => {
		let msg = format!( "unexpected token {:?}", tok.token_type);
		self.report_error(&tok, &msg);
		Err(ParseError::UnexpectedToken)
	    }
	}
    }

    fn advance(&mut self) -> Token {
	if let Some(t) = self.tokens.next() {
	    return t.clone();
	}
	Token::new(TokenType::Eof,1)
    }

    fn peek(&mut self) -> Token {
	if let Some(t) = self.tokens.clone().next() {
	    return t.clone();
	}
	Token::new(TokenType::Eof,1)
    }

    fn expect(&mut self, tok_type: TokenType) -> Result<(), ParseError> {
	let tok = self.peek();
	if tok.token_type == tok_type {
	    self.advance();
	    return Ok(());
	}
	let msg = format!( "mismatched token; expected {:?}, found {:?}",
			    tok_type,
			    tok.token_type);
	self.report_error(&tok, &msg);
	return Err(ParseError::MismatchedToken);
    }

    fn report_error(&mut self, tok: &Token, msg: &str) {
	self.has_error = true;
	let line =
	    if tok.token_type == TokenType::Eof {
		String::from( "<EOF>" )
	    } else {
		tok.line.to_string()
	    };
	println!("{}: error: {}", line, &msg);
    }
}
