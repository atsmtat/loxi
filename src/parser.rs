// program -> declaration* EOF
// declaration -> var_decl
//              | statement
// var_decl -> 'var' IDENTIFIER ('=' expression)? ';'
//
// statement -> expr_stmt
//            | print_stmt
// expr_stmt -> expression ';'
// print_stmt -> Print expression ';'
//
// expression -> assignment
// assignment -> IDENTIFIER '=' assignment
//             | equality
// equality -> comparison (( '==' | '!=' ) comparison)*
// comparison -> addition (( '>' | '>=' | '<' | '<=') addition)*
// addition -> multiplication (('+' | '-' ) multiplication)*
// multiplication -> unary (('*' | '/') unary)*
// unary -> ('!' | '-') unary
//          | primary
//
// primary -> literal | grouping | IDENTIFIER
// literal -> STR | NUMBER | 'nil' | 'true' | 'false'
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
    InvalidAssignmentTarget,
}

impl<'a> Parser<'a> {
    pub fn new(tok_vec: &'a Vec<Token>) -> Self {
	Parser {
	    tokens: tok_vec.iter(),
	    has_error: false
	}
    }

    pub fn parse(&mut self) -> Option<Vec<Box<ast::Stmt>>> {
	if let Ok(stmts) = self.program() {
	    return Some(stmts);
	}
	return None;
    }

    fn program(&mut self) -> Result<Vec<Box<ast::Stmt>>, ParseError> {
	let mut stmts = Vec::new();
	while self.peek().token_type != TokenType::Eof {
	    let stmt = self.declaration()?;
	    stmts.push(stmt);
	}
	Ok(stmts)
    }

    fn declaration(&mut self) -> Result<Box<ast::Stmt>, ParseError> {
	let tok = self.peek();
	match &tok.token_type {
	    TokenType::Var => {
		// consume "var"
		self.advance();
		self.var_decl()
	    }
	    _ => self.statement(),
	}
    }

    fn var_decl(&mut self) ->  Result<Box<ast::Stmt>, ParseError> {
	let tok = self.peek();
	let ident_name;
	match &tok.token_type {
	    TokenType::Identifier(ref name) => {
		self.advance();
		ident_name = name.to_string();
	    },
	    _ => {
		self.report_error(&tok,  "mismatched token; expected identifier");
		return Err(ParseError::MismatchedToken);
	    }
	}

	let mut expr = None;
	if self.peek().token_type == TokenType::Equal {
	    // consume '='
	    self.advance();
	    let expr_res = self.expression()?;
	    expr = Some(expr_res);
	}

	self.expect(TokenType::Semicolon)?;
	let ident = ast::Ident::new( &ident_name, tok );
	let stmt_kind = ast::StmtKind::VarStmt(ident, expr);
	Ok(Box::new(ast::Stmt { stmt_kind }))
    }

    fn statement(&mut self) -> Result<Box<ast::Stmt>, ParseError> {
	let tok = self.peek();
	match &tok.token_type {
	    TokenType::Print => {
		// consume "print"
		self.advance();

		self.print_stmt()
	    }
	    _ => self.expr_stmt(),

	}
    }

    fn expr_stmt(&mut self) ->  Result<Box<ast::Stmt>, ParseError> {
	let expr = self.expression()?;
	self.expect(TokenType::Semicolon)?;
	let stmt_kind = ast::StmtKind::ExprStmt(expr);
	Ok(Box::new(ast::Stmt { stmt_kind }))
    }

    fn print_stmt(&mut self) ->  Result<Box<ast::Stmt>, ParseError> {
	let expr = self.expression()?;
	self.expect(TokenType::Semicolon)?;
	let stmt_kind = ast::StmtKind::PrintStmt(expr);
	Ok(Box::new(ast::Stmt { stmt_kind }))
    }

    fn expression(&mut self) -> Result<Box<ast::Expr>, ParseError> {
	self.assignment()
    }

    fn assignment(&mut self) -> Result<Box<ast::Expr>, ParseError> {
	// Parse assignment target as if it's an expression, as target
	// could have a complex syntax which is already captured by expression,
	// Later, we'll check if it's a valid target.
	let expr = self.equality()?;
	match self.peek().token_type {
	    TokenType::Equal => {
		// consume '='
		let eq_tok = self.advance();

		match expr.expr_kind {
		    ast::ExprKind::Variable(ident) => {
			let rval = self.assignment()?;
			let expr_kind = ast::ExprKind::Assign(ident, rval);
			Ok(Box::new(ast::Expr{expr_kind}))
		    }
		    _ => {
			self.report_error(&eq_tok, "Invalid assignment target");
			Err(ParseError::InvalidAssignmentTarget)
		    }
		}
	    }
	    _ => Ok(expr),
	}
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
	    TokenType::Identifier(ref name) => {
		self.advance();
		let expr_kind = ast::ExprKind::Variable(
		    ast::Ident::new(name, tok.clone()) );
		Ok(Box::new(ast::Expr { expr_kind }))
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
