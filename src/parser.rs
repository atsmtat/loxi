// program -> declaration* EOF
// declaration -> fun_decl
//              | var_decl
//              | statement
// var_decl -> 'var' IDENTIFIER ('=' expression)? ';'
//
// fun_decl -> 'fun' function
// function -> IDENTIFIER '(' parameters? ')' block
// parameters -> IDENTIFIER (',' IDENTIFIER)*
//
// statement -> expr_stmt
//            | print_stmt
//            | return_stmt
//            | if_stmt
//            | while_stmt
//            | for_stmt
//            | block
//
// expr_stmt -> expression ';'
// print_stmt -> Print expression ';'
// return_stmt -> Return expression? ';'
// if_stmt -> 'if' '(' expression ')' statement ('else' statement)?
// while_stmt -> 'while' '(' expression ')' statement
// for_stmt -> 'for' '(' (var_decl | expr_stmt | ';')
//                    expression? ';'
//                    expression? ')' statement
// block -> '{' declaration* '}'
//
// expression -> assignment
// assignment -> IDENTIFIER '=' assignment
//             | logic_or
//
// logic_or -> logic_and ('or' logic_and)*
// logic_and -> equality ('and' equality)*
// equality -> comparison (( '==' | '!=' ) comparison)*
// comparison -> addition (( '>' | '>=' | '<' | '<=') addition)*
// addition -> multiplication (('+' | '-' ) multiplication)*
// multiplication -> unary (('*' | '/') unary)*
// unary -> ('!' | '-') unary
//          | call
//
// call -> primary ( '(' arguments? ')' )*
// arguments -> expression ( ',' expression )*
// primary -> literal | grouping | IDENTIFIER
// literal -> STR | NUMBER | 'nil' | 'true' | 'false'
// grouping -> '(' expression ')'

use crate::ast;
use crate::token::Token;
use crate::token::TokenType;
use std::rc::Rc;
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
            has_error: false,
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
            TokenType::Fun => {
                // consume "fun"
                self.advance();
                self.function()
            }
            _ => self.statement(),
        }
    }

    fn ident(&mut self) -> Result<ast::Ident, ParseError> {
        let tok = self.peek();
        match tok.token_type {
            TokenType::Identifier(ref name) => {
                self.advance();
                Ok(ast::Ident::new(name, tok.clone()))
            }
            _ => {
                self.report_error(&tok, "mismatched token; expected identifier");
                Err(ParseError::MismatchedToken)
            }
        }
    }

    fn var_decl(&mut self) -> Result<Box<ast::Stmt>, ParseError> {
        let var_name = self.ident()?;

        let mut expr = None;
        if self.peek().token_type == TokenType::Equal {
            // consume '='
            self.advance();
            let expr_res = self.expression()?;
            expr = Some(expr_res);
        }

        self.expect(TokenType::Semicolon)?;
        let stmt_kind = ast::StmtKind::VarStmt(var_name, expr);
        Ok(Box::new(ast::Stmt { stmt_kind }))
    }

    fn function(&mut self) -> Result<Box<ast::Stmt>, ParseError> {
        let name = self.ident()?;
        self.expect(TokenType::LeftParen)?;

        let mut params = vec![];
        if self.peek().token_type != TokenType::RightParen {
            params = self.parameters()?;
        }

        self.expect(TokenType::RightParen)?;
        self.expect(TokenType::LeftBrace)?;

        let body = self.block_stmts()?;
        let fun_def = Rc::new(ast::FunDef { name, params, body });
        let stmt_kind = ast::StmtKind::FunStmt(fun_def);
        Ok(Box::new(ast::Stmt { stmt_kind }))
    }

    fn parameters(&mut self) -> Result<Vec<ast::Ident>, ParseError> {
        let mut params = vec![self.ident()?];
        while self.peek().token_type == TokenType::Comma {
            if params.len() >= 255 {
                let tok = self.peek();
                self.report_error(&tok, "cannot have more than 255 parameters");
            }

            // consume ','
            self.advance();
            params.push(self.ident()?);
        }
        Ok(params)
    }

    fn statement(&mut self) -> Result<Box<ast::Stmt>, ParseError> {
        let tok = self.peek();
        match &tok.token_type {
            TokenType::Print => {
                // consume "print"
                self.advance();

                self.print_stmt()
            }
            TokenType::Return => self.return_stmt(),
            TokenType::LeftBrace => {
                // consume '{'
                self.advance();
                self.block()
            }
            TokenType::If => {
                // consume 'if'
                self.advance();
                self.if_stmt()
            }
            TokenType::While => {
                // consume 'while'
                self.advance();
                self.while_stmt()
            }
            TokenType::For => {
                // consume 'for'
                self.advance();
                self.for_stmt()
            }
            _ => self.expr_stmt(),
        }
    }

    fn expr_stmt(&mut self) -> Result<Box<ast::Stmt>, ParseError> {
        let expr = self.expression()?;
        self.expect(TokenType::Semicolon)?;
        let stmt_kind = ast::StmtKind::ExprStmt(expr);
        Ok(Box::new(ast::Stmt { stmt_kind }))
    }

    fn print_stmt(&mut self) -> Result<Box<ast::Stmt>, ParseError> {
        let expr = self.expression()?;
        self.expect(TokenType::Semicolon)?;
        let stmt_kind = ast::StmtKind::PrintStmt(expr);
        Ok(Box::new(ast::Stmt { stmt_kind }))
    }

    fn return_stmt(&mut self) -> Result<Box<ast::Stmt>, ParseError> {
        let rtok = self.peek();
        // consume "return"
        self.advance();

        let mut expr = None;
        if self.peek().token_type != TokenType::Semicolon {
            expr = Some(self.expression()?);
        }
        self.expect(TokenType::Semicolon)?;
        let stmt_kind = ast::StmtKind::RetStmt(rtok, expr);
        Ok(Box::new(ast::Stmt { stmt_kind }))
    }

    fn if_stmt(&mut self) -> Result<Box<ast::Stmt>, ParseError> {
        self.expect(TokenType::LeftParen)?;
        let if_expr = self.expression()?;
        self.expect(TokenType::RightParen)?;
        let then_stmt = self.statement()?;

        let mut else_stmt = None;
        if self.peek().token_type == TokenType::Else {
            // consume "else"
            self.advance();
            else_stmt = Some(self.statement()?);
        }
        let stmt_kind = ast::StmtKind::IfStmt(if_expr, then_stmt, else_stmt);
        Ok(Box::new(ast::Stmt { stmt_kind }))
    }

    fn while_stmt(&mut self) -> Result<Box<ast::Stmt>, ParseError> {
        self.expect(TokenType::LeftParen)?;
        let expr = self.expression()?;
        self.expect(TokenType::RightParen)?;
        let stmt = self.statement()?;

        let stmt_kind = ast::StmtKind::WhileStmt(expr, stmt);
        Ok(Box::new(ast::Stmt { stmt_kind }))
    }

    fn for_stmt(&mut self) -> Result<Box<ast::Stmt>, ParseError> {
        self.expect(TokenType::LeftParen)?;

        let mut initializer = None;
        match self.peek().token_type {
            TokenType::Semicolon => {
                self.advance();
            }
            TokenType::Var => {
                // consume "var"
                self.advance();
                initializer = Some(self.var_decl()?);
            }
            _ => {
                initializer = Some(self.expr_stmt()?);
            }
        }

        let condition;
        if self.peek().token_type != TokenType::Semicolon {
            condition = self.expression()?;
        } else {
            let expr_kind = ast::ExprKind::LitExpr(ast::Lit::Boolean(true));
            condition = Box::new(ast::Expr { expr_kind });
        }
        self.expect(TokenType::Semicolon)?;

        let mut increment = None;
        if self.peek().token_type != TokenType::RightParen {
            let expr = self.expression()?;
            let stmt_kind = ast::StmtKind::ExprStmt(expr);
            increment = Some(Box::new(ast::Stmt { stmt_kind }));
        }
        self.expect(TokenType::RightParen)?;

        let mut body = self.statement()?;

        if let Some(increment) = increment {
            let stmt_kind = ast::StmtKind::BlockStmt(vec![body, increment]);
            body = Box::new(ast::Stmt { stmt_kind });
        }

        let stmt_kind = ast::StmtKind::WhileStmt(condition, body);
        body = Box::new(ast::Stmt { stmt_kind });

        if let Some(initializer) = initializer {
            let stmt_kind = ast::StmtKind::BlockStmt(vec![initializer, body]);
            body = Box::new(ast::Stmt { stmt_kind });
        }

        Ok(body)
    }

    fn block(&mut self) -> Result<Box<ast::Stmt>, ParseError> {
        let stmts = self.block_stmts()?;
        let stmt_kind = ast::StmtKind::BlockStmt(stmts);
        Ok(Box::new(ast::Stmt { stmt_kind }))
    }

    fn block_stmts(&mut self) -> Result<Vec<Box<ast::Stmt>>, ParseError> {
        let mut stmts = Vec::new();
        loop {
            let tok = self.peek();
            match tok.token_type {
                TokenType::RightBrace => {
                    // consume '}'
                    self.advance();
                    // end of block
                    break;
                }
                TokenType::Eof => {
                    self.report_error(&tok, "Expecting end of block '}'");
                    return Err(ParseError::UnexpectedToken);
                }
                _ => {
                    let stmt = self.declaration()?;
                    stmts.push(stmt);
                }
            }
        }
        Ok(stmts)
    }

    fn expression(&mut self) -> Result<Box<ast::Expr>, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Box<ast::Expr>, ParseError> {
        // Parse assignment target as if it's an expression, as target
        // could have a complex syntax which is already captured by expression,
        // Later, we'll check if it's a valid target.
        let expr = self.logical_or()?;
        match self.peek().token_type {
            TokenType::Equal => {
                // consume '='
                let eq_tok = self.advance();

                match expr.expr_kind {
                    ast::ExprKind::Variable(ident) => {
                        let rval = self.assignment()?;
                        let expr_kind = ast::ExprKind::Assign(ident, rval);
                        Ok(Box::new(ast::Expr { expr_kind }))
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

    fn logical_or(&mut self) -> Result<Box<ast::Expr>, ParseError> {
        let mut expr = self.logical_and()?;

        loop {
            let tok = self.peek();
            match &tok.token_type {
                TokenType::Or => {
                    self.advance();
                    let rhs = self.logical_and()?;
                    let expr_kind = ast::ExprKind::LogicalExpr(expr, tok, rhs);
                    expr = Box::new(ast::Expr { expr_kind });
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<Box<ast::Expr>, ParseError> {
        let mut expr = self.equality()?;

        loop {
            let tok = self.peek();
            match &tok.token_type {
                TokenType::And => {
                    self.advance();
                    let rhs = self.equality()?;
                    let expr_kind = ast::ExprKind::LogicalExpr(expr, tok, rhs);
                    expr = Box::new(ast::Expr { expr_kind });
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Box<ast::Expr>, ParseError> {
        let mut expr = self.comparison()?;

        loop {
            let tok = self.peek();
            match &tok.token_type {
                TokenType::BangEqual | TokenType::EqualEqual => {
                    self.advance();
                    let rhs = self.comparison()?;
                    let expr_kind = ast::ExprKind::BinaryExpr(expr, tok, rhs);
                    expr = Box::new(ast::Expr { expr_kind });
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
                TokenType::Greater
                | TokenType::GreaterEqual
                | TokenType::Less
                | TokenType::LessEqual => {
                    self.advance();
                    let rhs = self.addition()?;
                    let expr_kind = ast::ExprKind::BinaryExpr(expr, tok, rhs);
                    expr = Box::new(ast::Expr { expr_kind });
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
                TokenType::Plus | TokenType::Minus => {
                    self.advance();
                    let rhs = self.multiplication()?;
                    let expr_kind = ast::ExprKind::BinaryExpr(expr, tok, rhs);
                    expr = Box::new(ast::Expr { expr_kind });
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
                TokenType::Star | TokenType::Slash => {
                    self.advance();
                    let rhs = self.unary()?;
                    let expr_kind = ast::ExprKind::BinaryExpr(expr, tok, rhs);
                    expr = Box::new(ast::Expr { expr_kind });
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Box<ast::Expr>, ParseError> {
        let tok = self.peek();
        match &tok.token_type {
            TokenType::Bang | TokenType::Minus => {
                self.advance();
                let rhs = self.unary()?;
                let expr_kind = ast::ExprKind::UnaryExpr(tok, rhs);
                Ok(Box::new(ast::Expr { expr_kind }))
            }
            _ => self.call(),
        }
    }

    fn call(&mut self) -> Result<Box<ast::Expr>, ParseError> {
        let mut expr = self.primary()?;

        while self.peek().token_type == TokenType::LeftParen {
            // consume '('
            self.advance();

            let mut args = vec![];
            if self.peek().token_type != TokenType::RightParen {
                args = self.arguments()?;
            }
            let close_paren = self.peek();
            self.expect(TokenType::RightParen)?;
            let expr_kind = ast::ExprKind::CallExpr(expr, close_paren, args);
            expr = Box::new(ast::Expr { expr_kind });
        }
        Ok(expr)
    }

    fn arguments(&mut self) -> Result<Vec<Box<ast::Expr>>, ParseError> {
        let mut args = vec![self.expression()?];
        while self.peek().token_type == TokenType::Comma {
            if args.len() >= 255 {
                let tok = self.peek();
                self.report_error(&tok, "cannot have more than 255 arguments");
            }

            // consume ','
            self.advance();
            args.push(self.expression()?);
        }
        Ok(args)
    }

    fn primary(&mut self) -> Result<Box<ast::Expr>, ParseError> {
        let tok = self.peek();
        match &tok.token_type {
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                self.expect(TokenType::RightParen)?;
                let expr_kind = ast::ExprKind::ParenExpr(expr);
                Ok(Box::new(ast::Expr { expr_kind }))
            }
            TokenType::Str { val: v } => {
                self.advance();
                let lit = ast::Lit::Str(String::from(v));
                let expr_kind = ast::ExprKind::LitExpr(lit);
                Ok(Box::new(ast::Expr { expr_kind }))
            }
            TokenType::Number { val: v } => {
                self.advance();
                let lit = ast::Lit::Double(*v);
                let expr_kind = ast::ExprKind::LitExpr(lit);
                Ok(Box::new(ast::Expr { expr_kind }))
            }
            TokenType::Nil => {
                self.advance();
                let lit = ast::Lit::Nil;
                let expr_kind = ast::ExprKind::LitExpr(lit);
                Ok(Box::new(ast::Expr { expr_kind }))
            }
            TokenType::True => {
                self.advance();
                let lit = ast::Lit::Boolean(true);
                let expr_kind = ast::ExprKind::LitExpr(lit);
                Ok(Box::new(ast::Expr { expr_kind }))
            }
            TokenType::False => {
                self.advance();
                let lit = ast::Lit::Boolean(false);
                let expr_kind = ast::ExprKind::LitExpr(lit);
                Ok(Box::new(ast::Expr { expr_kind }))
            }
            TokenType::Identifier(ref name) => {
                self.advance();
                let expr_kind = ast::ExprKind::Variable(ast::Ident::new(name, tok.clone()));
                Ok(Box::new(ast::Expr { expr_kind }))
            }
            _ => {
                let msg = format!("unexpected token {:?}", tok.token_type);
                self.report_error(&tok, &msg);
                Err(ParseError::UnexpectedToken)
            }
        }
    }

    fn advance(&mut self) -> Token {
        if let Some(t) = self.tokens.next() {
            return t.clone();
        }
        Token::new(TokenType::Eof, 1)
    }

    fn peek(&mut self) -> Token {
        if let Some(t) = self.tokens.clone().next() {
            return t.clone();
        }
        Token::new(TokenType::Eof, 1)
    }

    fn expect(&mut self, tok_type: TokenType) -> Result<(), ParseError> {
        let tok = self.peek();
        if tok.token_type == tok_type {
            self.advance();
            return Ok(());
        }
        let msg = format!(
            "mismatched token; expected {:?}, found {:?}",
            tok_type, tok.token_type
        );
        self.report_error(&tok, &msg);
        return Err(ParseError::MismatchedToken);
    }

    fn report_error(&mut self, tok: &Token, msg: &str) {
        self.has_error = true;
        let line = if tok.token_type == TokenType::Eof {
            String::from("<EOF>")
        } else {
            tok.line.to_string()
        };
        println!("{}: error: {}", line, &msg);
    }
}
