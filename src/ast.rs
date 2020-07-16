use crate::token::Token;
use std::marker::Sized;
use std::fmt::Write;

pub struct Expr {
    pub expr_kind : ExprKind,
}

#[derive(Debug)]
pub enum Lit {
    Nil,
    Boolean(bool),
    Double(f64),
    Str(String),
}

pub struct Ident {
    pub name: String,
    pub tok : Token,
}

impl Ident {
    pub fn new(nm: &str, tok: Token) -> Ident {
	Ident{ name: nm.to_string(), tok}
    }
}

pub enum ExprKind {
    // assignment
    Assign(Ident, Box<Expr>),
    
    // binary expr `left Op right`
    BinaryExpr(Box<Expr>, Token, Box<Expr>),

    // logical expr `left and/or right`
    LogicalExpr(Box<Expr>, Token, Box<Expr>),

    // parenthesized expr `( exrp )`
    ParenExpr(Box<Expr>),

    // unary expr
    UnaryExpr(Token, Box<Expr>),

    // literal
    LitExpr(Lit),

    // variable
    Variable(Ident),

    // callee, closed paren token, args
    CallExpr(Box<Expr>, Token, Vec<Box<Expr>>),
}

pub struct Stmt {
    pub stmt_kind : StmtKind,
}

pub enum StmtKind {
    ExprStmt(Box<Expr>),
    PrintStmt(Box<Expr>),
    VarStmt(Ident, Option<Box<Expr>>),
    BlockStmt(Vec<Box<Stmt>>),

    // if expr, then statement, optional else statement
    IfStmt(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),

    // while condition, body
    WhileStmt(Box<Expr>, Box<Stmt>),

    // function name, params, body
    FunStmt(Ident, Vec<Ident>, Vec<Box<Stmt>>),
}

pub trait Visitor : Sized {
    type ExprRet;
    type StmtRet;
    fn visit_expr(&mut self, expr:&Expr) -> Self::ExprRet;
    fn visit_stmt(&mut self, stmt:&Stmt) -> Self::StmtRet;
}

pub fn walk_expr<V: Visitor>(visitor:&mut V, expr:&Expr) {
    match &expr.expr_kind {
	ExprKind::Assign(_, expr) => {
	    visitor.visit_expr(expr);
	}
	ExprKind::BinaryExpr(left, _, right) => {
	    visitor.visit_expr(left);
	    visitor.visit_expr(right);
	}
	ExprKind::LogicalExpr(left, _, right) => {
	    visitor.visit_expr(left);
	    visitor.visit_expr(right);
	}
	ExprKind::ParenExpr(e) |
	ExprKind::UnaryExpr(_, e) => {
	    visitor.visit_expr(e);
	}
	ExprKind::CallExpr(ref callee, _, ref args) => {
	    visitor.visit_expr(callee);
	    for arg in args {
		visitor.visit_expr(arg);
	    }
	}
	_ => {}
    }
}

pub fn walk_stmt<V: Visitor>(visitor:&mut V, stmt:&Stmt) {
    match stmt.stmt_kind {
	StmtKind::ExprStmt(ref expr) => {
	    visitor.visit_expr(expr);
	}
	StmtKind::PrintStmt(ref expr) => {
	    visitor.visit_expr(expr);
	}
	StmtKind::VarStmt(_, ref initializer) => {
	    if let Some(expr) = initializer {
		visitor.visit_expr(expr);
	    }
	}
	StmtKind::BlockStmt(ref stmts) => {
	    for stmt in stmts {
		visitor.visit_stmt(stmt);
	    }
	}
	_ => {}
    }
}

pub struct AstPrinter<'ast> {
    root : &'ast Expr,
    pub ast_print : String,
}

impl<'ast> AstPrinter<'ast> {
    pub fn new(root: &'ast Expr) -> Self {
	AstPrinter{
	    root,
	    ast_print: String::new(),
	}
    }

    pub fn print(&mut self) {
	self.visit_expr(self.root);
    }
}

impl<'ast> Visitor for AstPrinter<'ast> {
    type ExprRet = ();
    type StmtRet = ();
    fn visit_expr(&mut self, expr:&Expr) -> Self::ExprRet {
	self.ast_print.push( '(' );
	match &expr.expr_kind {
	    ExprKind::Assign(ident, _) => {
		write!(&mut self.ast_print, "assignment to '{}'", ident.name).unwrap();
	    }
	    ExprKind::BinaryExpr(_, tok, _) => {
		write!(&mut self.ast_print, "{:?}", tok.token_type).unwrap();
	    }
	    ExprKind::LogicalExpr(_, tok, _) => {
		write!(&mut self.ast_print, "{:?}", tok.token_type).unwrap();
	    }
	    ExprKind::ParenExpr(_) => self.ast_print.push_str("paren"),
	    ExprKind::UnaryExpr(tok, _) => {
		write!(&mut self.ast_print, "{:?}", tok.token_type).unwrap();
	    }
	    ExprKind::LitExpr(lit) => {
		write!(&mut self.ast_print, "{:?}", lit).unwrap();
	    }
	    ExprKind::Variable(ident) => {
		write!(&mut self.ast_print, "{:?}", &ident.name).unwrap();
	    }
	    ExprKind::CallExpr(_,_,_) => { self.ast_print.push_str("call expr"); }
	}
	walk_expr(self, expr);
	self.ast_print.push(')');
    }

    fn visit_stmt(&mut self, stmt:&Stmt) -> Self::StmtRet {
	self.ast_print.push( '(' );
	match stmt.stmt_kind {
	    StmtKind::ExprStmt(_) => self.ast_print.push_str("expr stmt"),
	    StmtKind::PrintStmt(_) => self.ast_print.push_str("print stmt"),
	    StmtKind::VarStmt(_, _) => self.ast_print.push_str("variable stmt"),
	    StmtKind::BlockStmt(_) => self.ast_print.push_str("block stmt"),
	    StmtKind::IfStmt(_,_,_) => self.ast_print.push_str("if stmt"),
	    StmtKind::WhileStmt(_,_) => self.ast_print.push_str("while stmt"),
	    StmtKind::FunStmt(_,_,_) => self.ast_print.push_str("function stmt"),
	}
	walk_stmt(self, stmt);
	self.ast_print.push(')');
    }
}

