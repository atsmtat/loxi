use crate::token::Token;
use std::marker::Sized;
use std::fmt::Write;

pub struct Expr {
    pub expr_kind : ExprKind,
}

#[derive(Debug)]
pub enum Lit {
    Str(String),
    Double(f64),
}

pub enum ExprKind {
    // binary expr `left Op right`
    BinaryExpr(Box<Expr>, Token, Box<Expr>),

    // parenthesized expr `( exrp )`
    ParenExpr(Box<Expr>),

    // unary expr
    UnaryExpr(Token, Box<Expr>),

    // literal
    LitExpr(Lit)
}

pub trait Visitor : Sized {
    type VisRet : Default;
    fn visit_expr(&mut self, expr:&Expr) -> Self::VisRet {
	walk_expr(self, expr);
	Self::VisRet::default()
    }
}

pub fn walk_expr<V: Visitor>(visitor:&mut V, expr:&Expr) {
    match &expr.expr_kind {
	ExprKind::BinaryExpr(left, _, right) => {
	    visitor.visit_expr(left);
	    visitor.visit_expr(right);
	}

	ExprKind::ParenExpr(e) | ExprKind::UnaryExpr(_, e) => {
	    visitor.visit_expr(e);
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
    type VisRet = ();
    fn visit_expr(&mut self, expr:&Expr) -> Self::VisRet {
	self.ast_print.push( '(' );
	match &expr.expr_kind {
	    ExprKind::BinaryExpr(_, tok, _) => {
		write!(&mut self.ast_print, "{:?}", tok.token_type).unwrap();
	    }
	    ExprKind::ParenExpr(_) => self.ast_print.push_str("paren"),
	    ExprKind::UnaryExpr(tok, _) => {
		write!(&mut self.ast_print, "{:?}", tok.token_type).unwrap();
	    }
	    ExprKind::LitExpr(lit) => {
		write!(&mut self.ast_print, "{:?}", lit).unwrap();
	    }
	}
	walk_expr(self, expr);
	self.ast_print.push(')');
    }
}

