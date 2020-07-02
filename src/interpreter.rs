use std::collections::HashMap;
use crate::ast;
use crate::ast::{Expr, ExprKind, Stmt, StmtKind, Lit, Ident, Visitor};
use crate::token::{TokenType, Token};

#[derive(Clone, Debug)]
pub enum LoxType {
    Nil,
    Boolean(bool),
    Double(f64),
    Str(String),
}

#[derive(Clone, Debug)]
pub struct LoxValue {
    lox_type: LoxType,
}

impl LoxValue {
    fn is_number(&self) -> Option<f64> {
        match self.lox_type {
            LoxType::Double(num) => Some(num),
            _ => None,
        }
    }

    fn truth_value(&self) -> bool {
        match self.lox_type {
            LoxType::Nil => false,
            LoxType::Boolean(val) => val,
            _ => true,
        }
    }

}

pub enum RuntimeError {
    InvalidOperator,
    InvalidOperand,
    UndefinedVariable,
}

pub struct Environment {
    pub values: HashMap<String, LoxValue>,
}

impl Environment {
    pub fn new() -> Environment {
	Environment{ values : HashMap::new() }
    }

    pub fn define(&mut self, ident:&Ident, value:LoxValue) {
	self.values.insert(ident.name.clone(), value);
    }

    pub fn get(&self, ident:&Ident) -> Option<LoxValue> {
	if let Some(lv) = self.values.get(&ident.name) {
	    Some(lv.clone())
	} else {
	    None
	}
    }

    pub fn assign(&mut self, ident:&Ident, value:LoxValue) -> bool {
	if self.values.contains_key(&ident.name) {
	    self.values.insert(ident.name.clone(), value);
	    true
	} else {
	    false
	}
    }
}

pub struct Interpreter {
    pub has_error: bool,
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Interpreter {
	Interpreter{ has_error: false, environment: Environment::new(), }
    }

    pub fn run(&mut self, stmts:& Vec<Box<Stmt>>) {
	for stmt in stmts {
	    let res = self.visit_stmt( stmt );
	    match res {
		Err(_) => break,
		Ok(_) => {}
	    }
	}
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

    fn expect_number(&mut self, operand: &LoxValue, tok: &Token) -> Result<f64, RuntimeError> {
        if let Some(num) = operand.is_number() {
            Ok(num)
        } else {
            self.report_error(tok, "invalid operand; must be a number");
            Err(RuntimeError::InvalidOperand)
        }
    }

    fn evaluate(&mut self, expr:&Expr) -> Result<LoxValue, RuntimeError> {
	self.visit_expr( expr )
    }

    fn eval_binary_op(
        &mut self,
        left_val: &LoxValue,
        right_val: &LoxValue,
        op_token: &Token,
    ) -> Result<LoxValue, RuntimeError> {
        match op_token.token_type {
            TokenType::Plus => {
		match left_val.lox_type {
		    LoxType::Double(lnum) => {
			let rnum = self.expect_number(right_val, op_token)?;
			let lox_type = LoxType::Double(lnum + rnum);
			Ok(LoxValue{ lox_type })
		    }

		    LoxType::Str(ref lstr) => {
			if let LoxType::Str(ref rstr) = right_val.lox_type {
			    let lox_type = LoxType::Str( format!( "{}{}", lstr, rstr ) );
			    Ok(LoxValue{ lox_type })
			} else {
			    self.report_error(op_token, "invalid operand; must be a string");
			    Err(RuntimeError::InvalidOperand)
			}
		    }
		    _ => {
			self.report_error(op_token, "invalid operand; must be a number");
			Err(RuntimeError::InvalidOperand)
		    }
		}
            }
            TokenType::Minus => {
                let lnum = self.expect_number(left_val, op_token)?;
                let rnum = self.expect_number(right_val, op_token)?;
                let lox_type = LoxType::Double(lnum - rnum);
                Ok(LoxValue { lox_type })
            }
            TokenType::Star => {
                let lnum = self.expect_number(left_val, op_token)?;
                let rnum = self.expect_number(right_val, op_token)?;
                let lox_type = LoxType::Double(lnum * rnum);
                Ok(LoxValue { lox_type })
            }
            TokenType::Slash => {
                let lnum = self.expect_number(left_val, op_token)?;
                let rnum = self.expect_number(right_val, op_token)?;
                let lox_type = LoxType::Double(lnum / rnum);
                Ok(LoxValue { lox_type })
            }
            TokenType::Greater => {
                let lnum = self.expect_number(left_val, op_token)?;
                let rnum = self.expect_number(right_val, op_token)?;
                let lox_type = LoxType::Boolean(lnum > rnum);
                Ok(LoxValue { lox_type })
            }
            TokenType::GreaterEqual => {
                let lnum = self.expect_number(left_val, op_token)?;
                let rnum = self.expect_number(right_val, op_token)?;
                let lox_type = LoxType::Boolean(lnum >= rnum);
                Ok(LoxValue { lox_type })
            }
            TokenType::Less => {
                let lnum = self.expect_number(left_val, op_token)?;
                let rnum = self.expect_number(right_val, op_token)?;
                let lox_type = LoxType::Boolean(lnum < rnum);
                Ok(LoxValue { lox_type })
            }
            TokenType::LessEqual => {
                let lnum = self.expect_number(left_val, op_token)?;
                let rnum = self.expect_number(right_val, op_token)?;
                let lox_type = LoxType::Boolean(lnum <= rnum);
                Ok(LoxValue { lox_type })
            }
	    TokenType::BangEqual => {
		let lox_type = LoxType::Boolean(
		    left_val.truth_value() != right_val.truth_value());
		Ok(LoxValue {lox_type})
	    }
	    TokenType::EqualEqual => {
		let lox_type = LoxType::Boolean(
		    left_val.truth_value() == right_val.truth_value());
		Ok(LoxValue {lox_type})
	    }
            _ => {
                self.report_error(op_token, "invalid operator");
                Err(RuntimeError::InvalidOperator)
            }
        }
    }
}

impl ast::Visitor for Interpreter {
    type ExprRet = Result<LoxValue, RuntimeError>;
    type StmtRet = Result<(), RuntimeError>;

    fn visit_expr(&mut self, expr: &Expr) -> Self::ExprRet {
        match expr.expr_kind {
            ExprKind::LitExpr(ref lit) => {
                let lox_type = match lit {
                    Lit::Nil => LoxType::Nil,
                    Lit::Boolean(val) => LoxType::Boolean(*val),
                    Lit::Double(val) => LoxType::Double(*val),
                    Lit::Str(val) => LoxType::Str(String::from(val)),
                };
                Ok(LoxValue { lox_type })
            }

            ExprKind::ParenExpr(ref expr) => self.visit_expr(expr),

            ExprKind::UnaryExpr(ref tok, ref expr) => {
                let val = self.visit_expr(expr)?;
                match &tok.token_type {
                    TokenType::Minus => {
                        let num = self.expect_number(&val, tok)?;
                        let lox_type = LoxType::Double(-num);
                        Ok(LoxValue { lox_type })
                    }
                    TokenType::Bang => {
                        let lox_type = LoxType::Boolean(!val.truth_value());
                        Ok(LoxValue { lox_type })
                    }
                    _ => {
                        self.report_error(tok, "invalid operator");
                        Err(RuntimeError::InvalidOperator)
                    }
                }
            }

            ExprKind::BinaryExpr(ref lexpr, ref tok, ref rexpr) => {
                let left_val = self.visit_expr(lexpr)?;
                let right_val = self.visit_expr(rexpr)?;
                self.eval_binary_op(&left_val, &right_val, &tok)
            }

	    ExprKind::Variable(ref ident) => {
		let val = self.environment.get(ident);
		match val {
		    None => {
			let err_msg = format!( "undefined variable '{}'", &ident.name);
			self.report_error(&ident.tok, &err_msg);
			Err(RuntimeError::UndefinedVariable)
		    }
		    Some(v) => Ok(v),
		}
	    }

	    ExprKind::Assign(ref ident, ref expr) => {
		let rval = self.evaluate(expr)?;
		let ret_copy = rval.clone();
		if self.environment.assign(ident, rval) {
		    Ok(ret_copy)
		} else {
		    let err_msg = format!( "undefined variable '{}'", &ident.name);
		    self.report_error(&ident.tok, &err_msg);
		    Err(RuntimeError::UndefinedVariable)
		}
	    }
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::StmtRet {
	match stmt.stmt_kind {
	    StmtKind::ExprStmt(ref expr) => {
		self.visit_expr(expr)?;
		Ok(())
	    }

	    StmtKind::PrintStmt(ref expr) => {
		let val = self.visit_expr(expr)?;
		println!("{:?}", val);
		Ok(())
	    }

	    StmtKind::VarStmt(ref ident, ref initializer) => {
		let init_val;
		if let Some(ref init_expr) = initializer {
		    let val = self.evaluate(init_expr)?;
		    init_val = val;
		} else {
		    let lox_type = LoxType::Nil;
		    init_val = LoxValue{ lox_type };
		}
		self.environment.define(ident, init_val);
		Ok(())
	    }
	}
    }
}
