use std::collections::HashMap;
use std::collections::LinkedList;
use std::rc::Rc;
use crate::ast;
use crate::ast::{Expr, ExprKind, Stmt, StmtKind, Lit, Ident, Visitor, FunDef};
use crate::token::{TokenType, Token};

#[derive(Clone, Debug)]
enum LoxType {
    Nil,
    Boolean(bool),
    Double(f64),
    Str(String),
    Fun(Rc<FunDef>),
}

trait LoxCallable {
    fn call(&self, interpreter: &mut Interpreter, args: &Vec<LoxValue>) -> Result<LoxValue, RuntimeError>;
    fn arity(&self) -> usize;
}

impl LoxCallable for FunDef {
    fn call(&self, interpreter: &mut Interpreter, args: &Vec<LoxValue>) -> Result<LoxValue, RuntimeError> {
	interpreter.push_environment();
	for i in 0..self.params.len() {
	    interpreter.define(&self.params[i], args[i].clone());
	}

	let mut ret_val = LoxValue{lox_type: LoxType::Nil};
	interpreter.execute_block(&self.body)
	    .unwrap_or_else(|err| {
		match err {
		    RuntimeError::FunctionReturn(rv) => { ret_val = rv }
		    _ => {}
		}
	    });
	interpreter.pop_environment();
	Ok(ret_val)
    }

    fn arity(&self) -> usize {
	self.params.len()
    }
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
    FunctionReturn(LoxValue),
}

struct Environment {
    pub values: HashMap<String, LoxValue>,
}

impl Environment {
    fn new() -> Self {
	Environment{ values : HashMap::new() }
    }

    fn define(&mut self, ident:&Ident, value:LoxValue) {
	self.values.insert(ident.name.clone(), value);
    }

    fn get(&self, ident:&Ident) -> Option<LoxValue> {
	if let Some(lv) = self.values.get(&ident.name) {
	    Some(lv.clone())
	} else {
	    None
	}
    }

    fn assign(&mut self, ident:&Ident, value:&LoxValue) -> bool {
	if self.values.contains_key(&ident.name) {
	    self.values.insert(ident.name.clone(), value.clone());
	    true
	} else {
	    false
	}
    }
}

pub struct Interpreter {
    pub has_error: bool,
    environments: LinkedList<Environment>,
}

impl Interpreter {
    pub fn new() -> Self {
	Interpreter{ has_error: false, environments: LinkedList::new(), }
    }

    pub fn run(&mut self, stmts:& Vec<Box<Stmt>>) {
	// create global environment
	self.push_environment();
	for stmt in stmts {
	    let res = self.visit_stmt( stmt );
	    match res {
		Err(_) => break,
		Ok(_) => {}
	    }
	}
    }

    fn push_environment(&mut self) {
	self.environments.push_front(Environment::new());
    }

    fn pop_environment(&mut self) {
	self.environments.pop_front();
    }

    fn define(&mut self, ident:&Ident, value:LoxValue) {
	if let Some(curr_env) = self.environments.front_mut() {
	    curr_env.define(ident, value);
	} else {
	    panic!("environment missing");
	}
    }

    fn get(&mut self, ident:&Ident) -> Result<LoxValue, RuntimeError> {
	// search for identifier, starting from inner most environment,
	// following up the chain of enclosing environments
	for env in self.environments.iter() {
	    if let Some(val) = env.get(ident) {
		return Ok(val);
	    }
	}
	let err_msg = format!( "undefined variable '{}'", &ident.name);
	self.report_error(&ident.tok, &err_msg);
	return Err(RuntimeError::UndefinedVariable);
    }

    fn assign(&mut self, ident:&Ident, value:&LoxValue) -> Result<(), RuntimeError> {
	for env in self.environments.iter_mut() {
	    if env.assign(ident, value) {
		return Ok(());
	    }
	}
	let err_msg = format!( "undefined variable '{}'", &ident.name);
	self.report_error(&ident.tok, &err_msg);
	return Err(RuntimeError::UndefinedVariable);
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

    fn execute(&mut self, stmt:&Stmt) -> Result<(), RuntimeError> {
	self.visit_stmt( stmt )
    }

    fn execute_block(&mut self, stmts:&Vec<Box<Stmt>>) -> Result<(), RuntimeError> {
	let mut result = Ok(());
	for stmt in stmts {
	    result = self.execute(stmt);
	    match result {
		Err(_) => break,
		Ok(_) => {}
	    }
	}
	result
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

	    ExprKind::LogicalExpr(ref lexpr, ref tok, ref rexpr) => {
                let left_val = self.visit_expr(lexpr)?;
		match &tok.token_type {
		    TokenType::Or => {
			if left_val.truth_value() {
			    return Ok(left_val);
			}
		    }
		    TokenType::And => {
			if !left_val.truth_value() {
			    return Ok(left_val);
			}
		    }
		    _ => { panic!( "unexpected logical operand {:?}", tok); }
		}
		self.visit_expr(rexpr)
            }

	    ExprKind::Variable(ref ident) => {
		self.get(ident)
	    }

	    ExprKind::Assign(ref ident, ref expr) => {
		let rval = self.evaluate(expr)?;
		self.assign(ident, &rval)?;
		Ok(rval)
	    }

	    ExprKind::CallExpr(ref callee_expr, ref paren_tok, ref arg_exprs) => {
		let callee = self.evaluate( callee_expr )?;
		match callee.lox_type {
		    LoxType::Fun(lox_fun) => {
			let mut args = vec![];
			for ae in arg_exprs {
			    args.push(self.evaluate(ae)?);
			}

			if lox_fun.arity() != args.len() {
			    let err = format!( "Expected {} arguments, but got {}",
						lox_fun.arity(),
						args.len() );
			    self.report_error(paren_tok, &err );
			    return Err(RuntimeError::InvalidOperand);
			}

			lox_fun.call(self, &args)
		    }
		    _ => {
			self.report_error(paren_tok, "Can only call functions and classes");
			Err(RuntimeError::InvalidOperator)
		    }
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
		self.define(ident, init_val);
		Ok(())
	    }

	    StmtKind::IfStmt(ref expr, ref then_stmt, ref else_stmt) => {
		let val = self.evaluate(expr)?;
		if val.truth_value() {
		    self.execute(then_stmt)?;
		} else if let Some(es) = else_stmt {
		    self.execute(es)?;
		}
		Ok(())
	    }

	    StmtKind::WhileStmt(ref condition, ref body) => {
		while self.evaluate(condition)?.truth_value() {
		    self.execute(body)?;
		}
		Ok(())
	    }

	    StmtKind::BlockStmt(ref stmts) => {
		self.push_environment();
		let result = self.execute_block(stmts);
		self.pop_environment();
		result
	    }

	    StmtKind::FunStmt(ref fdef) => {
		let lox_val = LoxValue{lox_type: LoxType::Fun(fdef.clone())};
		self.define(&fdef.name, lox_val);
		Ok(())
	    }

	    StmtKind::RetStmt(_, ref ret_expr) => {
		let ret_val = match ret_expr {
		    Some(ref re) => {
			self.evaluate(re)?
		    }
		    None => {
			LoxValue{lox_type: LoxType::Nil}
		    }
		};
		Err(RuntimeError::FunctionReturn(ret_val))
	    }
	}
    }
}
