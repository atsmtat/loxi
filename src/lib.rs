pub mod scanner;
pub mod token;
pub mod ast;
pub mod parser;

pub fn print_ast_dummy() {
    let expr_kind = ast::ExprKind::LitExpr(ast::Lit::Double(4.2));
    let expr = Box::new(ast::Expr{ expr_kind });

    let expr_kind = ast::ExprKind::ParenExpr(expr);
    let expr = Box::new(ast::Expr{expr_kind});
    print_ast(&expr);
}

pub fn print_ast(root: & ast::Expr) {
    let mut printer = ast::AstPrinter::new(root);
    printer.print();
    println!("{}", &printer.ast_print);
}
