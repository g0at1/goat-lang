#[derive(Debug, Clone)]
pub enum Expr {
    Bool(bool),
    Number(i64),
    Var(String),
    Equal(Box<Expr>, Box<Expr>),
    StringLiteral(String),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    PrintExpr(Expr),
    Assign(String, Expr),
    If { cond: Expr, body: Vec<Stmt> },
}
