use gc::*;

#[derive(Debug, Trace)]
pub enum Factor {
    Int(i32),
    Var(String),
    Single(Box<Expr>),
}

#[derive(Debug, Trace)]
pub enum Term {
    Multi(Factor, Factor),
    Single(Factor)
}

#[derive(Debug, Trace)]
pub enum Expr {
    Plus(Term, Term),
    Single(Term),
}

#[derive(Debug, Trace)]
pub enum Statement {
    Assign(String, Expr),
    Aliasing(String, String),
}