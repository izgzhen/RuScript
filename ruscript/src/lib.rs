#![crate_name = "ruscript"]

#![feature(plugin, custom_derive)]

#![plugin(gc_plugin)]
extern crate gc;

#[macro_use]
// extern crate nom;

// use nom::{IResult, digit, multispace};

// use std::str;
// use std::str::FromStr;


// fn parse(s : &str) -> statement {

// }


#[derive(Debug)]
enum Factor {
    Int(i32),
    Var(&'static str),
    Single(Box<Expr>),
}

#[derive(Debug)]
enum Term {
    Multi(Factor, Factor),
    Single(Factor)
}

#[derive(Debug)]
enum Expr {
    Plus(Term, Term),
    Single(Term),
}

#[derive(Debug)]
enum Statement {
    Assign(&'static str, Expr),
}

pub fn main() {
    let stat = Statement::Assign("a", Expr::Single(Term::Single(Factor::Int(1))));
    println!("{:?}", stat);
}

