#![crate_name = "ruscript"]

#![feature(plugin, custom_derive, custom_attribute)]
#![feature(box_syntax)]

#![plugin(gc_plugin)]
extern crate gc;

#[macro_use]
// extern crate nom;

// use nom::{IResult, digit, multispace};

// use std::str;
// use std::str::FromStr;

// use std::collections::HashMap;
// use std::cell::{self, Cell, RefCell, BorrowState};
// use std::ptr;
// use std::thread;
// use std::mem;
// use gc::Gc;

pub mod object;
pub mod ast;
pub mod interpreter;

pub use ast::*;
