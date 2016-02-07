//! 
//! The RuScript Runtime
//! 
//! ## Loading stage
//! loader read in bytes, deserialize them
//! into bytecodes, structure them into declarations
//! and top-level bytecodes. 
//! 
//! ## Execution stage
//! The executor will start from top-level bytecodes,
//! transfer control from and to new frames initialized
//! from global functions or object methods
//!
//! ## Code structure
//! * Data structure
//!   + Static: `class`, `function`
//!   + Dynamic: `primitives`, `instance`
//! * Bytecode
//!   + Format: `bytecode`
//!   + Deserialization: `deserialize`
//! * Object
//!   + Unified interface: `object`
//!   + Dynamic dispatch: `dispatch`
//! * Runtime
//!   + Loading: `env`
//!   + Execution: `interpret`
//!

#![crate_name = "ruscript"]

#![feature(plugin, custom_derive, custom_attribute, box_syntax, test)]
#![feature(convert)]
#![plugin(gc_plugin)]
extern crate gc;
extern crate test;

/// 32-bit Integer is the standard of bytecode and Int type
pub type Integer = i32;

pub mod bytecode;
pub mod deserialize;
pub mod class;
pub mod env;
pub mod object;
pub mod instance;
pub mod primitives;
pub mod dispatch;
pub mod interpret;
pub mod function;
pub mod cmdopt;
