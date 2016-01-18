/* 
 * Runtime Environment
 *
 */

use class::Class;
use bytecode::ByteCode;

pub struct Env {
    pub classes   : Vec<Class>,
    pub functions : Vec<Vec<ByteCode>>,
}