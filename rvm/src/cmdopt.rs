//! 
//! Command line Option
//!

pub struct CmdOpt {
    pub debug : bool,
}

pub fn default_cmdopt() -> CmdOpt {
    CmdOpt {
        debug : false,
    }
}