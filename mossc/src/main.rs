
use std::fs::File;
use std::io::BufReader;
use std::io::Read;
use std::env;

mod parser;
mod typing;

use parser::{parse,Error};
use typing::{type_check,Env,SymbolTable};

fn print_error(e: &Error, file: &str) {
    print!("File '{}', line {}, col {}:\n",file,e.line+1,e.col+1);
    println!("{}",e.text);
}

fn compile(s: &str, file: &str) -> Result<(),()> {
    let t = match parse(s) {
        Ok(t) => t,
        Err(e) => {
            print_error(&e,file);
            return Err(());
        }
    };
    let env = Env::new();
    let mut symbol_table = SymbolTable::new();
    match type_check(&env,&t,&mut symbol_table) {
        Ok(()) => {},
        Err(e) => {
            e.print();
        }
    }
    return Ok(());
}

fn read_file(path: &str) -> Result<String,std::io::Error> {
    let file = File::open(path)?;
    let mut buf_reader = BufReader::new(file);
    let mut s = String::new();
    buf_reader.read_to_string(&mut s)?;
    return Ok(s);
}

struct CmdInfo {
    path: Option<String>
}

impl CmdInfo {
    pub fn new() -> Self {
        let mut info = CmdInfo{path: None};
        let mut first = true;
        for s in env::args() {
            if first {first = false; continue;}
            info.path = Some(String::from(s));
        }
        return info;
    }
}

static HELP_MESSAGE: &str = "
Usage: mossc file
";

fn main_result() -> Result<(),()> {
    let info = CmdInfo::new();
    if let Some(path) = info.path {
        let s = match read_file(&path) {
            Ok(s) => s,
            Err(_) => {
                println!("Error: Could not open file '{}'.", path);
                return Err(());
            }
        };
        compile(&s,&path)?;
    }else{
        println!("{}",HELP_MESSAGE);
        return Err(());
    }
    return Ok(());
}

fn main()  {
    let result = match main_result() {Ok(())=>0, Err(())=>1};
    std::process::exit(result);
}