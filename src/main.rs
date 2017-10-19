
#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_mut)]

#[path = "compiler/compiler.rs"]
mod compiler;

#[path = "system/system.rs"]
mod system;

use std::fs::File;
use std::io::Read;
use std::env;

fn command_line_session(){
  let mut history = system::History::new();
  loop{
    let mut input = String::new();
    match system::getline_history("> ",&history) {
      Ok(s) => {
        if s=="" {continue;}
        history.append(&s);
        input=s;
      },
      Err(error) => {println!("Error: {}", error);},
    };
    if input=="quit" {break}
    match compiler::scan(&input,1,"command line") {
      Ok(v) => {
        // compiler::print_vtoken(&v);
        match compiler::compile(v,true,&mut history,"command line") {
          Ok(_) => {},
          Err(e) => {compiler::print_syntax_error(e);}
        };
      },
      Err(error) => {
        compiler::print_syntax_error(error);
      }
    }
  }
}

fn eval_string(s: &str, id: &str){
  let mut history = system::History::new();
  match compiler::scan(s,1,id) {
    Ok(v) => {
      match compiler::compile(v,false,&mut history,id) {
        Ok(_) => {},
        Err(e) => {compiler::print_syntax_error(e);}
      };
    },
    Err(error) => {
      compiler::print_syntax_error(error);
    }
  }
}

fn eval_file(id: &str){
  let mut path: String = String::from(id);
  path += ".moss";
  let mut f = match File::open(&path) {
    Ok(f) => f,
    Err(e) => {
      match File::open(id) {
        Ok(f) => f,
        Err(e) => {
          println!("File '{}' not found.",id);
          return;
        }
      }
    }
  };
  let mut s = String::new();
  f.read_to_string(&mut s).expect("something went wrong reading the file");
  eval_string(&s,id);
}

fn main(){
  let argv: Vec<String> = env::args().collect();
  if argv.len()<=1 {
    command_line_session();  
  }else{
    eval_file(&argv[1]);
  }
}

fn _main(){
  loop{
    let s = system::getline("# ").unwrap();
    println!("[{}]",s);
  }
}
