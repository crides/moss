
# Examples

## Minimal working example

```rust
extern crate moss;
use moss::object::Object;

fn main(){
    let i = moss::Interpreter::new();
    let x = i.eval(r#"
        f = |n| 1 if n==0 else n*f(n-1)
        f(4)
    "#);
    if let Object::Int(x) = x {
        println!("{}",x);
    }
}
```

## Calling a Rust function from Moss

```rust
extern crate moss;
use moss::object::{Object,Function,FnResult,Env};

fn new_i32_to_i32(f: fn(i32)->i32) -> Object {
    let fp = move |_env: &mut Env, _pself: &Object, argv: &[Object]| -> FnResult {
        match argv[0] {
            Object::Int(n) => {Ok(Object::Int(f(n)))},
            _ => panic!()
        }
    };
    return Function::mutable(Box::new(fp),1,1);
}

fn fac(n: i32) -> i32 {
    if n==0 {1} else {n*fac(n-1)}
}

fn main(){
    let i = moss::Interpreter::new();
    i.rte.gtab.borrow_mut().insert("fac",new_i32_to_i32(fac));
    i.eval(r#"
        print(fac(4))
    "#);
}
```

## Calling a Moss function from Rust

```rust
extern crate moss;
use moss::object::Object;
use std::rc::Rc;

trait I32TOI32 {
    fn i32_to_i32(&self, f: &str) -> Box<Fn(i32)->i32>;
}

impl I32TOI32 for Rc<moss::Interpreter> {
    fn i32_to_i32(&self, f: &str) -> Box<Fn(i32)->i32> {
        let i = self.clone();
        let fobj = i.eval(f);
        return Box::new(move |x: i32| -> i32 {
            return match i.call(&fobj,&Object::Null,&[Object::Int(x)]) {
                Ok(y) => match y {Object::Int(y) => y, _ => panic!()},
                Err(_) => panic!()
            };
        });
    }
}

fn main(){
    let i = Rc::new(moss::Interpreter::new());
    let fac = i.i32_to_i32("|n| (1..n).reduce(1,|x,y| x*y)");
    println!("{}",fac(4));
}
```