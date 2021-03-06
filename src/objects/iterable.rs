#![allow(clippy::many_single_char_names)]
use std::cell::RefCell;
use std::cmp::Ordering;
use std::rc::Rc;

use crate::class::Class;
use crate::global::list;
use crate::object::{
    downcast, float, CharString, EnumFunction, Exception, FnResult, Function, List, MutableFn,
    Object,
};
use crate::range::Range;
use crate::vm::{op_add, op_le, op_lt, op_mul, Env};

pub fn new_iterator(f: MutableFn) -> Object {
    Object::Function(Rc::new(Function {
        f: EnumFunction::Mut(RefCell::new(f)),
        argc: 0,
        argc_min: 0,
        argc_max: 0,
        id: Object::Null,
    }))
}

pub fn int_range_iterator(mut a: i32, b: i32, d: i32) -> MutableFn {
    Box::new(
        move |env: &mut Env, _pself: &Object, argv: &[Object]| -> FnResult {
            match argv.len() {
                0 => {}
                n => return env.argc_error(n, 0, 0, "range iterator"),
            }
            return if a <= b {
                a += d;
                Ok(Object::Int(a - d))
            } else {
                Ok(Object::empty())
            };
        },
    )
}

fn not_iterable(env: &mut Env) -> FnResult {
    env.type_error("Type error in iter(x): x is not iterable.")
}

pub fn iter(env: &mut Env, x: &Object) -> FnResult {
    match *x {
        Object::Int(n) => Ok(new_iterator(int_range_iterator(0, n - 1, 1))),
        Object::Function(ref f) => Ok(Object::Function(f.clone())),
        Object::List(ref a) => {
            let mut index: usize = 0;
            let a = a.clone();
            let f = Box::new(
                move |env: &mut Env, _pself: &Object, argv: &[Object]| -> FnResult {
                    match argv.len() {
                        0 => {}
                        n => return env.argc_error(n, 0, 0, "list iterator"),
                    }
                    let a = a.borrow();
                    if index == a.v.len() {
                        return Ok(Object::empty());
                    } else {
                        index += 1;
                        return Ok(a.v[index - 1].clone());
                    }
                },
            );
            Ok(new_iterator(f))
        }
        Object::Map(ref m) => {
            let mut index: usize = 0;
            let v: Vec<Object> = m.borrow().m.keys().cloned().collect();
            let f = Box::new(
                move |env: &mut Env, _pself: &Object, argv: &[Object]| -> FnResult {
                    match argv.len() {
                        0 => {}
                        n => return env.argc_error(n, 0, 0, "map iterator"),
                    }
                    if index == v.len() {
                        return Ok(Object::empty());
                    } else {
                        index += 1;
                        return Ok(v[index - 1].clone());
                    }
                },
            );
            Ok(new_iterator(f))
        }
        Object::String(ref s) => {
            let mut index: usize = 0;
            let s = s.clone();
            let f = Box::new(
                move |_env: &mut Env, _pself: &Object, _argv: &[Object]| -> FnResult {
                    if index == s.data.len() {
                        return Ok(Object::empty());
                    } else {
                        index += 1;
                        return Ok(Object::String(Rc::new(CharString {
                            data: vec![s.data[index - 1]],
                        })));
                    }
                },
            );
            Ok(new_iterator(f))
        }
        Object::Interface(ref x) => x.clone().iter(env),
        _ => not_iterable(env),
    }
}

fn cycle_iterable(env: &mut Env, x: &Object) -> FnResult {
    let a = match list(env, x)? {
        Object::List(a) => a,
        _ => unreachable!(),
    };
    let mut k: usize = 0;
    let f = Box::new(
        move |env: &mut Env, _pself: &Object, argv: &[Object]| -> FnResult {
            match argv.len() {
                0 => {}
                n => return env.argc_error(n, 0, 0, "cyclic iterator"),
            }
            let v = &a.borrow().v;
            if k < v.len() {
                k += 1;
            } else {
                k = 1;
            }
            return if v.is_empty() {
                env.value_error("Value error in iterator from cycle(a): a is empty.")
            } else {
                Ok(v[k - 1].clone())
            };
        },
    );
    return Ok(new_iterator(f));
}

fn cycle_range(env: &mut Env, a: i32, b: i32) -> FnResult {
    if b < a {
        return env.value_error("Value error in cycle(a..b): b<a.");
    }
    let mut k = a;
    let f = Box::new(
        move |_env: &mut Env, _pself: &Object, _argv: &[Object]| -> FnResult {
            let y = k;
            if k < b {
                k += 1;
            } else {
                k = a;
            }
            return Ok(Object::Int(y));
        },
    );
    return Ok(new_iterator(f));
}

pub fn cycle(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "cycle"),
    }
    let obj = &argv[0];
    if let Object::Int(n) = obj {
        cycle_range(env, 0, n - 1)
    } else if let Some(r) = downcast::<Range>(&obj) {
        if let Object::Int(a) = r.a {
            if let Object::Int(b) = r.b {
                if let Object::Null = r.step {
                    return cycle_range(env, a, b);
                }
            }
        }
        cycle_iterable(env, obj)
    } else {
        cycle_iterable(env, obj)
    }
}

pub fn to_list(env: &mut Env, pself: &Object, argv: &[Object]) -> FnResult {
    let i = &iter(env, pself)?;
    match argv.len() {
        0 => {
            let mut v: Vec<Object> = Vec::new();
            loop {
                let y = env.call(i, &Object::Null, &[])?;
                if y.is_empty() {
                    break;
                } else {
                    v.push(y);
                }
            }
            return Ok(List::new_object(v));
        }
        1 => match argv[0] {
            Object::Int(n) => {
                let mut v: Vec<Object> = Vec::new();
                for _ in 0..n {
                    let y = env.call(i, &Object::Null, &[])?;
                    if y.is_empty() {
                        break;
                    } else {
                        v.push(y);
                    }
                }
                return Ok(List::new_object(v));
            }
            _ => return env.type_error("Type error in i.list(n): n is not an integer."),
        },
        n => {
            return env.argc_error(n, 1, 1, "list");
        }
    }
}

fn map(env: &mut Env, pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "map"),
    }
    let i = iter(env, pself)?;
    let f = argv[0].clone();
    let g = Box::new(
        move |env: &mut Env, _pself: &Object, argv: &[Object]| -> FnResult {
            match argv.len() {
                0 => {}
                n => return env.argc_error(n, 0, 0, "iterator from Iterable.map"),
            }
            let x = env.call(&i, &Object::Null, &[])?;
            return if x.is_empty() {
                Ok(x)
            } else {
                let y = trace_err!(
                    env.call(&f, &Object::Null, &[x]),
                    "iterator from Iterable.map"
                );
                Ok(y)
            };
        },
    );
    Ok(new_iterator(g))
}

fn filter(env: &mut Env, pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "filter"),
    }
    let i = iter(env, pself)?;
    let f = argv[0].clone();
    let g =
        Box::new(
            move |env: &mut Env, _pself: &Object, argv: &[Object]| -> FnResult {
                match argv.len() {
                    0 => {}
                    n => return env.argc_error(n, 0, 0, "iterator from Iterable.filter"),
                }
                loop {
                    let x = env.call(&i, &Object::Null, &[])?;
                    if x.is_empty() {
                        return Ok(x);
                    } else {
                        let y = trace_err!(
                            env.call(&f, &Object::Null, &[x.clone()]),
                            "iterator from Iterable.filter"
                        );
                        match y {
                    Object::Bool(u) => {
                        if u {return Ok(x);}
                    },
                    _ => return env.type_error(
                        "Type error in i.filter(p): return value of p is not of boolean type.")
                }
                    }
                }
            },
        );
    Ok(new_iterator(g))
}

fn each(env: &mut Env, pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "each"),
    }
    let i = &iter(env, pself)?;
    loop {
        let y = env.call(i, &Object::Null, &[])?;
        if y.is_empty() {
            break;
        } else {
            env.call(&argv[0], &Object::Null, &[y])?;
        }
    }
    return Ok(Object::Null);
}

fn any_zero_args(env: &mut Env, pself: &Object) -> FnResult {
    let i = &iter(env, pself)?;
    loop {
        let x = env.call(i, &Object::Null, &[])?;
        if x.is_empty() {
            break;
        } else {
            if let Object::Bool(x) = x {
                if x {
                    return Ok(Object::Bool(true));
                }
            } else {
                return env.type_error(
                    "Type error in i.any(): expected all elements to be of type Bool.",
                );
            }
        }
    }
    return Ok(Object::Bool(false));
}

fn any(env: &mut Env, pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        0 => return any_zero_args(env, pself),
        1 => {}
        n => return env.argc_error(n, 1, 1, "any"),
    }
    let i = &iter(env, pself)?;
    let p = &argv[0];
    loop {
        let x = env.call(i, &Object::Null, &[])?;
        if x.is_empty() {
            break;
        } else {
            let y = env.call(p, &Object::Null, &[x])?;
            if let Object::Bool(yb) = y {
                if yb {
                    return Ok(Object::Bool(true));
                }
            } else {
                return env.type_error(
                    "Type error in i.any(p): return value of p is not of boolean type.",
                );
            }
        }
    }
    return Ok(Object::Bool(false));
}

fn all_zero_args(env: &mut Env, pself: &Object) -> FnResult {
    let i = &iter(env, pself)?;
    loop {
        let x = env.call(i, &Object::Null, &[])?;
        if x.is_empty() {
            break;
        } else {
            if let Object::Bool(x) = x {
                if !x {
                    return Ok(Object::Bool(false));
                }
            } else {
                return env.type_error(
                    "Type error in i.all(): expected all elements to be of type Bool.",
                );
            }
        }
    }
    return Ok(Object::Bool(true));
}

fn all(env: &mut Env, pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        0 => return all_zero_args(env, pself),
        1 => {}
        n => return env.argc_error(n, 1, 1, "all"),
    }
    let i = &iter(env, pself)?;
    let p = &argv[0];
    loop {
        let x = env.call(i, &Object::Null, &[])?;
        if x.is_empty() {
            break;
        } else {
            let y = env.call(p, &Object::Null, &[x])?;
            if let Object::Bool(yb) = y {
                if !yb {
                    return Ok(Object::Bool(false));
                }
            } else {
                return env.type_error(
                    "Type error in i.all(p): return value of p is not of boolean type.",
                );
            }
        }
    }
    return Ok(Object::Bool(true));
}

fn count_all(env: &mut Env, a: &Object) -> FnResult {
    let i = iter(env, a)?;
    let mut k: i32 = 0;
    loop {
        let x = env.call(&i, &Object::Null, &[])?;
        if x.is_empty() {
            return Ok(Object::Int(k));
        } else {
            k += 1;
        }
    }
}

fn count(env: &mut Env, pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        0 => return count_all(env, pself),
        1 => {}
        n => return env.argc_error(n, 0, 1, "count"),
    }
    let i = &iter(env, pself)?;
    let p = &argv[0];
    let mut k: i32 = 0;
    loop {
        let x = env.call(i, &Object::Null, &[])?;
        if x.is_empty() {
            break;
        } else {
            let y = env.call(p, &Object::Null, &[x])?;
            if let Object::Bool(yb) = y {
                if yb {
                    k += 1;
                }
            } else {
                return env.type_error(
                    "Type error in i.count(p): return value of p is not of boolean type.",
                );
            }
        }
    }
    return Ok(Object::Int(k));
}

fn chunks(env: &mut Env, pself: &Object, argv: &[Object]) -> FnResult {
    let n = match argv.len() {
        1 => match argv[0] {
            Object::Int(x) => {
                if x >= 0 {
                    x as usize
                } else {
                    return env.value_error("Value error in a.chunks(n): n<0.");
                }
            }
            _ => return env.type_error("Type error in a.chunks(n): n is not an integer."),
        },
        n => return env.argc_error(n, 1, 1, "chunks"),
    };
    let i = iter(env, pself)?;
    let mut empty = false;
    let g = Box::new(
        move |env: &mut Env, _pself: &Object, argv: &[Object]| -> FnResult {
            match argv.len() {
                0 => {}
                n => return env.argc_error(n, 0, 0, "iterator from Iterable.chunks"),
            }
            if empty {
                return Ok(Object::empty());
            }
            let mut v: Vec<Object> = Vec::with_capacity(n);
            for _ in 0..n {
                let y = env.call(&i, &Object::Null, &[])?;
                if y.is_empty() {
                    empty = true;
                    break;
                }
                v.push(y);
            }
            if v.is_empty() {
                Ok(Object::empty())
            } else {
                Ok(List::new_object(v))
            }
        },
    );
    Ok(new_iterator(g))
}

fn reduce(env: &mut Env, pself: &Object, argv: &[Object]) -> FnResult {
    let i = iter(env, pself)?;
    match argv.len() {
        1 => {
            let mut y = env.call(&i, &Object::Null, &[])?;
            let f = &argv[0];
            loop {
                let x = env.call(&i, &Object::Null, &[])?;
                if x.is_empty() {
                    break;
                }
                y = env.call(f, &Object::Null, &[y, x])?;
            }
            return Ok(y);
        }
        2 => {
            let mut y = argv[0].clone();
            let f = &argv[1];
            loop {
                let x = env.call(&i, &Object::Null, &[])?;
                if x.is_empty() {
                    break;
                }
                y = env.call(f, &Object::Null, &[y, x])?;
            }
            return Ok(y);
        }
        n => env.argc_error(n, 1, 2, "reduce"),
    }
}

fn sum(env: &mut Env, pself: &Object, argv: &[Object]) -> FnResult {
    let i = iter(env, pself)?;
    match argv.len() {
        0 => {
            let mut y = env.call(&i, &Object::Null, &[])?;
            if y.is_empty() {
                return Ok(Object::Int(0));
            }
            loop {
                let x = env.call(&i, &Object::Null, &[])?;
                if x.is_empty() {
                    break;
                }
                y = op_add(env, &y, &x)?;
            }
            return Ok(y);
        }
        1 => {
            let x = env.call(&i, &Object::Null, &[])?;
            if x.is_empty() {
                return Ok(Object::Int(0));
            }
            let f = &argv[0];
            let mut y = env.call(f, &Object::Null, &[x])?;
            loop {
                let x = env.call(&i, &Object::Null, &[])?;
                if x.is_empty() {
                    break;
                }
                let u = env.call(f, &Object::Null, &[x])?;
                y = op_add(env, &y, &u)?;
            }
            return Ok(y);
        }
        n => env.argc_error(n, 1, 2, "sum"),
    }
}

fn prod(env: &mut Env, pself: &Object, argv: &[Object]) -> FnResult {
    let i = iter(env, pself)?;
    match argv.len() {
        0 => {
            let mut y = env.call(&i, &Object::Null, &[])?;
            if y.is_empty() {
                return Ok(Object::Int(1));
            }
            loop {
                let x = env.call(&i, &Object::Null, &[])?;
                if x.is_empty() {
                    break;
                }
                y = op_mul(env, &y, &x)?;
            }
            return Ok(y);
        }
        1 => {
            let x = env.call(&i, &Object::Null, &[])?;
            if x.is_empty() {
                return Ok(Object::Int(1));
            }
            let f = &argv[0];
            let mut y = env.call(f, &Object::Null, &[x])?;
            loop {
                let x = env.call(&i, &Object::Null, &[])?;
                if x.is_empty() {
                    break;
                }
                let u = env.call(f, &Object::Null, &[x])?;
                y = op_mul(env, &y, &u)?;
            }
            return Ok(y);
        }
        n => env.argc_error(n, 1, 2, "sum"),
    }
}

fn compare_lists(a: &[Object], b: &[Object]) -> Ordering {
    let n = a.len().min(b.len());
    for i in 0..n {
        match compare(&a[i], &b[i]) {
            Ordering::Less => return Ordering::Less,
            Ordering::Greater => return Ordering::Greater,
            Ordering::Equal => {}
        }
    }
    return a.len().cmp(&b.len());
}

fn cmp_float(x: f64, y: f64) -> Ordering {
    if x < y {
        Ordering::Less
    } else if (x - y).abs() < f64::EPSILON {
        Ordering::Equal
    } else {
        Ordering::Greater
    }
}

fn compare(a: &Object, b: &Object) -> Ordering {
    match *a {
        Object::Int(x) => match *b {
            Object::Int(y) => x.cmp(&y),
            Object::Float(y) => cmp_float(float(x), y),
            _ => Ordering::Less,
        },
        Object::Float(x) => match *b {
            Object::Int(y) => cmp_float(x, float(y)),
            Object::Float(y) => cmp_float(x, y),
            _ => Ordering::Less,
        },
        Object::String(ref a) => match *b {
            Object::String(ref b) => a.data.cmp(&b.data),
            Object::List(_) => Ordering::Less,
            _ => Ordering::Greater,
        },
        Object::List(ref x) => match *b {
            Object::List(ref y) => compare_lists(&x.borrow().v, &y.borrow().v),
            _ => Ordering::Greater,
        },
        _ => Ordering::Equal,
    }
}

fn compare_by_value(a: &(Object, Object), b: &(Object, Object)) -> Ordering {
    compare(&a.1, &b.1)
}

fn sort_by(env: &mut Env, a: &mut [Object], fcmp: &Object) -> Option<FnResult> {
    let mut err: Option<FnResult> = None;
    {
        let cmp = |x: &Object, y: &Object| -> Ordering {
            let value = match env.call(fcmp, &Object::Null, &[x.clone(), y.clone()]) {
                Ok(value) => value,
                Err(e) => {
                    if err.is_none() {
                        err = Some(Err(e));
                    }
                    Object::Null
                }
            };
            let value = match value {
                Object::Bool(value) => value,
                _ => {
                    if err.is_none() {
                        err = Some(env.type_error(
                        "Type error in a.sort(null,cmp): return value of cmp is not of type Bool."));
                    }
                    true
                }
            };
            return if value {
                Ordering::Less
            } else {
                Ordering::Greater
            };
        };
        a.sort_by(cmp);
    }
    if let Some(e) = err {
        return Some(e);
    } else {
        return None;
    }
}

fn sort_by_key_by(env: &mut Env, a: &mut [(Object, Object)], fcmp: &Object) -> Option<FnResult> {
    let mut err: Option<FnResult> = None;
    {
        let cmp = |tx: &(Object, Object), ty: &(Object, Object)| -> Ordering {
            let value = match env.call(fcmp, &Object::Null, &[tx.1.clone(), ty.1.clone()]) {
                Ok(value) => value,
                Err(e) => {
                    if err.is_none() {
                        err = Some(Err(e));
                    }
                    Object::Null
                }
            };
            let value = match value {
                Object::Bool(value) => value,
                _ => {
                    if err.is_none() {
                        err = Some(env.type_error(
                        "Type error in a.sort(null,cmp): return value of cmp is not of type Bool."));
                    }
                    true
                }
            };
            return if value {
                Ordering::Less
            } else {
                Ordering::Greater
            };
        };
        a.sort_by(cmp);
    }
    if let Some(e) = err {
        return Some(e);
    } else {
        return None;
    }
}

fn sort(env: &mut Env, pself: &Object, argv: &[Object]) -> FnResult {
    let a = match *pself {
        Object::List(ref a) => a.clone(),
        ref x => {
            let y = list(env, x)?;
            match y {
                Object::List(a) => a,
                _ => panic!(),
            }
        }
    };
    {
        let mut ba = a.borrow_mut();
        match argv.len() {
            0 => {
                ba.v.sort_by(compare);
            }
            1 => {
                let mut v: Vec<(Object, Object)> = Vec::with_capacity(ba.v.len());
                for x in &ba.v {
                    let y = env.call(&argv[0], &Object::Null, &[x.clone()])?;
                    v.push((x.clone(), y));
                }
                v.sort_by(compare_by_value);
                ba.v = v.into_iter().map(|x| x.0).collect();
            }
            2 => match argv[0] {
                Object::Null => if let Some(e) = sort_by(env, &mut ba.v, &argv[1]) {
                    return e
                },
                ref p => {
                    let mut v: Vec<(Object, Object)> = Vec::with_capacity(ba.v.len());
                    for x in &ba.v {
                        let y = env.call(p, &Object::Null, &[x.clone()])?;
                        v.push((x.clone(), y));
                    }
                    v.sort_by(compare_by_value);
                    if let Some(e) = sort_by_key_by(env, &mut v, &argv[1]) {
                        return e;
                    }
                    ba.v = v.into_iter().map(|x| x.0).collect();
                }
            },
            n => return env.argc_error(n, 0, 2, "sort"),
        }
    }
    return Ok(Object::List(a));
}

fn skip(env: &mut Env, pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "skip"),
    }
    let i = iter(env, pself)?;
    match argv[0] {
        Object::Int(n) => {
            for _ in 0..n {
                let x = env.call(&i, &Object::Null, &[])?;
                if x.is_empty() {
                    return Ok(i);
                }
            }
            Ok(i)
        }
        ref n => env.type_error1("Type error in i.skip(n): n is not an integer.", "n", n),
    }
}

fn until(env: &mut Env, pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "until"),
    }
    let i = iter(env, pself)?;
    let f = argv[0].clone();
    let g = Box::new(
        move |env: &mut Env, _pself: &Object, argv: &[Object]| -> FnResult {
            match argv.len() {
                0 => {}
                n => return env.argc_error(n, 0, 0, "iterator from Iterable.until"),
            }
            let x = env.call(&i, &Object::Null, &[])?;
            return if x.is_empty() {
                Ok(x)
            } else {
                let y = env.call(&f, &Object::Null, &[x.clone()])?;
                match y {
                    Object::Bool(y) => {
                        if y {
                            return Ok(Object::empty());
                        } else {
                            return Ok(x);
                        }
                    }
                    ref y => {
                        return env.type_error1(
                            "Type error in i.until(p): p(x) is not a boolean.",
                            "p(x)",
                            y,
                        )
                    }
                }
            };
        },
    );
    Ok(new_iterator(g))
}

fn enumerate(env: &mut Env, pself: &Object, argv: &[Object]) -> FnResult {
    let mut k: i32 = match argv.len() {
        0 => 0,
        1 => match argv[0] {
            Object::Int(k) => k,
            _ => 0,
        },
        n => return env.argc_error(n, 0, 1, "enum"),
    };
    let i = iter(env, pself)?;
    let g = Box::new(
        move |env: &mut Env, _pself: &Object, argv: &[Object]| -> FnResult {
            match argv.len() {
                0 => {}
                n => return env.argc_error(n, 0, 0, "iterator from Iterable.enum"),
            }
            let x = env.call(&i, &Object::Null, &[])?;
            return if x.is_empty() {
                Ok(x)
            } else {
                k += 1;
                Ok(List::new_object(vec![Object::Int(k - 1), x]))
            };
        },
    );
    Ok(new_iterator(g))
}

fn take(env: &mut Env, pself: &Object, argv: &[Object]) -> FnResult {
    let n: i32 = match argv.len() {
        1 => match argv[0] {
            Object::Int(n) => n,
            ref n => {
                return env.type_error1("Type error in i.take(n): n is not an integer.", "n", n)
            }
        },
        len => return env.argc_error(len, 0, 1, "enum"),
    };
    let mut k: i32 = 0;
    let i = iter(env, pself)?;
    let g = Box::new(
        move |env: &mut Env, _pself: &Object, argv: &[Object]| -> FnResult {
            match argv.len() {
                0 => {}
                n => return env.argc_error(n, 0, 0, "iterator from Iterable.take"),
            }
            let x = env.call(&i, &Object::Null, &[])?;
            return Ok(if x.is_empty() {
                x
            } else {
                if k < n {
                    k += 1;
                    x
                } else {
                    Object::empty()
                }
            });
        },
    );
    Ok(new_iterator(g))
}

fn join(
    env: &mut Env,
    a: &[Object],
    sep: Option<&Object>,
    left: Option<&Object>,
    right: Option<&Object>,
) -> Result<String, Box<Exception>> {
    let mut s: String = String::new();
    if let Some(left) = left {
        s.push_str(&left.string(env)?);
    }
    if let Some(sep) = sep {
        let sep = &sep.string(env)?;
        let mut first = true;
        for x in a {
            if first {
                first = false;
            } else {
                s.push_str(sep);
            }
            s.push_str(&x.string(env)?);
        }
    } else {
        for x in a {
            s.push_str(&x.string(env)?);
        }
    }
    if let Some(right) = right {
        s.push_str(&right.string(env)?);
    }
    return Ok(s);
}

fn iterable_join(env: &mut Env, pself: &Object, argv: &[Object]) -> FnResult {
    let a = match *pself {
        Object::List(ref a) => a.clone(),
        ref x => match list(env, x)? {
            Object::List(a) => a,
            _ => unreachable!(),
        },
    };
    let y = match argv.len() {
        0 => join(env, &a.borrow().v, None, None, None),
        1 => join(env, &a.borrow().v, Some(&argv[0]), None, None),
        2 => join(env, &a.borrow().v, Some(&argv[0]), Some(&argv[1]), None),
        3 => join(
            env,
            &a.borrow().v,
            Some(&argv[0]),
            Some(&argv[1]),
            Some(&argv[2]),
        ),
        n => return env.argc_error(n, 0, 3, "join"),
    };
    Ok(CharString::new_object_str(&y?))
}

fn min_plain(env: &mut Env, a: &Object) -> FnResult {
    let i = &iter(env, a)?;
    let mut minimum = env.call(i, &Object::Null, &[])?;
    loop {
        let x = env.call(i, &Object::Null, &[])?;
        if x.is_empty() {
            break;
        } else {
            match op_lt(env, &x, &minimum)? {
                Object::Bool(condition) => {
                    if condition {
                        minimum = x;
                    }
                }
                _ => return env.type_error("Type error in a.min(): expected x<y of type Bool."),
            }
        }
    }
    return Ok(minimum);
}

fn iterable_min(env: &mut Env, pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        0 => return min_plain(env, pself),
        1 => {}
        n => return env.argc_error(n, 1, 1, "min"),
    }
    let i = &iter(env, pself)?;
    let p = &argv[0];
    let mut minimum = env.call(i, &Object::Null, &[])?;
    let mut ymin = env.call(p, &Object::Null, &[minimum.clone()])?;
    loop {
        let x = env.call(i, &Object::Null, &[])?;
        if x.is_empty() {
            break;
        } else {
            let y = env.call(p, &Object::Null, &[x.clone()])?;
            match op_lt(env, &y, &ymin)? {
                Object::Bool(condition) => {
                    if condition {
                        minimum = x;
                        ymin = y;
                    }
                }
                _ => {
                    return env
                        .type_error("Type error in a.min(p): expected p(x)<p(y) of type Bool.")
                }
            }
        }
    }
    return Ok(minimum);
}

fn max_plain(env: &mut Env, a: &Object) -> FnResult {
    let i = &iter(env, a)?;
    let mut maximum = env.call(i, &Object::Null, &[])?;
    loop {
        let x = env.call(i, &Object::Null, &[])?;
        if x.is_empty() {
            break;
        } else {
            match op_le(env, &x, &maximum)? {
                Object::Bool(condition) => {
                    if !condition {
                        maximum = x;
                    }
                }
                _ => return env.type_error("Type error in a.max(): expected x<y of type Bool."),
            }
        }
    }
    return Ok(maximum);
}

fn iterable_max(env: &mut Env, pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        0 => return max_plain(env, pself),
        1 => {}
        n => return env.argc_error(n, 1, 1, "max"),
    }
    let i = &iter(env, pself)?;
    let p = &argv[0];
    let mut maximum = env.call(i, &Object::Null, &[])?;
    let mut ymax = env.call(p, &Object::Null, &[maximum.clone()])?;
    loop {
        let x = env.call(i, &Object::Null, &[])?;
        if x.is_empty() {
            break;
        } else {
            let y = env.call(p, &Object::Null, &[x.clone()])?;
            match op_le(env, &y, &ymax)? {
                Object::Bool(condition) => {
                    if !condition {
                        maximum = x;
                        ymax = y;
                    }
                }
                _ => {
                    return env
                        .type_error("Type error in a.max(p): expected p(x)<p(y) of type Bool.")
                }
            }
        }
    }
    return Ok(maximum);
}

pub fn init(t: &Class) {
    let mut m = t.map.borrow_mut();
    m.insert_fn_plain("list", to_list, 0, 1);
    m.insert_fn_plain("each", each, 1, 1);
    m.insert_fn_plain("any", any, 1, 1);
    m.insert_fn_plain("all", all, 1, 1);
    m.insert_fn_plain("count", count, 0, 1);
    m.insert_fn_plain("reduce", reduce, 1, 2);
    m.insert_fn_plain("sum", sum, 1, 2);
    m.insert_fn_plain("prod", prod, 1, 2);
    m.insert_fn_plain("sort", sort, 0, 2);
    m.insert_fn_plain("map", map, 1, 1);
    m.insert_fn_plain("filter", filter, 1, 1);
    m.insert_fn_plain("chunks", chunks, 1, 1);
    m.insert_fn_plain("skip", skip, 1, 1);
    m.insert_fn_plain("until", until, 1, 1);
    m.insert_fn_plain("enum", enumerate, 0, 1);
    m.insert_fn_plain("take", take, 1, 1);
    m.insert_fn_plain("join", iterable_join, 0, 1);
    m.insert_fn_plain("min", iterable_min, 0, 1);
    m.insert_fn_plain("max", iterable_max, 0, 1);
}
