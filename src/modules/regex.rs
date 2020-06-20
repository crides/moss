#![allow(clippy::many_single_char_names)]
use std::any::Any;
use std::rc::Rc;
use std::iter::FromIterator;

use regex::{Regex, Captures};

use crate::class;
use crate::object::{
    downcast, new_module, ptr_eq_plain, CharString, Exception, FnResult, Function, Interface, List,
    Object,
};
use crate::vm::{interface_index, interface_types_set, Env, RTE};

fn regex_match(env: &mut Env, pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "match"),
    }
    match argv[0] {
        Object::String(ref s) => {
            if let Some(r) = downcast::<Regex>(pself) {
                Ok(Object::Bool(r.is_match(&String::from_iter(&s.data))))
            } else {
                env.type_error("Type error in r.match(s): r is not a regex.")
            }
        }
        ref s => env.type_error1("Type error in r.match(s): s is not a string.", "s", s),
    }
}

fn regex_list(env: &mut Env, pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "list"),
    }
    match argv[0] {
        Object::String(ref s) => {
            if let Some(r) = downcast::<Regex>(pself) {
                let list = r.find_iter(&String::from_iter(&s.data)).map(|m| CharString::new_object(m.as_str().chars().collect())).collect();
                Ok(List::new_object(list))
            } else {
                env.type_error1("Type error in r.list(s): r is not a regex.", "r", pself)
            }
        }
        ref s => env.type_error1("Type error in r.list(s): s is not a string.", "s", s),
    }
}

fn regex_split(env: &mut Env, pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "split"),
    }
    match argv[0] {
        Object::String(ref s) => {
            if let Some(r) = downcast::<Regex>(pself) {
                let list = r.split(&String::from_iter(&s.data)).map(|s| CharString::new_object(s.chars().collect())).collect();
                Ok(List::new_object(list))
            } else {
                env.type_error1("Type error in r.split(s): r is not a regex.", "r", pself)
            }
        }
        ref s => env.type_error1("Type error in r.split(s): s is not a string.", "s", s),
    }
}

fn regex_groups(env: &mut Env, pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "groups"),
    }
    match argv[0] {
        Object::String(ref s) => {
            if let Some(r) = downcast::<Regex>(pself) {
                Ok(match r.captures(&String::from_iter(&s.data)) {
                    Some(groups) => {
                        let list = (1..groups.len()).filter_map(|i| groups.get(i).map(|g| CharString::new_object(g.as_str().chars().collect()))).fuse().collect();
                        List::new_object(list)
                    }
                    None => Object::Null,
                })
            } else {
                env.type_error1("Type error in r.groups(s): r is not a regex.", "r", pself)
            }
        }
        ref s => env.type_error1("Type error in r.groups(s): s is not a string.", "s", s),
    }
}

fn regex_replace(env: &mut Env, pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        2 => {}
        n => return env.argc_error(n, 2, 2, "replace"),
    }
    match argv[0] {
        Object::String(ref s) => {
            if let Some(r) = downcast::<Regex>(pself) {
                let f = &argv[1];
                let mut err = Ok(Object::Null);
                let text = String::from_iter(&s.data);
                let replaced = r.replace_all(&text, |captures: &Captures| {
                    let m = captures.get(0).unwrap().as_str();
                    let x = CharString::new_object_str(m);
                    if err.is_err() {
                        m.into()
                    } else {
                        let res = match env.call(f, &Object::Null, &[x]) {
                            Ok(Object::String(s)) => String::from_iter(&s.data),
                            Ok(_) => {
                                err = env.type_error("Type error in r.replace(s,f): f(x) is not a string.");
                                m.into()
                            },
                            Err(e) => {
                                err = Err(e);
                                m.into()
                            }
                        };
                        res
                    }
                });
                err?;
                Ok(CharString::new_object_str(replaced.as_ref()))
            } else {
                env.type_error1(
                    "Type error in r.replace(s,f): r is not a regex.",
                    "r",
                    pself,
                )
            }
        }
        ref s => env.type_error1("Type error in r.replace(s,f): s is not a string.", "s", s),
    }
}

impl Interface for Regex {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn type_name(&self, _env: &mut Env) -> String {
        "Regex".to_string()
    }

    fn to_string(self: Rc<Self>, _env: &mut Env) -> Result<String, Box<Exception>> {
        Ok("regex object".to_string())
    }

    fn get_type(&self, env: &mut Env) -> FnResult {
        Ok(Object::Interface(
            env.rte().interface_types.borrow()[interface_index::REGEX].clone(),
        ))
    }

    fn is_instance_of(&self, type_obj: &Object, rte: &RTE) -> bool {
        if let Object::Interface(p) = type_obj {
            ptr_eq_plain(p, &rte.interface_types.borrow()[interface_index::REGEX])
        } else {
            false
        }
    }

    fn get(self: Rc<Self>, key: &Object, env: &mut Env) -> FnResult {
        let t = &env.rte().interface_types.borrow()[interface_index::REGEX];
        match t.slot(key) {
            Some(value) => return Ok(value),
            None => env.index_error(&format!("Index error in t.{0}: {0} not found.", key)),
        }
    }
}

fn regex_compile() -> Object {
    let f = Box::new(
        move |env: &mut Env, _pself: &Object, argv: &[Object]| -> FnResult {
            match argv.len() {
                1 => {}
                n => return env.argc_error(n, 1, 1, "re"),
            }
            match argv[0] {
                Object::String(ref s) => {
                    let r = match Regex::new(&String::from_iter(&s.data)) {
                        Ok(r) => r,
                        Err(e) => {
                            return env.std_exception(&e.to_string());
                        }
                    };
                    return Ok(Object::Interface(Rc::new(r)));
                }
                ref s => env.type_error1("Type error in re(s): s is not a string.", "s", s),
            }
        },
    );
    return Function::mutable(f, 1, 1);
}

pub fn load_regex(env: &mut Env) -> Object {
    let type_regex = class::Class::new("Regex", &Object::Null);
    {
        let mut m = type_regex.map.borrow_mut();
        m.insert_fn_plain("match", regex_match, 1, 1);
        m.insert_fn_plain("list", regex_list, 1, 1);
        m.insert_fn_plain("split", regex_split, 1, 1);
        m.insert_fn_plain("groups", regex_groups, 1, 1);
        m.insert_fn_plain("replace", regex_replace, 2, 2);
    }
    interface_types_set(env.rte(), interface_index::REGEX, type_regex.clone());

    let regex = new_module("regex");
    {
        let mut m = regex.map.borrow_mut();
        m.insert("re", regex_compile());
        m.insert("Regex", Object::Interface(type_regex));
    }
    return Object::Interface(Rc::new(regex));
}
