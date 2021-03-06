use std::f64::consts::{E, LOG10_E, PI};
use std::rc::Rc;

use crate::complex::c64;
use crate::object::{float, new_module, FnResult, Object};
use crate::vm::Env;
pub use statrs::function::{
    erf::erf,
    gamma::{gamma, ln_gamma as lgamma},
};

const SQRT_2PI: f64 = 2.5066282746310002;
const TAU: f64 = 2.0 * PI;

pub fn sgngamma(x: f64) -> f64 {
    if x < 0.0 {
        return (x * PI).sin().signum();
    } else {
        return 1.0;
    }
}

fn lanczos_cgamma(z: c64) -> c64 {
    let p = [
        0.9999999999998099,
        676.5203681218851,
        -1259.1392167224028,
        771.3234287776531,
        -176.6150291621406,
        12.507343278686905,
        -0.13857109526572012,
        9.984369578019572e-6,
        1.5056327351493116e-7,
    ];
    let z = z - 1.0;
    let mut y = c64 { re: p[0], im: 0.0 };
    y += p[1] / (z + 1.0);
    y += p[2] / (z + 2.0);
    y += p[3] / (z + 3.0);
    y += p[4] / (z + 4.0);
    y += p[5] / (z + 5.0);
    y += p[6] / (z + 6.0);
    y += p[7] / (z + 7.0);
    y += p[8] / (z + 8.0);
    let t = z + 7.5;
    return SQRT_2PI * t.powc(z + 0.5) * (-t).exp() * y;
}

pub fn cgamma(z: c64) -> c64 {
    if z.re < 0.5 {
        return PI / (PI * z).sin() / lanczos_cgamma(1.0 - z);
    } else {
        return lanczos_cgamma(z);
    }
}

#[inline(never)]
pub fn type_error_int_float(env: &mut Env, id: &str, x: &Object) -> FnResult {
    env.type_error1(
        &format!("Type error in {}(x): expected x of type Int or Float.", id),
        "x",
        x,
    )
}

#[inline(never)]
pub fn type_error_int_float_complex(env: &mut Env, id: &str, x: &Object) -> FnResult {
    env.type_error1(
        &format!(
            "Type error in {}(z): expected z of type Int, Float or Complex.",
            id
        ),
        "z",
        x,
    )
}

fn floor(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "floor"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Float(float(x).floor())),
        Object::Float(x) => Ok(Object::Float(x.floor())),
        ref x => type_error_int_float(env, "floor", x),
    }
}

fn ceil(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "ceil"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Float(float(x).ceil())),
        Object::Float(x) => Ok(Object::Float(x.ceil())),
        ref x => type_error_int_float(env, "ceil", x),
    }
}

fn trunc(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "trunc"),
    }
    let x = match argv[0] {
        Object::Int(x) => float(x),
        Object::Float(x) => x,
        ref x => return type_error_int_float(env, "trunc", x),
    };
    Ok(Object::Float(x.trunc()))
}

fn sqrt(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "sqrt"),
    }
    match argv[0] {
        Object::Float(x) => Ok(Object::Float(x.sqrt())),
        Object::Int(x) => Ok(Object::Float(float(x).sqrt())),
        ref x => type_error_int_float(env, "sqrt", x),
    }
}

fn exp(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "exp"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Float(float(x).exp())),
        Object::Float(x) => Ok(Object::Float(x.exp())),
        Object::Complex(z) => Ok(Object::Complex(z.exp())),
        ref x => type_error_int_float_complex(env, "exp", x),
    }
}

fn ln(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "ln"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Float(float(x).ln())),
        Object::Float(x) => Ok(Object::Float(x.ln())),
        ref x => type_error_int_float(env, "ln", x),
    }
}

fn lg(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "lg"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Float(LOG10_E * float(x).ln())),
        Object::Float(x) => Ok(Object::Float(LOG10_E * x.ln())),
        ref x => type_error_int_float(env, "lg", x),
    }
}

fn sin(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "sin"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Float(float(x).sin())),
        Object::Float(x) => Ok(Object::Float(x.sin())),
        Object::Complex(z) => Ok(Object::Complex(z.sin())),
        ref x => type_error_int_float_complex(env, "sin", x),
    }
}

fn cos(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "cos"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Float(float(x).cos())),
        Object::Float(x) => Ok(Object::Float(x.cos())),
        Object::Complex(z) => Ok(Object::Complex(z.cos())),
        ref x => type_error_int_float_complex(env, "cos", x),
    }
}

fn tan(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "tan"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Float(float(x).tan())),
        Object::Float(x) => Ok(Object::Float(x.tan())),
        Object::Complex(z) => Ok(Object::Complex(z.tan())),
        ref x => type_error_int_float_complex(env, "tan", x),
    }
}

fn sinh(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "sinh"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Float(float(x).sinh())),
        Object::Float(x) => Ok(Object::Float(x.sinh())),
        Object::Complex(z) => Ok(Object::Complex(z.sinh())),
        ref x => type_error_int_float_complex(env, "sinh", x),
    }
}

fn cosh(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "cosh"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Float(float(x).cosh())),
        Object::Float(x) => Ok(Object::Float(x.cosh())),
        Object::Complex(z) => Ok(Object::Complex(z.cosh())),
        ref x => type_error_int_float_complex(env, "cosh", x),
    }
}

fn tanh(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "tanh"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Float(float(x).tanh())),
        Object::Float(x) => Ok(Object::Float(x.tanh())),
        Object::Complex(z) => Ok(Object::Complex(z.tanh())),
        ref x => type_error_int_float_complex(env, "tanh", x),
    }
}

fn asin(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "asin"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Float(float(x).asin())),
        Object::Float(x) => Ok(Object::Float(x.asin())),
        ref x => type_error_int_float(env, "asin", x),
    }
}

fn acos(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "acos"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Float(float(x).acos())),
        Object::Float(x) => Ok(Object::Float(x.acos())),
        ref x => type_error_int_float(env, "acos", x),
    }
}

fn atan(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "atan"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Float(float(x).atan())),
        Object::Float(x) => Ok(Object::Float(x.atan())),
        ref x => type_error_int_float(env, "atan", x),
    }
}

fn asinh(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "asinh"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Float(float(x).asinh())),
        Object::Float(x) => Ok(Object::Float(x.asinh())),
        ref x => type_error_int_float(env, "asinh", x),
    }
}

fn acosh(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "acosh"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Float(float(x).acosh())),
        Object::Float(x) => Ok(Object::Float(x.acosh())),
        ref x => type_error_int_float(env, "acosh", x),
    }
}

fn atanh(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "atanh"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Float(float(x).atanh())),
        Object::Float(x) => Ok(Object::Float(x.atanh())),
        ref x => type_error_int_float(env, "atanh", x),
    }
}

fn math_gamma(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "gamma"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Float(gamma(float(x)))),
        Object::Float(x) => Ok(Object::Float(gamma(x))),
        Object::Complex(z) => Ok(Object::Complex(cgamma(z))),
        ref x => type_error_int_float_complex(env, "gamma", x),
    }
}

fn math_lgamma(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "lgamma"),
    }
    let x = match argv[0] {
        Object::Int(x) => float(x),
        Object::Float(x) => x,
        ref x => return type_error_int_float(env, "lgamma", x),
    };
    Ok(Object::Float(lgamma(x)))
}

fn math_sgngamma(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "sgngamma"),
    }
    let x = match argv[0] {
        Object::Int(x) => float(x),
        Object::Float(x) => x,
        ref x => return type_error_int_float(env, "sgngamma", x),
    };
    Ok(Object::Float(sgngamma(x)))
}

fn hypot(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        2 => {}
        n => return env.argc_error(n, 2, 2, "hypot"),
    }
    match argv[0] {
        Object::Float(x) => match argv[1] {
            Object::Float(y) => Ok(Object::Float(x.hypot(y))),
            Object::Int(y) => Ok(Object::Float(x.hypot(float(y)))),
            _ => env.type_error("Type error in hypot(x,y): y is not a float."),
        },
        Object::Int(x) => match argv[1] {
            Object::Float(y) => Ok(Object::Float(float(x).hypot(y))),
            Object::Int(y) => Ok(Object::Float(float(x).hypot(float(y)))),
            _ => env.type_error("Type error in hypot(x,y): y is not a float."),
        },
        _ => env.type_error("Type error in hypot(x,y): x is not a float."),
    }
}

fn atan2(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        2 => {}
        n => return env.argc_error(n, 2, 2, "atan2"),
    }
    match argv[0] {
        Object::Float(y) => match argv[1] {
            Object::Float(x) => Ok(Object::Float(y.atan2(x))),
            Object::Int(x) => Ok(Object::Float(y.atan2(float(x)))),
            _ => env.type_error("Type error in atan2(y,x): x is not a float."),
        },
        Object::Int(y) => match argv[1] {
            Object::Float(x) => Ok(Object::Float(float(y).atan2(x))),
            Object::Int(x) => Ok(Object::Float(float(y).atan2(float(x)))),
            _ => env.type_error("Type error in atan2(y,x): x is not a float."),
        },
        _ => env.type_error("Type error in atan2(y,x): y is not a float."),
    }
}

fn math_erf(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "erf"),
    }
    match argv[0] {
        Object::Float(x) => Ok(Object::Float(erf(x))),
        Object::Int(x) => Ok(Object::Float(erf(float(x)))),
        ref x => type_error_int_float(env, "erf", x),
    }
}

fn isnan(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "isnan"),
    }
    match argv[0] {
        Object::Int(_) => Ok(Object::Bool(false)),
        Object::Float(x) => Ok(Object::Bool(x.is_nan())),
        ref x => type_error_int_float(env, "isnan", x),
    }
}

fn isinf(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "isinf"),
    }
    match argv[0] {
        Object::Int(_) => Ok(Object::Bool(false)),
        Object::Float(x) => Ok(Object::Bool(x.is_infinite())),
        ref x => type_error_int_float(env, "isinf", x),
    }
}

fn re(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "re"),
    }
    match argv[0] {
        Object::Complex(z) => Ok(Object::Float(z.re)),
        Object::Float(x) => Ok(Object::Float(x)),
        Object::Int(x) => Ok(Object::Int(x)),
        ref x => type_error_int_float_complex(env, "re", x),
    }
}

fn im(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "im"),
    }
    match argv[0] {
        Object::Complex(z) => Ok(Object::Float(z.im)),
        Object::Float(x) => Ok(Object::Float(x)),
        Object::Int(x) => Ok(Object::Int(x)),
        ref x => type_error_int_float_complex(env, "im", x),
    }
}

fn arg(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "arg"),
    }
    match argv[0] {
        Object::Complex(z) => Ok(Object::Float(z.arg())),
        Object::Float(_) => Ok(Object::Float(0.0)),
        Object::Int(_) => Ok(Object::Float(0.0)),
        ref x => type_error_int_float_complex(env, "arg", x),
    }
}

fn conj(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "conj"),
    }
    match argv[0] {
        Object::Complex(z) => Ok(Object::Complex(z.conj())),
        Object::Float(x) => Ok(Object::Float(x)),
        Object::Int(x) => Ok(Object::Int(x)),
        ref x => type_error_int_float_complex(env, "conj", x),
    }
}

fn csqrt(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "sqrt"),
    }
    match argv[0] {
        Object::Complex(z) => Ok(Object::Complex(z.sqrt())),
        Object::Float(x) => {
            if x < 0.0 {
                Ok(Object::Complex(c64 { re: x, im: 0.0 }.sqrt()))
            } else {
                Ok(Object::Float(x.sqrt()))
            }
        }
        Object::Int(x) => {
            if x < 0 {
                Ok(Object::Complex(
                    c64 {
                        re: float(x),
                        im: 0.0,
                    }
                    .sqrt(),
                ))
            } else {
                Ok(Object::Float(float(x).sqrt()))
            }
        }
        ref x => type_error_int_float_complex(env, "sqrt", x),
    }
}

fn cln(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "ln"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Complex(
            c64 {
                re: float(x),
                im: 0.0,
            }
            .ln(),
        )),
        Object::Float(x) => Ok(Object::Complex(c64 { re: x, im: 0.0 }.ln())),
        Object::Complex(z) => Ok(Object::Complex(z.ln())),
        ref x => type_error_int_float_complex(env, "ln", x),
    }
}

fn casin(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "asin"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Complex(
            c64 {
                re: float(x),
                im: 0.0,
            }
            .asin(),
        )),
        Object::Float(x) => Ok(Object::Complex(c64 { re: x, im: 0.0 }.asin())),
        Object::Complex(z) => Ok(Object::Complex(z.asin())),
        ref x => type_error_int_float_complex(env, "asin", x),
    }
}

fn cacos(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "acos"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Complex(
            c64 {
                re: float(x),
                im: 0.0,
            }
            .acos(),
        )),
        Object::Float(x) => Ok(Object::Complex(c64 { re: x, im: 0.0 }.acos())),
        Object::Complex(z) => Ok(Object::Complex(z.acos())),
        ref x => type_error_int_float_complex(env, "acos", x),
    }
}

fn catan(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "atan"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Complex(
            c64 {
                re: float(x),
                im: 0.0,
            }
            .atan(),
        )),
        Object::Float(x) => Ok(Object::Complex(c64 { re: x, im: 0.0 }.atan())),
        Object::Complex(z) => Ok(Object::Complex(z.atan())),
        ref x => type_error_int_float_complex(env, "atan", x),
    }
}

fn casinh(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "asinh"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Complex(
            c64 {
                re: float(x),
                im: 0.0,
            }
            .asinh(),
        )),
        Object::Float(x) => Ok(Object::Complex(c64 { re: x, im: 0.0 }.asinh())),
        Object::Complex(z) => Ok(Object::Complex(z.asinh())),
        ref x => type_error_int_float_complex(env, "asinh", x),
    }
}

fn cacosh(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "acosh"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Complex(
            c64 {
                re: float(x),
                im: 0.0,
            }
            .acosh(),
        )),
        Object::Float(x) => Ok(Object::Complex(c64 { re: x, im: 0.0 }.acosh())),
        Object::Complex(z) => Ok(Object::Complex(z.acosh())),
        ref x => type_error_int_float_complex(env, "acosh", x),
    }
}

fn catanh(env: &mut Env, _pself: &Object, argv: &[Object]) -> FnResult {
    match argv.len() {
        1 => {}
        n => return env.argc_error(n, 1, 1, "atanh"),
    }
    match argv[0] {
        Object::Int(x) => Ok(Object::Complex(
            c64 {
                re: float(x),
                im: 0.0,
            }
            .atanh(),
        )),
        Object::Float(x) => Ok(Object::Complex(c64 { re: x, im: 0.0 }.atanh())),
        Object::Complex(z) => Ok(Object::Complex(z.atanh())),
        ref x => type_error_int_float_complex(env, "atanh", x),
    }
}

pub fn load_math() -> Object {
    let math = new_module("math");
    {
        let mut m = math.map.borrow_mut();
        m.insert("pi", Object::Float(PI));
        m.insert("tau", Object::Float(TAU));
        m.insert("e", Object::Float(E));
        m.insert("nan", Object::Float(::std::f64::NAN));
        m.insert("inf", Object::Float(::std::f64::INFINITY));

        m.insert_fn_plain("floor", floor, 1, 1);
        m.insert_fn_plain("ceil", ceil, 1, 1);
        m.insert_fn_plain("trunc", trunc, 1, 1);
        m.insert_fn_plain("sqrt", sqrt, 1, 1);
        m.insert_fn_plain("exp", exp, 1, 1);
        m.insert_fn_plain("ln", ln, 1, 1);
        m.insert_fn_plain("lg", lg, 1, 1);

        m.insert_fn_plain("sin", sin, 1, 1);
        m.insert_fn_plain("cos", cos, 1, 1);
        m.insert_fn_plain("tan", tan, 1, 1);
        m.insert_fn_plain("sinh", sinh, 1, 1);
        m.insert_fn_plain("cosh", cosh, 1, 1);
        m.insert_fn_plain("tanh", tanh, 1, 1);

        m.insert_fn_plain("asin", asin, 1, 1);
        m.insert_fn_plain("acos", acos, 1, 1);
        m.insert_fn_plain("atan", atan, 1, 1);
        m.insert_fn_plain("asinh", asinh, 1, 1);
        m.insert_fn_plain("acosh", acosh, 1, 1);
        m.insert_fn_plain("atanh", atanh, 1, 1);

        m.insert_fn_plain("gamma", math_gamma, 1, 1);
        m.insert_fn_plain("lgamma", math_lgamma, 1, 1);
        m.insert_fn_plain("sgngamma", math_sgngamma, 1, 1);
        m.insert_fn_plain("hypot", hypot, 2, 2);
        m.insert_fn_plain("atan2", atan2, 2, 2);
        m.insert_fn_plain("erf", math_erf, 1, 1);
        m.insert_fn_plain("isnan", isnan, 1, 1);
        m.insert_fn_plain("isinf", isinf, 1, 1);
    }
    return Object::Interface(Rc::new(math));
}

pub fn load_cmath() -> Object {
    let cmath = new_module("cmath");
    {
        let mut m = cmath.map.borrow_mut();
        m.insert_fn_plain("exp", exp, 1, 1);
        m.insert_fn_plain("sin", sin, 1, 1);
        m.insert_fn_plain("cos", cos, 1, 1);
        m.insert_fn_plain("tan", tan, 1, 1);
        m.insert_fn_plain("sinh", sinh, 1, 1);
        m.insert_fn_plain("cosh", cosh, 1, 1);
        m.insert_fn_plain("tanh", tanh, 1, 1);

        m.insert_fn_plain("asin", casin, 1, 1);
        m.insert_fn_plain("acos", cacos, 1, 1);
        m.insert_fn_plain("atan", catan, 1, 1);
        m.insert_fn_plain("asinh", casinh, 1, 1);
        m.insert_fn_plain("acosh", cacosh, 1, 1);
        m.insert_fn_plain("atanh", catanh, 1, 1);

        m.insert_fn_plain("re", re, 1, 1);
        m.insert_fn_plain("im", im, 1, 1);
        m.insert_fn_plain("conj", conj, 1, 1);
        m.insert_fn_plain("ln", cln, 1, 1);
        m.insert_fn_plain("sqrt", csqrt, 1, 1);
        m.insert_fn_plain("arg", arg, 1, 1);
    }
    return Object::Interface(Rc::new(cmath));
}
