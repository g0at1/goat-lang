use crate::ast::{Expr, Stmt};

use std::arch::asm;
use std::collections::HashMap;

#[cfg(not(any(target_os = "macos", target_os = "linux")))]
use std::io::Write;

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Str(String),
}

fn eval_value(e: &Expr, env: &HashMap<String, Value>) -> Value {
    match e {
        Expr::StringLiteral(s) => Value::Str(s.clone()),
        Expr::Number(n) => Value::Int(*n),
        Expr::Bool(b) => Value::Bool(*b),

        Expr::Var(name) => env.get(name).cloned().unwrap_or(Value::Int(0)),

        Expr::Equal(left, right) => {
            let lval = eval_value(left, env);
            let rval = eval_value(right, env);

            let is_eq = match (lval, rval) {
                (Value::Int(a), Value::Int(b)) => a == b,
                (Value::Bool(a), Value::Bool(b)) => a == b,
                (Value::Str(a), Value::Str(b)) => a == b,
                _ => false,
            };

            Value::Bool(is_eq)
        }
    }
}

fn eval_expr(e: &Expr, env: &HashMap<String, Value>) -> bool {
    match eval_value(e, env) {
        Value::Int(n) => n != 0,
        Value::Bool(b) => b,
        Value::Str(s) => !s.is_empty(),
    }
}

fn eval_to_string(e: &Expr, env: &HashMap<String, Value>) -> String {
    match eval_value(e, env) {
        Value::Int(n) => n.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Str(s) => s,
    }
}

fn syscall_write(buf: &[u8]) {
    #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
    unsafe {
        asm!(
            "svc 0",
            in("x0") 1,
            in("x1") buf.as_ptr(),
            in("x2") buf.len(),
            in("x16") 0x2000004_u64,
            options(nostack)
        );
    }

    #[cfg(target_os = "linux")]
    unsafe {
        asm!(
            "syscall",
            in("rax") 1,
            in("rdi") 1,
            in("rsi") buf.as_ptr(),
            in("rdx") buf.len(),
            options(nostack)
        );
    }

    #[cfg(not(any(target_os = "macos", target_os = "linux")))]
    {
        let _ = std::io::stdout().write_all(buf);
    }
}

pub fn eval(stmts: &[Stmt], env: &mut HashMap<String, Value>) {
    for s in stmts {
        match s {
            Stmt::PrintExpr(expr) => {
                let out = eval_to_string(expr, env);
                let mut buf = out.into_bytes();
                buf.push(b'\n');

                syscall_write(&buf);
            }

            Stmt::Assign(name, expr) => {
                let val = eval_value(expr, env);
                env.insert(name.clone(), val);
            }

            Stmt::If { cond, body } => {
                if eval_expr(cond, env) {
                    eval(body, env);
                }
            }
        }
    }
}
