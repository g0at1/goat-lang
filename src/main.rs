use std::arch::asm;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::process;

// --- LEXER ----------------------------------------

#[derive(Debug, Clone, PartialEq)]
enum Token {
    If,
    True,
    False,
    Print,
    Ident(String),
    Number(i64),
    Assign,
    Equal,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Semicolon,
    StringLiteral(String),
    EOF,
}

struct Lexer {
    src: Vec<char>,
    pos: usize,
}

impl Lexer {
    fn new(input: &str) -> Self {
        Lexer {
            src: input.chars().collect(),
            pos: 0,
        }
    }

    fn peek(&self) -> char {
        *self.src.get(self.pos).unwrap_or(&'\0')
    }

    fn bump(&mut self) {
        self.pos += 1;
    }

    fn skip_ws(&mut self) {
        while self.peek().is_whitespace() {
            self.bump();
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_ws();
        let c = self.peek();
        match c {
            '\0' => Token::EOF,
            '(' => {
                self.bump();
                Token::LParen
            }
            ')' => {
                self.bump();
                Token::RParen
            }
            '{' => {
                self.bump();
                Token::LBrace
            }
            '}' => {
                self.bump();
                Token::RBrace
            }
            ';' => {
                self.bump();
                Token::Semicolon
            }
            '"' => {
                self.bump();
                let mut s = String::new();
                while self.peek() != '"' && self.peek() != '\0' {
                    s.push(self.peek());
                    self.bump();
                }
                if self.peek() == '"' {
                    self.bump();
                }
                Token::StringLiteral(s)
            }
            '=' => {
                self.bump();
                if self.peek() == '=' {
                    self.bump();
                    Token::Equal
                } else {
                    Token::Assign
                }
            }
            d if d.is_digit(10) => {
                let mut num = String::new();
                while self.peek().is_digit(10) {
                    num.push(self.peek());
                    self.bump();
                }
                Token::Number(num.parse().unwrap())
            }
            a if a.is_alphabetic() => {
                let mut id = String::new();
                while self.peek().is_alphanumeric() {
                    id.push(self.peek());
                    self.bump();
                }
                match id.as_str() {
                    "if" => Token::If,
                    "true" => Token::True,
                    "false" => Token::False,
                    "print" => Token::Print,
                    _ => Token::Ident(id.to_string()),
                }
            }
            unexpected => panic!("Unexpected character: {}", unexpected),
        }
    }
}

// --- AST -----------------------------------------

#[derive(Debug, Clone)]
enum Expr {
    Bool(bool),
    Number(i64),
    Var(String),
    Equal(Box<Expr>, Box<Expr>),
    StringLiteral(String),
}

#[derive(Debug, Clone)]
enum Stmt {
    PrintExpr(Expr),
    Assign(String, Expr),
    If { cond: Expr, body: Vec<Stmt> },
}

// --- PARSER --------------------------------------

struct Parser {
    lexer: Lexer,
    cur: Token,
}

impl Parser {
    fn new(input: &str) -> Self {
        let mut lx = Lexer::new(input);
        let first = lx.next_token();
        Parser {
            lexer: lx,
            cur: first,
        }
    }

    fn bump(&mut self) {
        self.cur = self.lexer.next_token();
    }

    fn expect(&mut self, t: Token) {
        if std::mem::discriminant(&self.cur) == std::mem::discriminant(&t) {
            self.bump();
        } else {
            panic!("Expected {:?}, found {:?}", t, self.cur);
        }
    }

    fn parse(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while self.cur != Token::EOF {
            stmts.push(self.parse_stmt());
        }
        stmts
    }

    fn parse_stmt(&mut self) -> Stmt {
        match &self.cur {
            Token::Print => self.parse_print(),
            Token::If => self.parse_if(),
            Token::Ident(name) => self.parse_assign(name.clone()),
            _ => panic!("Unexpected token at stmt: {:?}", self.cur),
        }
    }

    fn parse_print(&mut self) -> Stmt {
        self.expect(Token::Print);
        self.expect(Token::LParen);

        let expr = self.parse_expr();

        self.expect(Token::RParen);
        self.expect(Token::Semicolon);
        Stmt::PrintExpr(expr)
    }

    fn parse_assign(&mut self, name: String) -> Stmt {
        self.bump();
        self.expect(Token::Assign);
        let expr = self.parse_expr();
        self.expect(Token::Semicolon);
        Stmt::Assign(name, expr)
    }

    fn parse_if(&mut self) -> Stmt {
        self.expect(Token::If);
        self.expect(Token::LParen);
        let cond = self.parse_expr();
        self.expect(Token::RParen);
        self.expect(Token::LBrace);
        let mut body = Vec::new();
        while self.cur != Token::RBrace {
            body.push(self.parse_stmt());
        }
        self.expect(Token::RBrace);
        Stmt::If { cond, body }
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_equality()
    }

    fn parse_equality(&mut self) -> Expr {
        let mut left = self.parse_primary();
        while let Token::Equal = self.cur {
            self.bump();
            let right = self.parse_primary();
            left = Expr::Equal(Box::new(left), Box::new(right));
        }
        left
    }

    fn parse_primary(&mut self) -> Expr {
        match &self.cur {
            Token::True => {
                self.bump();
                Expr::Bool(true)
            }
            Token::False => {
                self.bump();
                Expr::Bool(false)
            }
            Token::Number(n) => {
                let v = *n;
                self.bump();
                Expr::Number(v)
            }
            Token::StringLiteral(s) => {
                let lit = s.clone();
                self.bump();
                Expr::StringLiteral(lit)
            }
            Token::Ident(name) => {
                let v = name.clone();
                self.bump();
                Expr::Var(v)
            }
            Token::LParen => {
                self.bump();
                let expr = self.parse_expr();
                self.expect(Token::RParen);
                expr
            }
            _ => panic!("Expected primary, found {:?}", self.cur),
        }
    }
}

// --- RUNTIME / WARUNKI -------------------------------------

#[derive(Debug, Clone)]
enum Value {
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

fn eval(stmts: &[Stmt], env: &mut HashMap<String, Value>) {
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

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file.goat>", args[0]);
        process::exit(1);
    }
    let src = fs::read_to_string(&args[1]).unwrap_or_else(|e| {
        eprintln!("Error: {}", e);
        process::exit(1)
    });
    let mut parser = Parser::new(&src);
    let stmts = parser.parse();
    let mut env: HashMap<String, Value> = HashMap::new();
    eval(&stmts, &mut env);
}
