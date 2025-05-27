use std::arch::asm;

use std::{env, fs, process};

// --- LEXER ----------------------------------------

#[derive(Debug, Clone, PartialEq)]
enum Token {
    If,
    True,
    False,
    Print,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Semicolon,
    Equal,
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
                self.bump(); // skip "
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
                    panic!("Unexpected single `=`; use `==` for equality");
                }
            }
            _ if c.is_alphabetic() => {
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
                    _ => panic!("Unknown identifier: {}", id),
                }
            }
            _ => panic!("Unexpected character: {}", c),
        }
    }
}

// --- AST & PARSER -------------------------------

#[derive(Debug)]
enum Expr {
    Bool(bool),
    Equal(Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
enum Stmt {
    Print(String),
    If { cond: Expr, body: Vec<Stmt> },
}

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

    fn expect(&mut self, tok: Token) {
        if std::mem::discriminant(&self.cur) == std::mem::discriminant(&tok) {
            self.bump();
        } else {
            panic!("Expected {:?}, found {:?}", tok, self.cur);
        }
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
            Token::True  => { self.bump(); Expr::Bool(true) }
            Token::False => { self.bump(); Expr::Bool(false) }
            _ => panic!("Expected boolean literal, found {:?}", self.cur),
        }
    }

    fn parse_print(&mut self) -> Stmt {
        self.expect(Token::Print);
        self.expect(Token::LParen);
        let s = if let Token::StringLiteral(s) = &self.cur {
            s.clone()
        } else {
            panic!("Expected string literal, got {:?}", self.cur);
        };
        self.bump();
        self.expect(Token::RParen);
        self.expect(Token::Semicolon);
        Stmt::Print(s)
    }

    fn parse_if(&mut self) -> Stmt {
        self.expect(Token::If);
        self.expect(Token::LParen);
        let cond = self.parse_expr();
        self.expect(Token::RParen);
        self.expect(Token::LBrace);
        let mut body = Vec::new();
        while self.cur != Token::RBrace && self.cur != Token::EOF {
            body.push(self.parse_stmt());
        }
        self.expect(Token::RBrace);
        Stmt::If { cond, body }
    }

    fn parse_stmt(&mut self) -> Stmt {
        match &self.cur {
            Token::Print => self.parse_print(),
            Token::If    => self.parse_if(),
            _ => panic!("Unexpected token at start of stmt: {:?}", self.cur),
        }
    }

    fn parse(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while self.cur != Token::EOF {
            stmts.push(self.parse_stmt());
        }
        stmts
    }
}

// --- RUNTIME -------------------------------------

fn syscall_write(buf: &[u8]) {
    #[cfg(all(target_os = "macos", target_arch = "aarch64"))]
    unsafe {
        asm!(
            // na macOS–AArch64 syscall przez SVC 0
            "svc 0",
            in("x0") 1,                // fd = stdout
            in("x1") buf.as_ptr(),     // wskaźnik na bufor
            in("x2") buf.len(),        // długość
            in("x16") 0x2000004_u64,   // numer write + 0x2000000
            options(nostack),
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
            options(nostack),
        );
    }

    #[cfg(not(any(target_os = "macos", target_os = "linux")))]
    {
        // fallback dla innych platform
        use std::io::Write;
        let _ = std::io::stdout().write_all(buf);
    }
}

fn eval_expr(e: &Expr) -> bool {
    match e {
        Expr::Bool(b) => *b,
        Expr::Equal(l, r) => {
            let lv = eval_expr(l);
            let rv = eval_expr(r);
            lv == rv
        }
    }
}

fn eval(stmts: &[Stmt]) {
    for s in stmts {
        match s {
            Stmt::Print(text) => {
                let mut buf = text.as_bytes().to_vec();
                buf.push(b'\n');
                syscall_write(&buf);
            }
            Stmt::If { cond, body } => {
                if eval_expr(cond) {
                    eval(body);
                }
            }
        }
    }
}

// --- MAIN ----------------------------------------

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
    let ast = parser.parse();
    eval(&ast);
}
