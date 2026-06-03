use crate::ast::{Expr, Stmt};
use crate::lexer::{Lexer, Token};

pub struct Parser {
    lexer: Lexer,
    cur: Token,
}

impl Parser {
    pub fn new(input: &str) -> Self {
        let mut lexer = Lexer::new(input);
        let first = lexer.next_token();

        Self {
            lexer,
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

    pub fn parse(&mut self) -> Vec<Stmt> {
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
                let value = *n;
                self.bump();
                Expr::Number(value)
            }
            Token::StringLiteral(s) => {
                let value = s.clone();
                self.bump();
                Expr::StringLiteral(value)
            }
            Token::Ident(name) => {
                let value = name.clone();
                self.bump();
                Expr::Var(value)
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
