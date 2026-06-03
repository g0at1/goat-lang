#[derive(Debug, Clone, PartialEq)]
pub enum Token {
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

pub struct Lexer {
    src: Vec<char>,
    pos: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
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

    pub fn next_token(&mut self) -> Token {
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
            d if d.is_ascii_digit() => {
                let mut num = String::new();

                while self.peek().is_ascii_digit() {
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
                    _ => Token::Ident(id),
                }
            }
            unexpected => panic!("Unexpected character: {}", unexpected),
        }
    }
}
