use crate::error::ScriptError;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TokenKind {
    Identifier(String),
    Number(f64),
    String(String),
    Assign,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Tilde,
    AndAnd,
    OrOr,
    Not,
    EqEq,
    EqEqEq,
    Neq,
    NeqEq,
    Lt,
    Lte,
    Gt,
    Gte,
    In,
    Typeof,
    Question,
    Colon,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Dot,
    Spread,
    Let,
    If,
    Else,
    For,
    Of,
    Continue,
    Break,
    Directive,
    From,
    Try,
    Catch,
    Throw,
    Return,
    Eof,
}

#[derive(Debug, Clone)]
pub(crate) struct Token {
    pub kind: TokenKind,
    pub pos: usize,
}

pub(crate) struct Lexer<'a> {
    input: &'a [u8],
    idx: usize,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        Self {
            input: input.as_bytes(),
            idx: 0,
        }
    }

    fn peek(&self) -> Option<u8> {
        self.input.get(self.idx).copied()
    }

    fn bump(&mut self) -> Option<u8> {
        let ch = self.peek()?;
        self.idx += 1;
        Some(ch)
    }

    fn consume_while<F: FnMut(u8) -> bool>(&mut self, mut f: F) -> &'a [u8] {
        let start = self.idx;
        while let Some(c) = self.peek() {
            if f(c) {
                self.idx += 1;
            } else {
                break;
            }
        }
        &self.input[start..self.idx]
    }

    fn skip_ws(&mut self) {
        loop {
            while matches!(self.peek(), Some(b' ' | b'\n' | b'\t' | b'\r')) {
                self.idx += 1;
            }
            if self.peek() == Some(b'/') && self.input.get(self.idx + 1) == Some(&b'/') {
                self.idx += 2;
                self.consume_while(|c| c != b'\n');
                continue;
            }
            if self.peek() == Some(b'/') && self.input.get(self.idx + 1) == Some(&b'*') {
                self.idx += 2;
                while self.idx + 1 < self.input.len() {
                    if self.input[self.idx] == b'*' && self.input[self.idx + 1] == b'/' {
                        self.idx += 2;
                        break;
                    }
                    self.idx += 1;
                }
                continue;
            }
            break;
        }
    }

    fn identifier(&mut self) -> TokenKind {
        let start = self.idx - 1;
        self.consume_while(|c| c.is_ascii_alphanumeric() || c == b'_' || c == b'$');
        let text = std::str::from_utf8(&self.input[start..self.idx]).unwrap_or("");
        match text {
            "let" => TokenKind::Let,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "for" => TokenKind::For,
            "of" => TokenKind::Of,
            "continue" => TokenKind::Continue,
            "break" => TokenKind::Break,
            "directive" => TokenKind::Directive,
            "from" => TokenKind::From,
            "try" => TokenKind::Try,
            "catch" => TokenKind::Catch,
            "throw" => TokenKind::Throw,
            "return" => TokenKind::Return,
            "in" => TokenKind::In,
            "typeof" => TokenKind::Typeof,
            _ => TokenKind::Identifier(text.to_string()),
        }
    }

    fn number(&mut self, first: u8) -> Result<TokenKind, ScriptError> {
        let start = self.idx - 1;
        let mut seen_dot = first == b'.';
        self.consume_while(|c| {
            if c == b'.' && !seen_dot {
                seen_dot = true;
                return true;
            }
            c.is_ascii_digit()
        });
        let text = std::str::from_utf8(&self.input[start..self.idx]).unwrap_or("");
        let num = text.parse::<f64>().map_err(|e| ScriptError {
            message: format!("invalid number {text}: {e}"),
            position: Some(start),
        })?;
        Ok(TokenKind::Number(num))
    }

    fn string(&mut self, quote: u8) -> Result<TokenKind, ScriptError> {
        let mut out = String::new();
        while let Some(c) = self.bump() {
            if c == quote {
                return Ok(TokenKind::String(out));
            }
            if c == b'\\' {
                let Some(esc) = self.bump() else {
                    return Err(ScriptError::new("unterminated escape"));
                };
                let ch = match esc {
                    b'n' => '\n',
                    b't' => '\t',
                    b'r' => '\r',
                    b'\\' => '\\',
                    b'"' => '"',
                    b'\'' => '\'',
                    other => other as char,
                };
                out.push(ch);
            } else {
                out.push(c as char);
            }
        }
        Err(ScriptError::new("unterminated string literal"))
    }

    pub(crate) fn tokenize(mut self) -> Result<Vec<Token>, ScriptError> {
        let mut tokens = Vec::new();
        loop {
            self.skip_ws();
            let pos = self.idx;
            let Some(ch) = self.bump() else {
                tokens.push(Token {
                    kind: TokenKind::Eof,
                    pos,
                });
                break;
            };

            let kind = match ch {
                b'0'..=b'9' => self.number(ch)?,
                b'.' => {
                    if self.input.get(self.idx) == Some(&b'.')
                        && self.input.get(self.idx + 1) == Some(&b'.')
                    {
                        self.idx += 2;
                        TokenKind::Spread
                    } else if self
                        .input
                        .get(self.idx)
                        .map_or(false, |c| c.is_ascii_digit())
                    {
                        self.number(ch)?
                    } else {
                        TokenKind::Dot
                    }
                }
                b'\"' | b'\'' => self.string(ch)?,
                b'+' => TokenKind::Plus,
                b'-' => TokenKind::Minus,
                b'*' => TokenKind::Star,
                b'/' => TokenKind::Slash,
                b'%' => TokenKind::Percent,
                b'~' => TokenKind::Tilde,
                b'!' => {
                    if self.peek() == Some(b'=') {
                        self.idx += 1;
                        if self.peek() == Some(b'=') {
                            self.idx += 1;
                            TokenKind::NeqEq
                        } else {
                            TokenKind::Neq
                        }
                    } else {
                        TokenKind::Not
                    }
                }
                b'=' => {
                    if self.peek() == Some(b'=') {
                        self.idx += 1;
                        if self.peek() == Some(b'=') {
                            self.idx += 1;
                            TokenKind::EqEqEq
                        } else {
                            TokenKind::EqEq
                        }
                    } else {
                        TokenKind::Assign
                    }
                }
                b'<' => {
                    if self.peek() == Some(b'=') {
                        self.idx += 1;
                        TokenKind::Lte
                    } else {
                        TokenKind::Lt
                    }
                }
                b'>' => {
                    if self.peek() == Some(b'=') {
                        self.idx += 1;
                        TokenKind::Gte
                    } else {
                        TokenKind::Gt
                    }
                }
                b'&' if self.peek() == Some(b'&') => {
                    self.idx += 1;
                    TokenKind::AndAnd
                }
                b'|' if self.peek() == Some(b'|') => {
                    self.idx += 1;
                    TokenKind::OrOr
                }
                b'?' => TokenKind::Question,
                b':' => TokenKind::Colon,
                b',' => TokenKind::Comma,
                b';' => TokenKind::Semicolon,
                b'(' => TokenKind::LParen,
                b')' => TokenKind::RParen,
                b'{' => TokenKind::LBrace,
                b'}' => TokenKind::RBrace,
                b'[' => TokenKind::LBracket,
                b']' => TokenKind::RBracket,
                c if c.is_ascii_alphabetic() || c == b'_' || c == b'$' => self.identifier(),
                _ => {
                    return Err(ScriptError::with_pos(
                        format!("unexpected character '{}'", ch as char),
                        pos,
                    ));
                }
            };

            tokens.push(Token { kind, pos });
        }
        Ok(tokens)
    }
}
