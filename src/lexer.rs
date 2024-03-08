use std::fmt::Write;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum LexerToken {
    EndOfFile,
    Function,
    Identifier(String),
    Number,
    Char(char),
    Let,
    As,
}

pub struct Lexer {
    pub content: String,
    pub string_buffer: String,
    pub current_token: LexerToken,
    pub last_char: char,
    pub position: isize,
}

impl Lexer {
    pub fn new(content: String) -> Self {
        Self {
            content,
            string_buffer: String::new(),
            current_token: LexerToken::EndOfFile,
            last_char: ' ',
            position: -1,
        }
    }

    pub fn advance(&mut self) -> char {
        self.position += 1;

        if self.position >= self.content.len() as isize {
            return '\0';
        }

        self.content.chars().nth(self.position as usize).unwrap()
    }

    pub fn get_next_token(&mut self) {
        self.current_token = self.token();
        //dbg!(&self.current_token);
    }

    pub fn token(&mut self) -> LexerToken {
        while self.last_char.is_whitespace() {
            self.last_char = self.advance();
        }

        if self.last_char.is_alphabetic() {
            return self.identifier();
        }

        if self.last_char.is_numeric() {
            return self.number();
        }

        let token = if self.last_char < ' ' {
            LexerToken::EndOfFile
        } else {
            LexerToken::Char(self.last_char)
        };

        self.last_char = self.advance();

        token
    }

    pub fn identifier(&mut self) -> LexerToken {
        self.string_buffer.clear();
        //self.string_buffer.write_char(self.last_char).unwrap();

        while self.is_identifier_char(self.last_char) {
            self.string_buffer.write_char(self.last_char).unwrap();
            self.last_char = self.advance();
        }

        match self.string_buffer.as_str() {
            "fn" => LexerToken::Function,
            "let" => LexerToken::Let,
            "as" => LexerToken::As,
            _ => LexerToken::Identifier(self.string_buffer.to_string()),
        }
    }

    pub fn number(&mut self) -> LexerToken {
        self.string_buffer.clear();

        while self.last_char.is_numeric() || matches!(self.last_char, '.' | 'f' | '_') {
            if self.last_char != '_' {
                self.string_buffer.write_char(self.last_char).unwrap();
            }
            self.last_char = self.advance();
        }

        LexerToken::Number
    }

    pub fn is_identifier_char(&mut self, c: char) -> bool {
        c.is_alphanumeric() || c == '_'
    }

    // pub fn content(&self) -> &str {
    //     &self.content
    // }
}
