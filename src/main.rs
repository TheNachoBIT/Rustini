use std::fmt::Write;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum LexerToken {
    EndOfFile,
    Function,
    Identifier(String),
    Number,
    Char(char),
}

struct Lexer {
    content: String,
    string_buffer: String,
    last_char: char,
    position: usize,
}

impl Lexer {
    fn new(content: String) -> Self {
        Self {
            content,
            string_buffer: String::new(),
            last_char: ' ',
            position: 0,
        }
    }

    fn advance(&mut self) -> char {
        let c = self.content.chars().nth(self.position).unwrap();
        self.last_char = c;
        self.position += 1;
        c
    }

    fn token(&mut self) -> LexerToken {
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

    fn identifier(&mut self) -> LexerToken {
        self.string_buffer.clear();

        while is_identifier_char(self.last_char) {
            self.last_char = self.advance();
            self.string_buffer.write_char(self.last_char).unwrap();
        }

        match self.string_buffer.as_str() {
            "fn" => LexerToken::Function,
            _ => LexerToken::Identifier(self.string_buffer.to_string()),
        }
    }

    fn number(&mut self) -> LexerToken {
        self.string_buffer.clear();

        while self.last_char.is_numeric() || matches!(self.last_char, '.' | 'f' | '_') {
            if self.last_char != '_' {
                self.string_buffer.write_char(self.last_char).unwrap();
            }
        }

        LexerToken::Number
    }

    // fn content(&self) -> &str {
    //     &self.content
    // }
}

fn is_identifier_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn main() {
    let mut lex: Lexer = Lexer::new("Hello!".to_string());

    lex.advance();

    let token = lex.token();
    dbg!(token);
}
