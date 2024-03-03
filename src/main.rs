enum LexerToken {
    EndOfFile = -1,
    Function = -2,
    Identifier = -3,
    Number = -4,
}

#[derive(Default)]
struct Lexer {
    content: String,
    identifier_string: String,
    num_val_string: String,
    // string_string: String,

    current_token: isize,
    last_token: isize,
    position: isize
}

impl Lexer {
    fn start(&mut self, get_content: String) {

        self.content = get_content;
        self.position = -1;
        self.last_token = ' ' as isize;
    }

    fn advance(&mut self) -> isize {

        self.position += 1;
        dbg!(self.position, &self.content);
        return self.content.chars().nth(self.position as usize).unwrap() as isize;
    }

    fn get_next_token(&mut self) {
        self.current_token = self.get_token();
    }

    fn get_token(&mut self) -> isize {

        let last_token_converted = self.last_token as u8 as char;

        while char::is_whitespace(last_token_converted) {
            self.last_token = self.advance();
        }

        if char::is_alphabetic(last_token_converted) {
            return self.get_identifier();
        }

        if char::is_numeric(last_token_converted) {
            return self.get_number();
        }

        let mut this_token = self.last_token;
        self.last_token = self.advance();

        if this_token < 32 { this_token = LexerToken::EndOfFile as isize; }

        return this_token;
    }

    fn is_still_identifier(&mut self, tok: isize) -> bool {

        let tok_converted = tok as u8 as char;
        return char::is_alphanumeric(tok_converted) || tok_converted == '_';
    }

    fn is_identifier(&mut self, identifier: &str) -> bool {
        return self.identifier_string == identifier.to_string();
    }

    fn get_identifier(&mut self) -> isize {

        self.identifier_string = self.last_token.to_string();

        while self.is_still_identifier(self.last_token) {
            
            self.last_token = self.advance();
            self.identifier_string += &self.last_token.to_string();
        }

        if self.is_identifier("fn") { return LexerToken::Function as isize; }

        println!("Found {}", self.identifier_string);

        return LexerToken::Identifier as isize;
    }

    fn get_number(&mut self) -> isize {

        let tok_converted = self.last_token as u8 as char;
        let result: String;

        while char::is_numeric(tok_converted) || tok_converted == '.' || tok_converted == 'f' || tok_converted == '_' {

            if tok_converted != '_' {
                self.num_val_string += &self.last_token.to_string()
            }
        }

        return LexerToken::Number as isize;
    }

    fn get_content(&self) -> String {

        return self.content.clone();
    }
}

fn main() {
    let mut lex: Lexer = Lexer::default();
    lex.start("Hello!".to_string());

    lex.advance();

    lex.get_next_token();
}
