mod lexer;

enum Type {
    Int32,
}

enum Expression {
    Let {
        name: String,
        ty: Type
    },
    Number {
        val: f64
    },
    Function {
        name: String,
        instructions: Vec<Expression>
    },
    Variable {
        name: String,
    },
    Equals {
        lvalue: Box<Expression>,
        rvalue: Box<Expression>
    },
}

impl Expression {
    fn codegen(&self) -> String {
        match self {

            Expression::Function { name, instructions } => {

                let mut result: String = name.to_string();
                result += ":\n";

                for inst in instructions {
                    let inst_cg: String = inst.codegen();

                    if inst_cg != "" {
                        result += "\t";
                        result += &inst_cg;
                        result += "\n";
                    }
                }

                result
            },

            Expression::Number { val } => {
                val.to_string()
            },

            Expression::Equals { lvalue, rvalue } => {

                let mut result: String = String::new();
                result += "movl ";
                result += &lvalue.codegen();
                result += ", ";
                result += &rvalue.codegen();
                result
            },

            Expression::Let { name, ty: _ } => {
                return name.to_string()
            },

            _ => "".to_string()

        }
    }
}

struct VariableInfo {
    name: String
}

struct Parser {
    lex: lexer::Lexer,
    all_instructions: Vec<Expression>,
    all_registered_variables: Vec<VariableInfo>
}

impl Parser {

    fn new(add_lex: lexer::Lexer) -> Self {
        Self {
            lex: add_lex,
            all_instructions: Vec::new(),
            all_registered_variables: Vec::new()
        }
    }

    fn parse_equals(&mut self, lv: Expression) -> Expression {

        self.lex.get_next_token();

        let rv: Expression = self.parse_expression();

        return Expression::Equals { lvalue: Box::new(lv), rvalue: Box::new(rv) }
    }

    fn parse_binary_operator(&mut self, expr: Expression) -> Expression {

        match self.lex.current_token {
            lexer::LexerToken::Char('=') => self.parse_equals(expr),
            _ => expr
        }
    }

    fn parse_expression(&mut self) -> Expression {
        let expr = self.parse_primary();

        self.parse_binary_operator(expr)
    }

    fn get_type(&mut self) -> Type {

        if let lexer::LexerToken::Identifier(ident) = &self.lex.current_token {

            let ident_str = ident.as_str();
            match ident_str {
                "i32" => return Type::Int32,
                _ => panic!("Unknown type.")
            }
        }

        panic!("Expected type.");
    }

    fn parse_let(&mut self) -> Expression {
        self.lex.get_next_token();

        let get_name: String;
        if let lexer::LexerToken::Identifier(ident) = &self.lex.current_token {
            get_name = ident.clone();
        }
        else {
            panic!("Expected identifier");
        }

        self.lex.get_next_token();

        if self.lex.current_token != lexer::LexerToken::Char(':') {
            panic!("Expected ':' to set 'let' type.");
        }

        self.lex.get_next_token();

        let get_type: Type = self.get_type();

        self.lex.get_next_token();

        self.all_registered_variables.push(VariableInfo { name: get_name.clone() });

        return Expression::Let { name: get_name, ty: get_type };
    }

    fn parse_number(&mut self) -> Expression {

        let get_val: String = self.lex.string_buffer.clone();

        self.lex.get_next_token();

        return Expression::Number { val: get_val.parse().unwrap() };
    }

    fn parse_primary(&mut self) -> Expression {

        match self.lex.current_token {
            lexer::LexerToken::Let => self.parse_let(),
            lexer::LexerToken::Number => self.parse_number(),
            _ => panic!("Unknown identifier found!")
        }
    }

    fn parse_function(&mut self) -> Expression {

        self.lex.get_next_token();

        println!("Parsing function...");

        let get_name: String;
        if let lexer::LexerToken::Identifier(ident) = &self.lex.current_token {
            get_name = ident.clone();
        }
        else {
            panic!("Expected identifier");
        }

        self.lex.get_next_token();

        if self.lex.current_token != lexer::LexerToken::Char('(') {
            panic!("Expected '('");
        }

        self.lex.get_next_token();

        // =======================[ARGUMENTS]========================

        println!("TODO: Add Arguments.");

        // ==========================================================

        if self.lex.current_token != lexer::LexerToken::Char(')') {
            panic!("Expected ')'");
        }

        self.lex.get_next_token();

        if self.lex.current_token != lexer::LexerToken::Char('{') {
            panic!("Expected '{{'");
        }

        self.lex.get_next_token();

         // =======================[BODY]========================

        let mut all_inst: Vec<Expression> = Vec::new();

        while self.lex.current_token != lexer::LexerToken::Char('}') && self.lex.current_token != lexer::LexerToken::EndOfFile {

            let expr: Expression = self.parse_expression();

            if self.lex.current_token != lexer::LexerToken::Char(';') {
                panic!("Expected ';'");
            }

            all_inst.push(expr);

            self.lex.get_next_token();
        }

        // ======================================================

        if self.lex.current_token != lexer::LexerToken::Char('}') {
            panic!("Expected '}}'");
        }

        self.lex.get_next_token();

        return Expression::Function { name: get_name, instructions: all_inst };
    }

    fn start(&mut self) {

        self.lex.get_next_token();

        while self.lex.current_token != lexer::LexerToken::EndOfFile {

            match self.lex.current_token {
                lexer::LexerToken::Function => { 
                    let func = self.parse_function();
                    self.all_instructions.push(func);
                },
                _ => break
            };
        }

        for inst in &self.all_instructions {
            println!("{}", inst.codegen());
        }
    }
}

fn main() {
    let mut lex: lexer::Lexer = lexer::Lexer::new("fn main() { let test_var: i32 = 5; }".to_string());

    let mut par: Parser = Parser::new(lex);

    par.start();
}
