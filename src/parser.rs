use crate::ast;

use crate::ast::RType;

use crate::lexer;

use cranelift::codegen::settings;
use cranelift::codegen::verifier::verify_function;

use std::collections::HashMap;

use std::fmt::Write;

pub struct Parser {
    lex: lexer::Lexer,
    all_instructions: Vec<ast::Expression>,
    all_registered_variables: Vec<ast::VariableInfo>,
    variable_id: usize,
    binary_precedence: HashMap<String, i64>,
}

pub fn init_binary_precedence() -> HashMap<String, i64> {

    let mut h = HashMap::new();

    h.insert(String::from(";"), -1);

    h.insert(String::from("="), 2);
    h.insert(String::from("+="), 3);
    h.insert(String::from("-="), 3);

    h.insert(String::from("<"), 10);
    h.insert(String::from(">"), 10);
    h.insert(String::from("+"), 20);
    h.insert(String::from("-"), 20);
    h.insert(String::from("*"), 40);

    return h;
}

impl Parser {

    pub fn new(get_lex: lexer::Lexer) -> Self {
        Self {
            lex: get_lex,
            all_instructions: Vec::new(),
            all_registered_variables: Vec::new(),
            variable_id: 0,
            binary_precedence: init_binary_precedence(),
        }
    }

    pub fn parse_as(&mut self, expr: ast::Expression, get_target: Option<ast::Expression>) -> ast::Expression {

        println!("Parsing 'as'...");

        self.lex.get_next_token();

        let get_type = if let lexer::LexerToken::Identifier(ident) = &self.lex.current_token { 
            self.parse_type(ident.to_string()) 
        } 
        else { 
            panic!("Expected identifier for type!") 
        };

        self.lex.get_next_token();

        let final_target = if let Some(t) = get_target { t } else { expr.clone() };

        return ast::Expression::RAs { target: Box::new(final_target), val: Box::new(expr), ty: get_type };
    }

    pub fn parse_expression(&mut self, target: Option<ast::Expression>) -> ast::Expression {
        let expr = self.parse_primary(target.clone());

        let final_target = match target.clone() {
            Some(_t) => target,
            None => Some(expr.clone())
        };

        return self.parse_binary_operator(0, expr, final_target);
    }

    pub fn parse_number(&mut self) -> ast::Expression {

        let numb: f64 = self.lex.string_buffer.parse::<f64>().unwrap();

        self.lex.get_next_token();

        return ast::Expression::RNumber { val: numb };
    }

    pub fn parse_return(&mut self) -> ast::Expression {

        self.lex.get_next_token();

        let ret_val = self.parse_expression(None);

        return ast::Expression::RRealReturn { ret: Box::new(ret_val) };
    }

    pub fn parse_primary(&mut self, target: Option<ast::Expression>) -> ast::Expression {

        let mut res = match &self.lex.current_token {
            lexer::LexerToken::Let => self.parse_let(),
            lexer::LexerToken::Identifier(ident) => self.parse_identifier(ident.clone()),
            lexer::LexerToken::Number => self.parse_number(),
            lexer::LexerToken::Return => self.parse_return(),
            _ => panic!("Unknown primary identifier, found {:#?}", &self.lex.current_token)
        };

        if self.lex.current_token == lexer::LexerToken::As {
            res = self.parse_as(res, target.clone());
        }

        res
    }

    pub fn parse_identifier(&mut self, ident: String) -> ast::Expression {

        self.lex.get_next_token();

        return ast::Expression::RVariable { name: ident };
    }

    pub fn parse_equals(&mut self, lv: ast::Expression, rv: ast::Expression) -> ast::Expression {

        return ast::Expression::REquals { lvalue: Box::new(lv), rvalue: Box::new(rv) };
    }

    pub fn parse_add(&mut self, lv: ast::Expression, rv: ast::Expression, target: Option<ast::Expression>) -> ast::Expression {

        let final_target = if let Some(t) = target { t } else { lv.clone() };

        return ast::Expression::RAdd { target: Box::new(final_target), lvalue: Box::new(lv), rvalue: Box::new(rv) };
    }

    pub fn parse_sub(&mut self, lv: ast::Expression, rv: ast::Expression, target: Option<ast::Expression>) -> ast::Expression {

        let final_target = if let Some(t) = target { t } else { lv.clone() };

        return ast::Expression::RSub { target: Box::new(final_target), lvalue: Box::new(lv), rvalue: Box::new(rv) };
    }

    pub fn parse_mul(&mut self, lv: ast::Expression, rv: ast::Expression, target: Option<ast::Expression>) -> ast::Expression {

        let final_target = if let Some(t) = target { t } else { lv.clone() };

        return ast::Expression::RMul { target: Box::new(final_target), lvalue: Box::new(lv), rvalue: Box::new(rv) };
    }

    pub fn get_token_precedence(&self, tok: String) -> i64 {
        return self.binary_precedence.get(&tok).copied().unwrap_or(-1);
    }

    pub fn create_binary(&mut self, op: String, lv: ast::Expression, rv: ast::Expression, target: Option<ast::Expression>) -> ast::Expression {

        match op.as_str() {
            "=" => self.parse_equals(lv, rv),
            "+" => self.parse_add(lv, rv, target),
            "-" => self.parse_sub(lv, rv, target),
            "*" => self.parse_mul(lv, rv, target),
            ";" => lv,
            _ => panic!("Unknown binary operator, found {:#?}", &op)
        }
    }

    pub fn parse_binary_operator(&mut self, expr_precedence: i64, lv: ast::Expression, target: Option<ast::Expression>) -> ast::Expression {

        let mut grab_lv = lv;

        loop {

            let mut left_tok_string: String = if let lexer::LexerToken::Char(c) = self.lex.current_token { String::from(c) } else { String::from("") };

            if left_tok_string == ";" { return grab_lv; }

            self.lex.get_next_token();

            if let lexer::LexerToken::Char(c) = self.lex.current_token {
                left_tok_string.write_char(c).unwrap();
                self.lex.get_next_token();
            }

            let precedence = self.get_token_precedence(left_tok_string.clone());

            if precedence < expr_precedence { return grab_lv; }

            let mut grab_rv = self.parse_primary(target.clone());

            let right_tok_string: String = if let lexer::LexerToken::Char(c) = self.lex.current_token { String::from(c) } else { String::from("") };

            let next_precedence = self.get_token_precedence(right_tok_string);

            if precedence < next_precedence {
                grab_rv = self.parse_binary_operator(precedence + 1, grab_rv, target.clone());
            }

            grab_lv = self.create_binary(left_tok_string, grab_lv, grab_rv, target.clone());
        }
    }

    pub fn parse_type(&mut self, ident: String) -> RType {

        match ident.as_str() {
            "i32" => RType::Int32,
            "i64" => RType::Int64,
            _ => panic!("Type \"{}\" does not exist", ident)
        }
    }

    pub fn parse_let(&mut self) -> ast::Expression {
        self.lex.get_next_token();

        let get_name = if let lexer::LexerToken::Identifier(ident) = &self.lex.current_token {
            ident.clone()
        }
        else {
            panic!("Expected identifier!") 
        };

        self.lex.get_next_token();

        if self.lex.current_token != lexer::LexerToken::Char(':') {
            panic!("Expected ':'");
        }

        self.lex.get_next_token();

        let get_type = if let lexer::LexerToken::Identifier(ident) = &self.lex.current_token { 
            self.parse_type(ident.to_string()) 
        } 
        else { 
            panic!("Expected identifier for type!") 
        };

        self.lex.get_next_token();

        let new_var_info = ast::VariableInfo::new(get_name.clone(), get_type.clone(), self.variable_id);

        self.variable_id += 1;

        self.all_registered_variables.push(new_var_info);

        return ast::Expression::RLet { name: get_name.to_string(), ty: get_type };
    }

    pub fn parse_function(&mut self) -> ast::Expression {

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

        // =======================[TYPE]========================

        if self.lex.current_token != lexer::LexerToken::Char('-') {
            panic!("Expected '->'");
        }

        self.lex.get_next_token();

        if self.lex.current_token != lexer::LexerToken::Char('>') {
            panic!("Expected '->'");
        }

        self.lex.get_next_token();

        let get_type = if let lexer::LexerToken::Identifier(ident) = &self.lex.current_token { 
            self.parse_type(ident.to_string()) 
        } 
        else { 
            panic!("Expected identifier for type!") 
        };

        self.lex.get_next_token();

        // =====================================================

        if self.lex.current_token != lexer::LexerToken::Char('{') {
            panic!("Expected '{{'");
        }

        self.lex.get_next_token();

         // =======================[BODY]========================

        let mut all_inst: Vec<ast::Expression> = Vec::new();

        while self.lex.current_token != lexer::LexerToken::Char('}') && self.lex.current_token != lexer::LexerToken::EndOfFile {

            let expr: ast::Expression = self.parse_expression(None);

            if self.lex.current_token != lexer::LexerToken::Char(';') {
                panic!("Expected ';'. Found {:#?}", &self.lex.current_token);
            }

            all_inst.push(expr);

            self.lex.get_next_token();
        }

        // ======================================================

        if self.lex.current_token != lexer::LexerToken::Char('}') {
            panic!("Expected '}}'");
        }

        self.lex.get_next_token();

        return ast::Expression::RFunction { name: get_name, ty: get_type, instructions: all_inst };
    }

    pub fn start(&mut self) {

        self.lex.get_next_token();

        while self.lex.current_token != lexer::LexerToken::EndOfFile {

            match self.lex.current_token {
                lexer::LexerToken::Function => { 
                    let func = self.parse_function();
                    dbg!(&func);
                    self.all_instructions.push(func);
                },
                _ => break
            };
        }

        for inst in &self.all_instructions {
            let cg_inst = inst.codegen_function(&mut self.all_registered_variables);

            let flags = settings::Flags::new(settings::builder());
            let res = verify_function(&cg_inst, &flags);

            println!("{}", cg_inst.display());

            if let Err(errors) = res {
                panic!("{}", errors);
            }
        }
    }
}