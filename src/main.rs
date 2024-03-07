mod lexer;

use std::fs;

#[derive(Clone, PartialEq)]
enum Type {
    Int64 {
        move_inst: String,
        all_memory: Vec<String>,
    },
    Int32 {
        move_inst: String,
        all_memory: Vec<String>,
    },
    Void
}

fn int64_memory() -> Vec<String> {
    return vec!["rax".to_string(), 
    "rbx".to_string(), 
    "rcx".to_string(), 
    "rdx".to_string(), 
    "r8".to_string(), 
    "r9".to_string(), 
    "r10".to_string(), 
    "r11".to_string(), 
    "r12".to_string(), 
    "r13".to_string(), 
    "r14".to_string(), 
    "r15".to_string()];
}

fn int32_memory() -> Vec<String> {
    return vec!["eax".to_string(), "ebx".to_string(), "ecx".to_string(), "edx".to_string()];
}

impl Type {

    fn get_real_value(&self, id: usize) -> String {

        match self {
            Type::Int64 { move_inst: _, all_memory } => {
                "%".to_string() + &all_memory[id - 1]
            },
            Type::Int32 { move_inst: _, all_memory } => {
                "%".to_string() + &all_memory[id - 1]
            },
            _ => panic!("Type not currently supported.")
        }
    }

    fn move_codegen(&self, a: &Box<Expression>, b: &Box<Expression>, reg_vars: &Vec<VariableInfo>) -> String {

        match self {

            Type::Int64 { move_inst, all_memory: _ } => {

                let first: String = move_inst.replace("{{1}}", &b.codegen(reg_vars));
                let second: String = first.replace("{{2}}", &a.codegen(reg_vars));
                return second;
            },

            Type::Int32 { move_inst, all_memory: _ } => {

                let first: String = move_inst.replace("{{1}}", &b.codegen(reg_vars));
                let second: String = first.replace("{{2}}", &a.codegen(reg_vars));
                return second;
            },

            _ => panic!("Type not available for codegen.")
        }
    }
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

fn find_type(reg_vars: &Vec<VariableInfo>, name: String) -> Type {
    for r in reg_vars {
        if r.name == name {
            return r.ty.clone()
        }
    }

    return Type::Void;
}

fn get_id(reg_vars: &Vec<VariableInfo>, va: &mut VariableInfo) {

    let mut final_id: usize = 0;
    let mut highest_id: usize = 0;

    for r in reg_vars {
        if r.ty == va.ty {

            if r.moved {
                final_id = r.id;
            }
            else if r.id == final_id {
                final_id = 0;
            }

            if highest_id < r.id {
                highest_id = r.id;
            }
        }
    }

    if final_id == 0 {
        final_id = highest_id + 1;
    }

    va.id = final_id;
}

impl Expression {

    fn get_name(&self) -> String {
        match self {
            Expression::Variable { name } => {
                name.to_string()
            }
            Expression::Let { name, ty: _ } => {
                name.to_string()
            }
            _ => "".to_string()
        }
    }

    fn codegen(&self, reg_vars: &Vec<VariableInfo>) -> String {
        match self {

            Expression::Function { name, instructions } => {

                let mut result: String = name.to_string();
                result += ":\n";

                for inst in instructions {
                    let inst_cg: String = inst.codegen(reg_vars);

                    if inst_cg != "" {
                        result += "\t";
                        result += &inst_cg;
                        result += "\n";
                    }
                }

                // Temporary return for testing purposes only.
                result += "\tretq\n";

                result
            },

            Expression::Number { val } => {
                return "$".to_string() + &val.to_string()
            },

            Expression::Equals { lvalue, rvalue } => {

                let ty = find_type(reg_vars, lvalue.get_name());

                return ty.move_codegen(lvalue, rvalue, reg_vars);
            },

            Expression::Let { name, ty: _ } => {
                return "$".to_string() + name
            },

            Expression::Variable { name } => {
                return "$".to_string() + name
            }

        }
    }
}

struct VariableInfo {
    name: String,
    ty: Type,
    id: usize,
    moved: bool,
}

struct Parser {
    lex: lexer::Lexer,
    all_instructions: Vec<Expression>,
    all_registered_variables: Vec<VariableInfo>,
}

impl Parser {

    fn new(add_lex: lexer::Lexer) -> Self {
        Self {
            lex: add_lex,
            all_instructions: Vec::new(),
            all_registered_variables: Vec::new(),
        }
    }

    fn parse_equals(&mut self, lv: Expression) -> Expression {

        let lv_name = lv.get_name();
        for r in &self.all_registered_variables {
            if r.name == lv_name && r.moved {
                panic!("Variable {} already moved/borrowed!", lv_name);
            }
        }

        self.lex.get_next_token();

        let rv: Expression = self.parse_expression();

        let rv_name = rv.get_name();
        for r in &mut self.all_registered_variables {
            if r.name == rv_name {
                r.moved = true;
            }
        }

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
                "i64" => return Type::Int64 { move_inst: "movq {{1}}, {{2}}".to_string(), all_memory: int64_memory() },
                "i32" => return Type::Int32 { move_inst: "movl {{1}}, {{2}}".to_string(), all_memory: int32_memory() },
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

        for v in &self.all_registered_variables {
            if v.name == get_name {
                panic!("Variable already exists.");
            }
        }

        self.lex.get_next_token();

        if self.lex.current_token != lexer::LexerToken::Char(':') {
            panic!("Expected ':' to set 'let' type.");
        }

        self.lex.get_next_token();

        let get_type: Type = self.get_type();

        self.lex.get_next_token();

        let mut v_info: VariableInfo = VariableInfo { name: get_name.clone(), ty: get_type.clone(), id: 0, moved: false };

        get_id(&self.all_registered_variables, &mut v_info);

        //dbg!(v_info.id);

        self.all_registered_variables.push(v_info);

        return Expression::Let { name: get_name, ty: get_type };
    }

    fn parse_number(&mut self) -> Expression {

        let get_val: String = self.lex.string_buffer.clone();

        self.lex.get_next_token();

        return Expression::Number { val: get_val.parse().unwrap() };
    }

    fn parse_variable(&mut self, ident: String) -> Expression {

        let mut found: bool = false;

        for v in &self.all_registered_variables {
            if v.name == ident {
                found = true;
            }
        }

        if !found {
            panic!("Variable not found.");
        }

        self.lex.get_next_token();

        return Expression::Variable { name: ident };
    }

    fn parse_primary(&mut self) -> Expression {

        match &self.lex.current_token {
            lexer::LexerToken::Let => self.parse_let(),
            lexer::LexerToken::Number => self.parse_number(),
            lexer::LexerToken::Identifier(ident) => self.parse_variable(ident.clone()),
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

        let mut final_codegen: String = String::new();

        for inst in &self.all_instructions {
            final_codegen += &inst.codegen(&self.all_registered_variables);
        }

        let result = replacement_pass(final_codegen.clone(), &self.all_registered_variables);

        println!("{}", result);

        fs::write("output.s", result).expect("Unable to write output.s");
    }
}

fn replacement_pass(cg: String, reg_vars: &Vec<VariableInfo>) -> String {

    let mut result = cg;
    for r in reg_vars {
        let real_name = "$".to_string() + &r.name;
        result = result.replace(&real_name, &r.ty.get_real_value(r.id));
    }

    return result;
}

fn main() {
    let lex: lexer::Lexer = lexer::Lexer::new(fs::read_to_string("main.rstini").expect("Cannot open main.rstini"));

    let mut par: Parser = Parser::new(lex);

    par.start();
}
