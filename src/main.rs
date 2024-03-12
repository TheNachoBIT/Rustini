mod lexer;

use std::fs;
use std::process::Command;

use cranelift::codegen::entity::EntityRef;
use cranelift::codegen::ir::{AbiParam, UserFuncName, Function, InstBuilder, Signature};
use cranelift::codegen::isa::CallConv;
use cranelift::codegen::settings;
use cranelift::codegen::verifier::verify_function;
use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext, Variable};

use cranelift::prelude::Type;
use cranelift::prelude::types::*;

use cranelift::prelude::Value;

use cranelift::prelude::Imm64;

#[derive(Clone, PartialEq, Debug)]
enum RType {
    Int64,
    Int32,
    Void
}

impl RType {

    fn codegen(&self) -> Type {
        match self {
            RType::Int64 => I64,
            RType::Int32 => I32,
            _ => panic!("Type not supported for codegen.")
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
enum Expression {

    RLet {
        name: String,
        ty: RType
    },
    RNumber {
        val: f64
    },
    RFunction {
        name: String,
        ty: RType,
        instructions: Vec<Expression>
    },
    RVariable {
        name: String,
    },
    REquals {
        lvalue: Box<Expression>,
        rvalue: Box<Expression>
    },
    RRealReturn {
        ret: Box<Expression>
    },
    RNothing,
}

impl Expression {

    fn get_name(&self) -> String {
        match self {
            Expression::RLet { name, .. } => name.to_string(),
            Expression::RVariable { name } => name.to_string(),
            _ => "".to_string()
        }
    }

    fn contains_name(&self) -> bool {
        match self {
            Expression::RLet { .. } | Expression::RFunction { .. } | Expression::RVariable { .. } => true,
            _ => false
        };

        false
    }

    fn codegen_variable<'var>(&self, reg_vars: &'var [VariableInfo]) -> &'var Variable {

        match self {
            Expression::RVariable { name } | Expression::RLet { name, .. } => {
                find_cranelift_variable(name.to_string(), reg_vars)
            }
            _ => panic!("codegen_variable() has no use fo {:#?}.", self)
        }
    }

    fn codegen_value(&self, builder: &mut FunctionBuilder, ty: RType, reg_vars: &Vec<VariableInfo>) -> Value {
        match self {
            Expression::RNumber { val } => {
                builder.ins().iconst(ty.codegen(), *val as i64)
            },
            Expression::RVariable { name, .. } => {
                builder.use_var(*find_cranelift_variable(name.to_string(), reg_vars))
            },
            _ => panic!("codegen_variable() has no use for {:#?}.", self)
        }
    }

    fn codegen_function(&self, reg_vars: &mut Vec<VariableInfo>) -> Function {

        if let Expression::RFunction { name, ty, instructions } = self {

            let mut sig = Signature::new(CallConv::SystemV);
            sig.returns.push(AbiParam::new(ty.codegen()));
            let mut fn_builder_ctx = FunctionBuilderContext::new();
            let mut func = Function::with_name_signature(UserFuncName::user(0, 0), sig);
            {
                let mut builder = FunctionBuilder::new(&mut func, &mut fn_builder_ctx);

                let entry = builder.create_block();
                builder.switch_to_block(entry);

                for i in instructions {
                    i.codegen(&mut builder, reg_vars, ty.clone());
                }
            }

            return func;
        }

         panic!("codegen_function() has no use for this type.")
    }

    fn codegen(&self, builder: &mut FunctionBuilder, reg_vars: &mut Vec<VariableInfo>, func_ty: RType) {

        match self {
            Expression::RLet { name, ty } => {
                for v in reg_vars {
                    if &v.name == name {
                        v.cranelift_variable = Some(Variable::new(v.id));
                        builder.declare_var(v.cranelift_variable.unwrap(), ty.codegen());
                    }
                }
            },
            Expression::REquals { lvalue, rvalue } => {

                lvalue.codegen(builder, reg_vars, func_ty.clone());
                rvalue.codegen(builder, reg_vars, func_ty.clone());

                let lv = lvalue.codegen_variable(&reg_vars);
                let lv_ty = find_type(lvalue.get_name(), &reg_vars);

                let rv = rvalue.codegen_value(builder, lv_ty, reg_vars);

                builder.def_var(*lv, rv);
            },
            Expression::RRealReturn { ret } => {

                ret.codegen(builder, reg_vars, func_ty.clone());

                let rv = ret.codegen_value(builder, func_ty, reg_vars);

                builder.ins().return_(&[rv]);
            }
            _ => {}
        }
    }
}

fn find_type(name: String, reg_vars: &[VariableInfo]) -> RType {
    for r in reg_vars {
        if r.name == name {
            return r.ty.clone();
        }
    }

    panic!("Type not found!");
}

fn find_cranelift_variable(name: String, reg_vars: &[VariableInfo]) -> &Variable {
    for r in reg_vars {
        if r.name == name {
            match &r.cranelift_variable {
                Some(var) => return var,
                None => panic!("Cranelift Variable \"{}\" found as 'None'", &name)
            }
        }
    }

    panic!("Cranelift Variable \"{}\" not found", &name)
}

#[derive(Clone)]
struct VariableInfo {
    name: String,
    ty: RType,
    id: usize,
    moved: bool,
    cranelift_variable: Option<Variable>
}

impl VariableInfo {
    fn new(get_name: String, get_type: RType) -> Self {
        Self {
            name: get_name,
            ty: get_type,
            id: 0,
            moved: false,
            cranelift_variable: None
        }
    }
}

struct Parser {
    lex: lexer::Lexer,
    all_instructions: Vec<Expression>,
    all_registered_variables: Vec<VariableInfo>,
}

impl Parser {

    fn new(get_lex: lexer::Lexer) -> Self {
        Self {
            lex: get_lex,
            all_instructions: Vec::new(),
            all_registered_variables: Vec::new(),
        }
    }

    fn parse_expression(&mut self) -> Expression {
        let expr = self.parse_primary();

        return self.parse_binary_operator(expr);
    }

    fn parse_number(&mut self) -> Expression {

        let numb: f64 = self.lex.string_buffer.parse::<f64>().unwrap();

        self.lex.get_next_token();

        return Expression::RNumber { val: numb };
    }

    fn parse_return(&mut self) -> Expression {

        self.lex.get_next_token();

        let ret_val = self.parse_expression();

        return Expression::RRealReturn { ret: Box::new(ret_val) };
    }

    fn parse_primary(&mut self) -> Expression {

        match &self.lex.current_token {
            lexer::LexerToken::Let => self.parse_let(),
            lexer::LexerToken::Identifier(ident) => self.parse_identifier(ident.clone()),
            lexer::LexerToken::Number => self.parse_number(),
            lexer::LexerToken::Return => self.parse_return(),
            _ => panic!("Unknown primary identifier, found {:#?}", &self.lex.current_token)
        }
    }

    fn parse_identifier(&mut self, ident: String) -> Expression {

        self.lex.get_next_token();

        return Expression::RVariable { name: ident };
    }

    fn parse_equals(&mut self, lv: Expression) -> Expression {

        self.lex.get_next_token();

        let rv = self.parse_expression();

        return Expression::REquals { lvalue: Box::new(lv), rvalue: Box::new(rv) };
    }

    fn parse_binary_operator(&mut self, lv: Expression) -> Expression {

        match &self.lex.current_token {
            lexer::LexerToken::Char('=') => self.parse_equals(lv),
            lexer::LexerToken::Char(';') => lv,
            _ => panic!("Unknown binary operator, found {:#?}", &self.lex.current_token)
        }
    }

    fn parse_type(&mut self, ident: String) -> RType {

        match ident.as_str() {
            "i32" => RType::Int32,
            "i64" => RType::Int64,
            _ => panic!("Type \"{}\" does not exist", ident)
        }
    }

    fn parse_let(&mut self) -> Expression {
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

        let new_var_info = VariableInfo::new(get_name.clone(), get_type.clone());

        self.all_registered_variables.push(new_var_info);

        return Expression::RLet { name: get_name.to_string(), ty: get_type };
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

        return Expression::RFunction { name: get_name, ty: get_type, instructions: all_inst };
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

fn main() {
    let lex: lexer::Lexer = lexer::Lexer::new(fs::read_to_string("main.rstini").expect("Cannot open main.rstini"));

    let mut par: Parser = Parser::new(lex);

    par.start();
}
