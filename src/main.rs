mod lexer;

use std::fs;
use std::process::Command;

use cranelift::codegen::entity::EntityRef;
use cranelift::codegen::ir::{AbiParam, UserFuncName, Function, InstBuilder, Signature};
use cranelift::codegen::Context;
use cranelift::codegen::isa::CallConv;
use cranelift::codegen::settings;
use cranelift::codegen::verifier::verify_function;
use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext, Variable};

use cranelift::prelude::Type;
use cranelift::prelude::types::*;

use cranelift::prelude::Value;

use cranelift::prelude::Imm64;

use cranelift_object::ObjectBuilder;
use cranelift_object::ObjectModule;

use cranelift::prelude::Configurable;

use cranelift_module::Module;
use cranelift_module::Linkage;

use cranelift::prelude::isa;

use target_lexicon::Triple;

use std::fs::File;

use cranelift::prelude::isa::x64::settings::builder;

use std::collections::HashMap;

use std::fmt::Write;

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
    RAdd {
        target: Box<Expression>,
        lvalue: Box<Expression>,
        rvalue: Box<Expression>
    },
    RSub {
        target: Box<Expression>,
        lvalue: Box<Expression>,
        rvalue: Box<Expression>
    },
    RMul {
        target: Box<Expression>,
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
            },
            Expression::RAdd { target, .. } | Expression::RSub { target, .. } | Expression::RMul { target, .. } => {
                target.codegen_variable(reg_vars)
            }
            _ => panic!("codegen_variable() has no use for {:#?}.", self)
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
            Expression::RAdd { target, .. } | Expression::RSub { target, .. } | Expression::RMul { target, .. } => {
                builder.use_var(*target.codegen_variable(&reg_vars))
            },
            _ => panic!("codegen_value() has no use for {:#?}.", self)
        }
    }

    fn codegen_function(&self, reg_vars: &mut Vec<VariableInfo>) -> Function {

        if let Expression::RFunction { name, ty, instructions } = self {

            let mut shared_builder = settings::builder();
            shared_builder.enable("is_pic").unwrap();
            let shared_flags = settings::Flags::new(shared_builder);
    
            let isa_builder = isa::lookup(Triple::host()).unwrap();
            let isa = isa_builder.finish(shared_flags).unwrap();
            let call_conv = isa.default_call_conv();

            let obj_builder =
                ObjectBuilder::new(isa, &**name, cranelift_module::default_libcall_names()).unwrap();
            let mut obj_module = ObjectModule::new(obj_builder);

            let mut sig = Signature::new(CallConv::SystemV);
            sig.returns.push(AbiParam::new(ty.codegen()));

            let fid = obj_module
                .declare_function(&**name, Linkage::Export, &sig)
                .unwrap();

            let mut fn_builder_ctx = FunctionBuilderContext::new();
            let mut func = Function::with_name_signature(UserFuncName::user(0, 0), sig);
            {
                let mut builder = FunctionBuilder::new(&mut func, &mut fn_builder_ctx);

                let entry = builder.create_block();
                builder.switch_to_block(entry);

                for i in instructions {
                    i.codegen(&mut builder, reg_vars, ty.clone());
                }

                builder.seal_all_blocks();
                builder.finalize();
            }

            let mut context = Context::for_function(func.clone());
            obj_module.define_function(fid, &mut context).unwrap();

            let res = obj_module.finish();

            let mut file = File::create(name.to_string() + ".o").unwrap();
            res.object.write_stream(&mut file).unwrap();

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
            Expression::RAdd { target, lvalue, rvalue } | 
            Expression::RSub { target, lvalue, rvalue } | 
            Expression::RMul { target, lvalue, rvalue } => {

                lvalue.codegen(builder, reg_vars, func_ty.clone());
                rvalue.codegen(builder, reg_vars, func_ty.clone());

                let lv_ty = if lvalue.get_name() != "" { find_type(lvalue.get_name(), &reg_vars) } else { func_ty.clone() };

                let lv = lvalue.codegen_value(builder, lv_ty.clone(), reg_vars);
                let rv = rvalue.codegen_value(builder, lv_ty, reg_vars);

                let equation: Value = match self {
                    Expression::RAdd { .. } => builder.ins().iadd(lv, rv),
                    Expression::RSub { .. } => builder.ins().isub(lv, rv),
                    Expression::RMul { .. } => builder.ins().imul(lv, rv),
                    _ => todo!()
                };

                builder.def_var(*target.codegen_variable(&reg_vars), equation);
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
    fn new(get_name: String, get_type: RType, get_id: usize) -> Self {
        Self {
            name: get_name,
            ty: get_type,
            id: get_id,
            moved: false,
            cranelift_variable: None
        }
    }
}

struct Parser {
    lex: lexer::Lexer,
    all_instructions: Vec<Expression>,
    all_registered_variables: Vec<VariableInfo>,
    variable_id: usize,
    binary_precedence: HashMap<String, i64>,
}

fn init_binary_precedence() -> HashMap<String, i64> {

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

    fn new(get_lex: lexer::Lexer) -> Self {
        Self {
            lex: get_lex,
            all_instructions: Vec::new(),
            all_registered_variables: Vec::new(),
            variable_id: 0,
            binary_precedence: init_binary_precedence(),
        }
    }

    fn parse_expression(&mut self, target: Option<Expression>) -> Expression {
        let expr = self.parse_primary();

        let final_target = match target.clone() {
            Some(t) => target,
            None => Some(expr.clone())
        };

        return self.parse_binary_operator(0, expr, final_target);
    }

    fn parse_number(&mut self) -> Expression {

        let numb: f64 = self.lex.string_buffer.parse::<f64>().unwrap();

        self.lex.get_next_token();

        return Expression::RNumber { val: numb };
    }

    fn parse_return(&mut self) -> Expression {

        self.lex.get_next_token();

        let ret_val = self.parse_expression(None);

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

    fn parse_equals(&mut self, lv: Expression, rv: Expression) -> Expression {

        return Expression::REquals { lvalue: Box::new(lv), rvalue: Box::new(rv) };
    }

    fn parse_add(&mut self, lv: Expression, rv: Expression, target: Option<Expression>) -> Expression {

        let final_target = if let Some(t) = target { t } else { lv.clone() };

        return Expression::RAdd { target: Box::new(final_target), lvalue: Box::new(lv), rvalue: Box::new(rv) };
    }

    fn parse_sub(&mut self, lv: Expression, rv: Expression, target: Option<Expression>) -> Expression {

        let final_target = if let Some(t) = target { t } else { lv.clone() };

        return Expression::RSub { target: Box::new(final_target), lvalue: Box::new(lv), rvalue: Box::new(rv) };
    }

    fn parse_mul(&mut self, lv: Expression, rv: Expression, target: Option<Expression>) -> Expression {

        let final_target = if let Some(t) = target { t } else { lv.clone() };

        return Expression::RMul { target: Box::new(final_target), lvalue: Box::new(lv), rvalue: Box::new(rv) };
    }

    fn get_token_precedence(&self, tok: String) -> i64 {
        return self.binary_precedence.get(&tok).copied().unwrap_or(-1);
    }

    fn create_binary(&mut self, op: String, lv: Expression, rv: Expression, target: Option<Expression>) -> Expression {

        match op.as_str() {
            "=" => self.parse_equals(lv, rv),
            "+" => self.parse_add(lv, rv, target),
            "-" => self.parse_sub(lv, rv, target),
            "*" => self.parse_mul(lv, rv, target),
            ";" => lv,
            _ => panic!("Unknown binary operator, found {:#?}", &op)
        }
    }

    fn parse_binary_operator(&mut self, expr_precedence: i64, lv: Expression, target: Option<Expression>) -> Expression {

        let mut grab_lv = lv;

        while true {

            let mut left_tok_string: String = if let lexer::LexerToken::Char(c) = self.lex.current_token { String::from(c) } else { String::from("") };

            if left_tok_string == ";" { return grab_lv; }

            self.lex.get_next_token();

            if let lexer::LexerToken::Char(c) = self.lex.current_token {
                left_tok_string.write_char(c).unwrap();
                self.lex.get_next_token();
            }

            let precedence = self.get_token_precedence(left_tok_string.clone());

            if precedence < expr_precedence { return grab_lv; }

            let mut grab_rv = self.parse_primary();

            let right_tok_string: String = if let lexer::LexerToken::Char(c) = self.lex.current_token { String::from(c) } else { String::from("") };

            let next_precedence = self.get_token_precedence(right_tok_string);

            if precedence < next_precedence {
                grab_rv = self.parse_binary_operator(precedence + 1, grab_rv, target.clone());
            }

            grab_lv = self.create_binary(left_tok_string, grab_lv, grab_rv, target.clone());
        }

        panic!("What");
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

        let new_var_info = VariableInfo::new(get_name.clone(), get_type.clone(), self.variable_id);

        self.variable_id += 1;

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

            let expr: Expression = self.parse_expression(None);

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
