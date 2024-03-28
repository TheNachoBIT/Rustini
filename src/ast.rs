use cranelift::codegen::entity::EntityRef;
use cranelift::codegen::ir::{AbiParam, UserFuncName, Function, InstBuilder, Signature};
use cranelift::codegen::Context;
use cranelift::codegen::isa::CallConv;
use cranelift::codegen::settings;

use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext, Variable};

use cranelift::prelude::Type;
use cranelift::prelude::types::*;

use cranelift::prelude::Value;

use cranelift_object::ObjectBuilder;
use cranelift_object::ObjectModule;

use cranelift::prelude::Configurable;

use cranelift_module::Module;
use cranelift_module::Linkage;

use cranelift::prelude::isa;

use target_lexicon::Triple;

use std::fs::File;

#[derive(Clone, PartialEq, Debug, PartialOrd, Eq, Ord)]
pub enum RType {
    Void,
    Bool,
    Int8,
    Int16,
    Int32,
    Int64,
    Float32,
    Float64,
}

impl RType {

    pub fn codegen(&self) -> Type {
        match self {
            RType::Int64 => I64,
            RType::Int32 => I32,
            RType::Int16 => I16,
            RType::Int8 | RType::Bool => I8,
            RType::Float32 => F32,
            RType::Float64 => F64,
            _ => panic!("Type not supported for codegen.")
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {

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
    RAs {
        target: Box<Expression>,
        val: Box<Expression>,
        ty: RType,
    },
    RRealReturn {
        ret: Box<Expression>
    },
    RCompare {
        target: Box<Expression>,
        lvalue: Box<Expression>,
        rvalue: Box<Expression>,
        operator: String,
    },
    RNothing,
}

pub fn is_float(ty: &RType) -> bool {
    match ty {
        RType::Float32 | RType::Float64 => true,
        _ => false
    }
}

impl Expression {

    pub fn get_name(&self) -> String {
        match self {
            Expression::RLet { name, .. } => name.to_string(),
            Expression::RVariable { name } => name.to_string(),
            _ => "".to_string()
        }
    }

    pub fn contains_name(&self) -> bool {
        match self {
            Expression::RLet { .. } | Expression::RFunction { .. } | Expression::RVariable { .. } => true,
            _ => false
        };

        false
    }

    pub fn find_rtype(&self, default_if_fails: RType, reg_vars: &[VariableInfo]) -> RType {

        match self {
            Expression::RLet { ty, .. } | Expression::RAs { ty, .. } => {
                ty.clone()
            },
            Expression::RVariable { name } => {
                find_type(name.to_string(), reg_vars)
            }
            _ => default_if_fails
        }
    }

    pub fn codegen_variable<'var>(&self, reg_vars: &'var [VariableInfo]) -> &'var Variable {

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

    pub fn codegen_value(&self, builder: &mut FunctionBuilder, ty: RType, reg_vars: &Vec<VariableInfo>) -> Value {
        match self {
            Expression::RNumber { val } => {
                if val.fract() == 0.0 {
                    builder.ins().iconst(ty.codegen(), *val as i64)
                }
                else {
                    match &ty {
                        RType::Float32 => builder.ins().f32const(*val as f32),
                        RType::Float64 => builder.ins().f64const(*val),
                        _ => panic!("Type not compatible in codegen!")
                    }
                }
            },
            Expression::RVariable { name, .. } => {
                builder.use_var(*find_cranelift_variable(name.to_string(), reg_vars))
            },
            Expression::RAdd { target, .. } | 
            Expression::RSub { target, .. } | 
            Expression::RMul { target, .. } => {
                builder.use_var(*target.codegen_variable(&reg_vars))
            },
            Expression::RAs { val, ty, .. } => {

                let val_ty = val.find_rtype(ty.clone(), reg_vars);
                let val_cg = val.codegen_value(builder, ty.clone(), reg_vars);

                let cast = if val_ty < *ty { 
                    builder.ins().sextend(ty.codegen(), val_cg)
                }
                else {
                    builder.ins().ireduce(ty.codegen(), val_cg)
                };

                cast
            }
            _ => panic!("codegen_value() has no use for {:#?}.", self)
        }
    }

    pub fn codegen_function(&self, reg_vars: &mut Vec<VariableInfo>) -> Function {

        if let Expression::RFunction { name, ty, instructions } = self {

            let mut shared_builder = settings::builder();
            shared_builder.enable("is_pic").unwrap();
            let shared_flags = settings::Flags::new(shared_builder);
    
            let isa_builder = isa::lookup(Triple::host()).unwrap();
            let isa = isa_builder.finish(shared_flags).unwrap();
            let _call_conv = isa.default_call_conv();

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

    pub fn codegen(&self, builder: &mut FunctionBuilder, reg_vars: &mut Vec<VariableInfo>, func_ty: RType) {

        match self {
            Expression::RLet { name, ty } => {
                for v in reg_vars {
                    if &v.name == name && v.cranelift_variable == None {
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

                let final_ty = target.find_rtype(func_ty.clone(), &reg_vars);

                lvalue.codegen(builder, reg_vars, final_ty.clone());
                rvalue.codegen(builder, reg_vars, final_ty.clone());

                let lv_ty = lvalue.find_rtype(final_ty.clone(), &reg_vars);

                let lv = lvalue.codegen_value(builder, lv_ty.clone(), reg_vars);
                let rv = rvalue.codegen_value(builder, lv_ty.clone(), reg_vars);

                let equation: Value = match self {
                    Expression::RAdd { .. } => if !is_float(&lv_ty.clone()) { builder.ins().iadd(lv, rv) } else { builder.ins().fadd(lv, rv) } ,
                    Expression::RSub { .. } => if !is_float(&lv_ty.clone()) { builder.ins().isub(lv, rv) } else { builder.ins().fsub(lv, rv) } ,
                    Expression::RMul { .. } => if !is_float(&lv_ty.clone()) { builder.ins().imul(lv, rv) } else { builder.ins().fmul(lv, rv) } ,
                    _ => todo!()
                };

                builder.def_var(*target.codegen_variable(&reg_vars), equation);
            },
            Expression::RRealReturn { ret } => {

                ret.codegen(builder, reg_vars, func_ty.clone());

                let rv = ret.codegen_value(builder, func_ty, reg_vars);

                builder.ins().return_(&[rv]);
            },
            _ => {}
        }
    }
}

pub fn find_type(name: String, reg_vars: &[VariableInfo]) -> RType {
    for r in reg_vars {
        if r.name == name {
            return r.ty.clone();
        }
    }

    panic!("Type not found!");
}

pub fn find_cranelift_variable(name: String, reg_vars: &[VariableInfo]) -> &Variable {
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
pub struct VariableInfo {
    pub name: String,
    pub ty: RType,
    pub id: usize,
    pub moved: bool,
    pub cranelift_variable: Option<Variable>
}

impl VariableInfo {
    pub fn new(get_name: String, get_type: RType, get_id: usize) -> Self {
        Self {
            name: get_name,
            ty: get_type,
            id: get_id,
            moved: false,
            cranelift_variable: None
        }
    }
}