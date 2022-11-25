#![allow(unused_variables)]
use pyo3::prelude::*;
use pyo3::exceptions::PyNotImplementedError;
//use pyo3::Python;
use rustpython_parser::ast::*;
use hvm::Term;
use hvm::syntax::File;
use hvm::syntax::Oper;
use hvm::syntax::Rule;
use std::collections::HashMap;
use num_traits::cast::ToPrimitive;

#[pyclass]
pub struct Ctx {
  pub vars: HashMap<String, Term>,
  pub curr_rule: String,
  pub file: File,
}

impl Ctx {
  pub fn new() -> Self {
    Ctx { vars: HashMap::new(),
          curr_rule : "".into(),
          file: File { rules: vec![], smaps: vec![]}
    }
  }
}

#[inline(always)]
fn none() -> Box<Term> {
  Box::new(Term::Ctr { name: "None".into(), args: vec![]})
}

pub trait ToHVM<T> {
  fn compile(&self, ctx: &mut Ctx) -> PyResult<T>;
}

impl ToHVM<()> for Top {
  fn compile(&self, ctx: &mut Ctx) -> PyResult<()> {
    match self {
      Top::Program(Program { statements }) => {
        for stmt in statements {
          let f   = stmt.compile(ctx)?;
          let _ = f(none());
        };
        Ok(())
      },
      Top::Statement(_) => todo!(),
      Top::Expression(_) => todo!(),
    }
  }
}

impl ToHVM<Vec<Box<Term>>> for Parameters {
  fn compile(&self, ctx: &mut Ctx) -> PyResult<Vec<Box<Term>>> {
    self.args.iter().map(|x| x.compile(ctx)).collect()
  }
}
impl ToHVM<Box<Term>> for Parameter {
  fn compile(&self, ctx: &mut Ctx) -> PyResult<Box<Term>> {
    let Parameter { location: _, arg, annotation: _} = self;
    let name = arg.clone();
    let var = Term::Var { name };
    Ok(Box::new(var))
  }
}


impl ToHVM<Box<Term>> for Number {
  fn compile(&self, _ctx: &mut Ctx) -> PyResult<Box<Term>> {
    match self {
      rustpython_parser::ast::Number::Integer { value } => Ok(Box::new(Term::U6O { numb: value.to_u64().unwrap() } )) ,
      rustpython_parser::ast::Number::Float { value } => todo!(),
      rustpython_parser::ast::Number::Complex { real, imag } => todo!(),
    }
  }
}

impl ToHVM<Box<Term>> for Vec<&StatementType> {
  fn compile(&self, ctx: &mut Ctx) -> PyResult<Box<Term>> {
    self.iter().rev().fold(Ok(none()), |acc, &func| {
      let func = func.compile(ctx);
      match func {
        Ok(func) => acc.map(func),
        Err(err) => Err(err)
      }
    })
  }
}

impl ToHVM<Box<Term>> for Vec<Statement> {
  fn compile(&self, ctx: &mut Ctx) -> PyResult<Box<Term>> {
    self.iter().rev().fold(Ok(none()), |acc, func| {
      let func = func.compile(ctx);
      match func {
        Ok(func) => acc.map(func),
        Err(err) => Err(err)
      }
    })
  }
}

impl ToHVM<Vec<Box<Term>>> for Vec<Expression> {
  fn compile(&self, ctx: &mut Ctx) -> PyResult<Vec<Box<Term>>> {
    self.iter().map(|x| x.compile(ctx)).collect()
  }
}

impl ToHVM<Box<dyn FnOnce(Box<Term>) -> Box<Term>>> for StatementType
{
  // ngl this type looks a little bit messy
  fn compile(&self, ctx: &mut Ctx) -> PyResult<Box<dyn FnOnce(Box<Term>) -> Box<Term>>> {
    match self {
      StatementType::Assign { targets, value } => {
        if let [Located{ location:_, node: ExpressionType::Identifier { name } }] = targets.as_slice() {
          let name = name.clone();
          let expr = value.compile(ctx)?;
          Ok(Box::new(move |body| Box::new(Term::Let {name, expr, body})))
        }
        else {
          Err(PyNotImplementedError::new_err("Multiple assignment is not supported yet"))
        }
      }
      StatementType::Return { value } => {
        if let Some(val) = value {
          let val = val.compile(ctx)?;
          Ok(Box::new(move |_| val))
        }
        else {
          Ok(Box::new(|_| none()))
        }
      }
      StatementType::Break => todo!(),
      StatementType::Continue => todo!(),
      StatementType::Import { names } => todo!(),
      StatementType::ImportFrom { level, module, names } => todo!(),
      StatementType::Pass => todo!(),
      StatementType::Assert { test, msg } => todo!(),
      StatementType::Delete { targets } => todo!(),
      StatementType::AugAssign { target, op, value } => todo!(),
      StatementType::AnnAssign { target, annotation, value } => todo!(),
      StatementType::Expression { expression } => todo!(),
      StatementType::Global { names } => todo!(),
      StatementType::Nonlocal { names } => todo!(),
      StatementType::If { test, body, orelse } => todo!(),
      StatementType::While { test, body, orelse } => todo!(),
      StatementType::With { is_async, items, body } => todo!(),
      StatementType::For { is_async, target, iter, body, orelse } => todo!(),
      StatementType::Raise { exception, cause } => todo!(),
      StatementType::Try { body, handlers, orelse, finalbody } => todo!(),
      StatementType::ClassDef { name, body, bases, keywords, decorator_list } => todo!(),
      StatementType::FunctionDef { is_async, name, args, body, decorator_list, returns } => {
        let name = name.clone();
        let args = (*args).compile(ctx)?;
        let lhs = Box::new(Term::Ctr { name, args });
        let rhs = (*body).compile(ctx)?;
        let rule = Rule { lhs, rhs };
        ctx.file.rules.push(rule);
        Ok(Box::new(|_| none()))
      },
    }
  }
}

// just throw away the locations
impl<T, S> ToHVM<S> for Located<T>
where
  T: ToHVM<S>,
{
  fn compile(&self, ctx: &mut Ctx) -> PyResult<S> {
    let Located {location: _, node} = self;
    node.compile(ctx)
  }
}

impl ToHVM<Box<Term>> for ExpressionType {
  fn compile(&self, ctx: &mut Ctx) -> PyResult<Box<Term>> {
    match self {
      ExpressionType::BoolOp { op, values } => todo!(),
      ExpressionType::Binop { a, op, b } => {
        let val0 = a.compile(ctx)?;
        let val1 = b.compile(ctx)?;
        let oper = match op {
          Operator::Add      => Ok(Oper::Add),
          Operator::Sub      => Ok(Oper::Sub),
          Operator::Mult     => Ok(Oper::Mul),
          Operator::Div      => Ok(Oper::Div),
          Operator::Mod      => Ok(Oper::Mod),
          Operator::LShift   => Ok(Oper::Shl),
          Operator::RShift   => Ok(Oper::Shr),
          Operator::BitOr    => Ok(Oper::Or ),
          Operator::BitXor   => Ok(Oper::Xor),
          Operator::BitAnd   => Ok(Oper::And),
          _ => Err(PyNotImplementedError::new_err("Operation not implemented yet."))
        }?;
        Ok(Box::new(Term::Op2 { oper, val0, val1 }))
      },
      ExpressionType::Subscript { a, b } => todo!(),
      ExpressionType::Unop { op, a } => todo!(),
      ExpressionType::Await { value } => todo!(),
      ExpressionType::Yield { value } => todo!(),
      ExpressionType::YieldFrom { value } => todo!(),
      ExpressionType::Compare { vals, ops } => todo!(),
      ExpressionType::Attribute { value, name } => todo!(),
      ExpressionType::Call { function, args, keywords } => {
        let args = args.compile(ctx)?;
        let func = &function.node;
        let term = match func {
          ExpressionType::Identifier { name } => {
            let name = String::from("Function.") + name;
            Box::new(Term::Ctr { name, args })
          },
          other => {
            let func = function.compile(ctx)?;
            args.into_iter().fold(func, |acc, arg| Box::new(Term::App { func: acc, argm: arg }))
          }
        };
        Ok(term)
      },
      ExpressionType::Number { value } => value.compile(ctx),
      ExpressionType::List { elements } => todo!(),
      ExpressionType::Tuple { elements } => todo!(),
      ExpressionType::Dict { elements } => todo!(),
      ExpressionType::Set { elements } => todo!(),
      ExpressionType::Comprehension { kind, generators } => todo!(),
      ExpressionType::Starred { value } => todo!(),
      ExpressionType::Slice { elements } => todo!(),
      ExpressionType::String { value } => todo!(),
      ExpressionType::Bytes { value } => todo!(),
      ExpressionType::Identifier { name } => todo!(),
      ExpressionType::Lambda { args, body } => todo!(),
      ExpressionType::IfExpression { test, body, orelse } => todo!(),
      ExpressionType::NamedExpression { left, right } => todo!(),
      ExpressionType::True => Ok(Box::new(Term::U6O {numb: 1})),
      ExpressionType::False => Ok(Box::new(Term::U6O {numb: 0})),
      ExpressionType::None => Ok(none()),
      ExpressionType::Ellipsis => todo!(),
    }
  }
}

// impl ToHVM<Box<Term>> for PyAny {
//   // try to transform objects in pure python to HVM's term
//   fn compile(&self, ctx: &mut Ctx) -> PyResult<Box<Term>> {
//     match  {

//     }
//   }
// }
