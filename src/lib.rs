mod compiler;

use compiler::ToHVM;
use pyo3::prelude::*;
use rustpython_parser::parser;
use rustpython_parser::mode::Mode;
use hvm::syntax::{Term, Rule};

const MEMORY_SIZE: usize = 1 << 20;
const THREAD_NUM : usize = 4;

pub fn make_main_call(func_name: String) -> Rule {
  Rule {
    lhs: Box::new(Term::Ctr { name: "PYHVM_MAIN_CALL".into(), args: vec![] }),
    rhs: Box::new(Term::Ctr { name: func_name, args: vec![] } )
  }
}

#[pyfunction]
pub fn execute_as_hvm(func_name: String, code: &str) -> PyResult<String> {
  let program  = parser::parse(code, Mode::Program).unwrap();
  let mut ctx  = compiler::Ctx::new();
  program.compile(&mut ctx)?;
  let mainrule = make_main_call(func_name);
  ctx.file.rules.push(mainrule);
  let rulebook = hvm::language::rulebook::gen_rulebook(&ctx.file);
  let mut program  = hvm::runtime::Program::new();
  program.add_book(&rulebook);
  let heap = hvm::runtime::new_heap(MEMORY_SIZE, THREAD_NUM);
  let tids = hvm::runtime::new_tids(THREAD_NUM);
  hvm::runtime::link(&heap, 0, hvm::runtime::Fun(*rulebook.name_to_id.get("PYHVM_MAIN_CALL").unwrap(), 0));
  let host = 0;
  hvm::runtime::normalize(&heap, &program, &tids, host, false);
  let code = format!("{}", hvm::language::readback::as_term(&heap, &program, host));
  hvm::runtime::collect(&heap, &program.aris, tids[0], hvm::runtime::load_ptr(&heap, host));
  hvm::runtime::free(&heap, 0, 0, 1);
  Ok(code)
}

/// A Python module implemented in Rust.
#[pymodule]
fn PyHVM(_py: Python, m: &PyModule) -> PyResult<()> {
  m.add_function(pyo3::wrap_pyfunction!(execute_as_hvm, m)?)?;
  Ok(())
}
