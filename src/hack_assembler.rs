use crate::hack_parser;
use crate::hack_emitter;

pub fn assemble(source: &str) -> Result<String, String>
{
  hack_parser::parse(source).map(
      |ast| hack_emitter::emit(ast)
  )
}