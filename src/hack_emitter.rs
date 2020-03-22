use crate::hack_parser::*;
use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
  static ref COMPUTATION_INSTRUCTIONS: HashMap<&'static str, &'static str> = [
    ("0", "0101010"),
    ("1", "0111111"),
    ("-1", "0111010"),
    ("D", "0001100"),
    ("A", "0110000"),
    ("!D", "0001101"),
    ("!A", "0110001"),
    ("-D", "0001111"),
    ("-A", "0110011"),
    ("D+1", "0011111"),
    ("A+1", "0110111"),
    ("D-1", "0001110"),
    ("A-1", "0110010"),
    ("D+A", "0000010"),
    ("D-A", "0010011"),
    ("A-D", "0000111"),
    ("D&A", "0000000"),
    ("D|A", "0010101"),
    ("M", "1110000"),
    ("!M", "1110001"),
    ("-M", "1110011"),
    ("M+1", "1110111"),
    ("M-1", "1110010"),
    ("D+M", "1000010"),
    ("D-M", "1010011"),
    ("M-D", "1000111"),
    ("D&M", "1000000"),
    ("D|M", "1010101"),
  ]
  .iter()
  .cloned()
  .collect();
}

pub fn emit(instructions: Vec<Instruction>) -> String {
  instructions
    .iter()
    .map(|instruction| match instruction {
      Instruction::A(instruction) =>
        match instruction {
          AInstruction::number(number) => format!("0{:015b}\n", number),
          AInstruction::label(label) => panic!("I found an unconverted label `{}` in the A instruction.\nAll labels should be converted by the parser before emitting binary code.", label.value),
        }
      Instruction::C {
        destinations,
        computation,
        jump,
      } => format!(
        "111{}{}{}\n",
        emit_computation(computation),
        emit_destinations(destinations),
        emit_jump(jump)
      ),
      Instruction::Ignored => "".to_string(),
    })
    .collect::<Vec<String>>()
    .join("")
}

fn emit_destinations(destinations: &Option<Destinations>) -> String {
  match destinations {
    Some(dest) => format!(
      "{}{}{}",
      bool_to_bit(dest.A),
      bool_to_bit(dest.D),
      bool_to_bit(dest.M)
    ),
    None => "000".to_string(),
  }
}

fn emit_computation(computation: &String) -> String {
  COMPUTATION_INSTRUCTIONS
    .get::<str>(&computation)
    .unwrap()
    .to_string()
}

fn emit_jump(jump: &Option<Jump>) -> String {
  match jump {
    Some(j) => format!(
      "{}{}{}",
      bool_to_bit(j.LT),
      bool_to_bit(j.EQ),
      bool_to_bit(j.GT)
    ),
    None => "000".to_string()
  }
}

fn bool_to_bit(boolean: bool) -> String {
  (if boolean { "1" } else { "0" }).to_string()
}
