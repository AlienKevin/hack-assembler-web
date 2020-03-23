#[path = "parser.rs"]
mod parser;

use lazy_static::lazy_static;
use parser::*;
use std::collections::HashSet;
use im::hashmap::HashMap;

lazy_static! {
  static ref COMPUTATION_INSTRUCTIONS: HashSet<&'static str> = vec![
    "0",
    "1",
    "-1",
    "D",
    "A",
    "!D",
    "!A",
    "-D",
    "-A",
    "D+1",
    "A+1",
    "D-1",
    "A-1",
    "D+A",
    "D-A",
    "A-D",
    "D&A",
    "D|A",
    "M",
    "!M",
    "-M",
    "M+1",
    "M-1",
    "D+M",
    "D-M",
    "M-D",
    "D&M",
    "D|M",
  ]
  .into_iter()
  .collect();
}

#[derive(Debug, Clone)]
pub enum AInstruction {
  number(usize),
  label(Located<String>),
}

#[derive(Debug, Clone)]
pub enum Instruction {
  A(AInstruction),
  C {
    destinations: Option<Destinations>,
    computation: String,
    jump: Option<Jump>,
  },
  Ignored,
}

#[derive(Copy, Clone, Debug)]
pub struct Destinations {
  pub A: bool,
  pub D: bool,
  pub M: bool,
}

#[derive(Clone, Debug)]
pub struct Jump {
  pub LT: bool,
  pub EQ: bool,
  pub GT: bool,
}

#[derive(Clone, Debug)]
pub struct State {
  symbol_table: HashMap<String, usize>,
  instruction_index: usize,
  variable_index: usize,
}

struct SingleParseError {
  message: String,
  from: Location,
  to: Location,
  state: State,
}

pub fn parse<'a>(source: &'a str) -> Result<Vec<Instruction>, String> {
  let initial_table: HashMap<String, usize> = vec![("R0", 0),
    ("R1", 1),
    ("R2", 2),
    ("R3", 3),
    ("R4", 4),
    ("R5", 5),
    ("R6", 6),
    ("R7", 7),
    ("R8", 8),
    ("R9", 9),
    ("R10", 10),
    ("R11", 11),
    ("R12", 12),
    ("R13", 13),
    ("R14", 14),
    ("R15", 15),
    ("SCREEN", 16384),
    ("KBD", 24576),
    ("SP", 0),
    ("LCL", 1),
    ("ARG", 2),
    ("THIS", 3),
    ("THAT", 4),
    ].iter().cloned().map(|(key, value)| (key.to_string(), value)).collect();
  let initial_state = State {
    symbol_table: initial_table,
    instruction_index: 0,
    variable_index: 16,
  };
  let parser = one_or_more_till_end(|input, location, state: State| match maybe_indented(token("@")).parse(input, location, state.clone()) {
    ParseResult::ParseOk { .. } => maybe_indented(a_instruction()).parse(input, location, state),
    ParseResult::ParseError { .. } => maybe_indented(either(other(), c_instruction())).parse(input, location, state),
  }).map_with_state(
    |instructions, state|
      instructions.iter().map(|instruction|
        match instruction {
          Instruction::A(a_instruction) =>
            match a_instruction {
              AInstruction::label(label) =>
              {
                match state.symbol_table.get(&label.value) {
                  Some(index) => Ok(Instruction::A(AInstruction::number(*index))),
                  None => Err(
                    SingleParseError {
                      message: format!("I found an undefined goto label `{}`", label.value),
                      from: label.from,
                      to: label.to,
                      state: state.clone(),
                    }
                  )
                }
              },
              _ => Ok(Instruction::A(a_instruction.clone())),
            },
          _ => Ok(instruction.clone()),
        }
      ).collect::<Result<Vec<Instruction>, SingleParseError>>()
  );
  let output = parser.parse(source, Location { row: 1, col: 1 }, initial_state);
  match output {
    ParseResult::ParseOk { output, .. } =>
      match output {
        Ok(output) => Ok(output),
        Err(
          SingleParseError {
            message: error_message,
            from,
            to,
            ..
          }
        ) => Err(display_error(source, error_message, from, to)),
      }
    ParseResult::ParseError {
      message: error_message,
      from,
      to,
      ..
    } => Err(display_error(source, error_message, from, to)),
  }
}

// @3
fn a_instruction<'a>() -> BoxedParser<'a, Instruction, State> {
  right(
    token("@"),
    choose3(
      left(
        whole_decimal().pred(|number| *number <= 32767, "a decimal number <= 32767 (2^15 - 1)").map(|number| AInstruction::number(number)),
        newline_with_comment("//"),
      ),
      left(
        located(variable_label()).update_state(|label, state|
        if !state.symbol_table.contains_key::<str>(&label.value) {
          let new_symbol_table = state.symbol_table.update(label.value.clone(), state.variable_index);
          State {
            variable_index: state.variable_index + 1,
            symbol_table: new_symbol_table,
            ..state
          }
        } else {
          state
        }
        ).map(|label|
          AInstruction::label(label)
        ),
        newline_with_comment("//"),
      ),
      left(
        located(goto_label()).map(|label| AInstruction::label(label)),
        newline_with_comment("//"),
      ),
    )
  )
  .map(|instruction| Instruction::A(instruction))
  .update_state(move | _output, state |
    State {
      instruction_index: state.instruction_index + 1,
      ..state
    }
  )
}

pub fn variable_label<'a>() -> BoxedParser<'a, String, State> {
  one_or_more(
    any_char().pred(
    | character |
      character.is_lowercase() || character.is_digit(10)
    , "a static variable name optionally ended with `.` and some number like `counter` and `var.0`"
    )
  ).and_then(|name_chars| {
    let name = name_chars.iter().collect::<String>();
    optional(name.clone(), right(
      token("."),
      whole_decimal(),
    ).map(move |index| name.clone() + &index.to_string()))
  }).and_then(
    | label | {
      if let Some(digit_index) = label.find(|c: char | c.is_digit(10)) {
        if digit_index == 0 {
        return BoxedParser::new(move | _input, location: Location, state |
          ParseResult::ParseError {
            message: format!(
              "I'm expecting an all-lowercase variable name like `my_var` but found `{}`.",
              label
            ),
            from: Location {
              col: location.col - label.len(),
              ..location
            },
            to: location,
            state,
          }
        )
      }
      }
      BoxedParser::new(move | input, location, state |
        ParseResult::ParseOk {
          input, location, output: label.clone(), state,
        }
      )
    }
  )
}

pub fn goto_label<'a>() -> BoxedParser<'a, String, State> {
  one_or_more(
    any_char().pred(
    | character |
      character.is_alphanumeric() || *character == '_' || *character == '.' || *character == '$'
    , "a goto label like `LOOP_ONE` or `ponggame.run$if_end1`"
    )
  ).and_then(
    | characters | {
      let label = characters.iter().collect::<String>();
      if label.starts_with("_") || label.ends_with("_") || label.contains("__")
        || label.starts_with(".") || label.ends_with(".") || label.contains("..")
        || label.starts_with("$") || label.ends_with("$") || label.contains("$$") {
        BoxedParser::new(move | _input, location: Location, state |
          ParseResult::ParseError {
            message: "I'm expecting an all-caps goto label like LOOP_ONE".to_string(),
            from: Location {
              col: location.col - label.len(),
              ..location
            },
            to: location,
            state,
          }
        )
      } else {
        BoxedParser::new(move | input, location, state |
          ParseResult::ParseOk {
            input, location, output: label.clone(), state,
          }
        )
      }
    }
  )
}

// D=D+A;JMP
fn c_instruction<'a>() -> BoxedParser<'a, Instruction, State> {
  optional(
    None,
    left(
      destinations().map(|destinations| Some(destinations)),
      token("="),
    ),
  )
  .and_then(|destinations| {
    one_or_more(any_char().pred(
      |character| *character != ';' && *character != '\n' && *character != '\r' && *character != ' ' && *character != '/',
      "a computation instruction",
    ))
    .and_then(|characters| {
      let comp_instruction = characters.iter().collect::<String>();
      move |input, location, state|
        match COMPUTATION_INSTRUCTIONS.get::<str>(&comp_instruction) {
          Some(instruction) =>
            ParseResult::ParseOk {
              input,
              location,
              output: instruction,
              state,
            },
          None =>
            ParseResult::ParseError {
              message: format!("I can't find a computation instruction matching `{}`.\nTry something like `D+1` and `0`.", comp_instruction),
              from: Location {
                row: location.row,
                col: location.col - comp_instruction.len(),
              },
              to: location,
              state,
            }
        }
    }
    )
    .and_then(move |computation| {
      left(
        optional(
          None,
          right(
            token(";"),
            choose3(
              choose3(
                token("JGT").map(|_| Jump {
                  LT: false,
                  EQ: false,
                  GT: true,
                }),
                token("JEQ").map(|_| Jump {
                  LT: false,
                  EQ: true,
                  GT: false,
                }),
                token("JGE").map(|_| Jump {
                  LT: false,
                  EQ: true,
                  GT: true,
                }),
              ),
              choose3(
                token("JLT").map(|_| Jump {
                  LT: true,
                  EQ: false,
                  GT: false,
                }),
                token("JNE").map(|_| Jump {
                  LT: true,
                  EQ: false,
                  GT: true,
                }),
                token("JLE").map(|_| Jump {
                  LT: true,
                  EQ: true,
                  GT: false,
                }),
              ),
              token("JMP").map(|_| Jump {
                LT: true,
                EQ: true,
                GT: true,
              }),
            ),
          )
          .map(|jump| Some(jump)),
        )
        .map(move |jump| Instruction::C {
          destinations,
          computation: computation.to_string(),
          jump,
        }),
        newline_with_comment("//"),
      )
    })
  }).update_state(|_output, state| {
    State {
      instruction_index: state.instruction_index + 1,
      ..state
    }
  })
}

fn destinations<'a>() -> impl Parser<'a, Destinations, State> {
  choose3(
    choose3(
      token("AMD").map(|_| Destinations {
        A: true,
        D: true,
        M: true,
      }),
      token("AD").map(|_| Destinations {
        A: true,
        D: true,
        M: false,
      }),
      token("AM").map(|_| Destinations {
        A: true,
        D: false,
        M: true,
      }),
    ),
    choose3(
      token("MD").map(|_| Destinations {
        A: false,
        D: true,
        M: true,
      }),
      token("M").map(|_| Destinations {
        A: false,
        D: false,
        M: true,
      }),
      token("D").map(|_| Destinations {
        A: false,
        D: true,
        M: false,
      }),
    ),
    token("A").map(|_| Destinations {
      A: true,
      D: false,
      M: false,
    }),
  )
}

fn other<'a>() -> BoxedParser<'a, Instruction, State> {
  BoxedParser::new(|input, location, state: State| {
    match token("(").parse(input, location, state.clone()) {
      ParseResult::ParseOk { .. } => {
        let result = right(
          token("("),
        left(
          goto_label(),
          token(")"),
        ).update_state(move |label, state|
          if !state.symbol_table.contains_key::<str>(&label) {
            let new_symbol_table = state.symbol_table.update(label.clone(), state.instruction_index);
            State {
              symbol_table: new_symbol_table,
              ..state
            }
          } else {
            state
          }
        )
      ).ignore().parse(input, location, state);
      result
      },
      ParseResult::ParseError { .. } =>
        either(line_comment("//"), newline_char()).parse(input, location, state),
    }.map(|_| Instruction::Ignored)
  })
}

fn maybe_indented<'a, A: 'a>(parser: BoxedParser<'a, A, State>) -> BoxedParser<'a, A, State>
{
  BoxedParser::new(move |input, location, state: State|
    match zero_or_more(space_char()).parse(input, location, state.clone()) {
      ParseResult::ParseOk{ input: next_input, location: next_location, state: next_state, .. } =>
        parser.parse(next_input, next_location, next_state),
      ParseResult::ParseError { .. } =>
        parser.parse(input, location, state),
  })
}