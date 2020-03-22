use std::fmt::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Located<A> {
  pub value: A,
  pub from: Location,
  pub to: Location,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Location {
  pub row: usize,
  pub col: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseResult<'a, Output, State> {
  ParseOk {
    input: &'a str,
    location: Location,
    output: Output,
    state: State,
  },
  ParseError {
    message: String,
    from: Location,
    to: Location,
    state: State,
  },
}

impl<'a, T, S: Clone> ParseResult<'a, T, S> {
  pub fn map<U, F: FnOnce(T) -> U>(self, func: F) -> ParseResult<'a, U, S> {
    match self {
      ParseResult::ParseOk { input, location, output, state } =>
        ParseResult::ParseOk {
          input,
          location,
          output: func(output),
          state,
        },
      ParseResult::ParseError { message, from, to, state } =>
        ParseResult::ParseError { message, from, to, state },
    }
  }
  pub fn map_with_state<U, F: FnOnce(T, S) -> U>(self, func: F) -> ParseResult<'a, U, S> {
    match self {
      ParseResult::ParseOk { input, location, output, state } =>
        ParseResult::ParseOk {
          input,
          location,
          output: func(output, state.clone()),
          state: state,
        },
      ParseResult::ParseError { message, from, to, state } =>
        ParseResult::ParseError { message, from, to, state },
    }
  }
  pub fn map_err<F: FnOnce(String) -> String>(self, func: F) -> ParseResult<'a, T, S> {
    match self {
      ParseResult::ParseOk { input, location, output, state } =>
        ParseResult::ParseOk {
          input,
          location,
          output,
          state,
        },
      ParseResult::ParseError { message, from, to, state } =>
        ParseResult::ParseError {
          message: func(message),
          from,
          to,
          state,
        }
    }
  }
  pub fn and_then<U, F: FnOnce(&'a str, T, Location, S) -> ParseResult<'a, U, S>>(self, func: F) -> ParseResult<'a, U, S> {
    match self {
      ParseResult::ParseOk { input, output, location, state } =>
        func(input, output, location, state),
      ParseResult::ParseError { message, from, to, state } =>
        ParseResult::ParseError { message, from, to, state },
    }
  }
}

pub trait Parser<'a, Output, State: Clone> {
  fn parse(&self, input: &'a str, location: Location, state: State) -> ParseResult<'a, Output, State>;
  fn map<F, NewOutput>(self, map_fn: F) -> BoxedParser<'a, NewOutput, State>
    where
        Self: Sized + 'a,
        Output: 'a,
        NewOutput: 'a,
        State: 'a,
        F: Fn(Output) -> NewOutput + 'a,
  {
      BoxedParser::new(map(self, map_fn))
  }
  fn map_with_state<F, NewOutput>(self, map_fn: F) -> BoxedParser<'a, NewOutput, State>
  where
      Self: Sized + 'a,
      Output: 'a,
      NewOutput: 'a,
      State: 'a,
      F: Fn(Output, State) -> NewOutput + 'a,
  {
      BoxedParser::new(map_with_state(self, map_fn))
  }
  fn map_err<F>(self, map_fn: F) -> BoxedParser<'a, Output, State>
    where
        Self: Sized + 'a,
        Output: 'a,
        State: 'a,
        F: Fn(String) -> String + 'a,
  {
      BoxedParser::new(map_err(self, map_fn))
  }
  fn and_then<F, NextParser, NewOutput>(self, f: F) -> BoxedParser<'a, NewOutput, State>
  where
    Self: Sized + 'a,
    Output: 'a,
    NewOutput: 'a,
    State: 'a,
    NextParser: Parser<'a, NewOutput, State> + 'a,
    F: Fn(Output) -> NextParser + 'a,
  {
      BoxedParser::new(and_then(self, f))
  }
  fn pred<F>(self, predicate: F, expecting: &'a str) -> BoxedParser<'a, Output, State>
  where
    Self: Sized + 'a,
    Output: std::fmt::Display + 'a,
    State: 'a,
    F: Fn(&Output) -> bool + 'a,
  {
    BoxedParser::new(pred(self, predicate, expecting))
  }
  fn ignore(self) -> BoxedParser<'a, (), State>
    where
      Self: Sized + 'a,
      Output: 'a,
      State: 'a,
  {
      BoxedParser::new(map(self, |_| ()))
  }
  fn update_state<F>(self, f: F) -> BoxedParser<'a, Output, State>
  where
    Self: Sized + 'a,
    State: 'a,
    Output: Clone + 'a,
    F: Fn(Output, State) -> State + 'a,
  {
    BoxedParser::new(update_state(self, f))
  }
}

impl<'a, F, Output, State: 'a> Parser<'a, Output, State> for F
where
  F: Fn(&'a str, Location, State) -> ParseResult<'a, Output,State>,
  State: Clone,
{
  fn parse(&self, input: &'a str, location: Location, state: State) -> ParseResult<'a, Output, State> {
    self(input, location, state)
  }
}

pub struct BoxedParser<'a, Output, State> {
  parser: Box<dyn Parser<'a, Output, State> + 'a>,
}

impl<'a, Output,State> BoxedParser<'a, Output, State> {
  pub fn new<P>(parser: P) -> Self
  where
      P: Parser<'a, Output, State> + 'a,
      State: Clone,
  {
      BoxedParser {
          parser: Box::new(parser),
      }
  }
}

impl<'a, Output, State> Parser<'a, Output, State> for BoxedParser<'a, Output, State>
  where
    State: Clone,
  {
  fn parse(&self, input: &'a str, location: Location, state: State) -> ParseResult<'a, Output, State> {
      self.parser.parse(input, location, state)
  }
}

fn and_then<'a, P, F, A, B, S: Clone + 'a, NextP>(parser: P, f: F) -> impl Parser<'a, B, S>
  where
    P: Parser<'a, A, S>,
    NextP: Parser<'a, B, S>,
    F: Fn(A) -> NextP,
    S: Clone,
{
  move |input, location, state| parser.parse(input, location, state)
    .and_then(| next_input, next_output, next_location, next_state: S |
      f(next_output).parse(next_input, next_location, next_state)
    )
}

pub fn token<'a, S: Clone + 'a>(expected: &'static str) -> BoxedParser<'a, &str, S> {
  BoxedParser::new(
    move |input: &'a str, location: Location, state: S| {
    let found = input.get(0..expected.len());
    match found {
      Some(next) if next == expected => ParseResult::ParseOk {
        input: &input[expected.len()..],
        output: expected,
        location: increment_col(expected.len(), location),
        state,
      },
      _ => ParseResult::ParseError {
        message: format!(
          "I'm expecting a `{}` but found {}.",
          expected,
          display_token(found)
        ),
        from: location,
        to: match found {
          Some(found_str) => Location {
            row: location.row + found_str.len(),
            col: location.col,
          },
          None => location,
        },
        state,
      },
    }
  })
}

pub fn increment_col(additional_col: usize, location: Location) -> Location {
  Location {
    col: location.col + additional_col,
    ..location
  }
}

pub fn increment_row(additional_row: usize, location: Location) -> Location {
  Location {
    row: location.row + additional_row,
    col: 1,
  }
}

pub fn display_token<T: Display>(token: Option<T>) -> String {
  match token {
    Some(token_content) => format!("`{}`", token_content).replace("\n", "\\n"),
    None => "nothing".to_string(),
  }
}

pub fn pair<'a, P1, P2, R1, R2, S: Clone + 'a>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2), S>
where
  P1: Parser<'a, R1, S>,
  P2: Parser<'a, R2, S>,
{
  move |input, location, state|
    parser1.parse(input, location, state)
      .and_then(| next_input, first_output, next_location, next_state: S |
        parser2.parse(next_input, next_location, next_state).map(| second_output |
          (first_output, second_output)
        )
      )
}

pub fn quadruple<'a, P1: 'a, P2: 'a, P3: 'a, P4: 'a, R1: 'a, R2: 'a, R3: 'a, R4: 'a, S: Clone + 'a>
  (parser1: P1, parser2: P2, parser3: P3, parser4: P4)
  -> BoxedParser<'a, (R1, R2, R3, R4), S>
  where
    P1: Parser<'a, R1, S>,
    P2: Parser<'a, R2, S>,
    P3: Parser<'a, R3, S>,
    P4: Parser<'a, R4, S>,
{
  pair(
    pair(parser1, parser2),
    pair(parser3, parser4),
  )
  .map(|((result1, result2), (result3, result4))|
    (result1, result2, result3, result4)
  )
}

pub fn map<'a, P: 'a, F: 'a, A, B, S: Clone + 'a>(parser: P, map_fn: F) -> BoxedParser<'a, B, S>
where
  P: Parser<'a, A, S>,
  F: Fn(A) -> B,
{
  BoxedParser::new(
    move |input, location, state| parser.parse(input, location, state).map(
    |output| map_fn(output)
  ))
}

pub fn map_with_state<'a, P: 'a, F: 'a, A, B, S: Clone + 'a>(parser: P, map_fn: F) -> BoxedParser<'a, B, S>
where
  P: Parser<'a, A, S>,
  F: Fn(A, S) -> B,
{
  BoxedParser::new(
    move |input, location, state: S| match parser.parse(input, location, state.clone()) {
      ParseResult::ParseOk {
        input: next_input,
        output,
        location: next_location,
        state: next_state,
      } => ParseResult::ParseOk {
        input: next_input,
        location: next_location,
        output: map_fn(output, next_state.clone()),
        state: next_state,
      },
      ParseResult::ParseError {
        message,
        from,
        to,
        state: error_state,
      } => ParseResult::ParseError {
        message,
        from,
        to,
        state: error_state,
      }
    }
  )
}

pub fn map_err<'a, P, F, A, S: Clone + 'a>(parser: P, map_fn: F) -> impl Parser<'a, A, S>
where
  P: Parser<'a, A, S>,
  F: Fn(String) -> String,
{
  move |input, location, state| parser.parse(input, location, state).map_err(
    |error_message| map_fn(error_message)
  )
}

fn map2<'a, P1, P2, F, A, B, C, S: Clone + 'a>(parser1: P1, parser2: P2, map_fn: F) -> impl Parser<'a, C, S>
where
  P1: Parser<'a, A, S>,
  P2: Parser<'a, B, S>,
  F: Fn(A, B) -> C,
{
  move |input, location, state| parser1.parse(input, location, state).and_then(
    |input1, output1, location1, state1 | parser2.parse(input1, location1, state1).map(
      |output2| map_fn(output1, output2)
    )
  )
}

pub fn left<'a, P1: 'a, P2: 'a, R1: 'a, R2: 'a, S: Clone + 'a>(parser1: P1, parser2: P2) -> BoxedParser<'a, R1, S>
where
  P1: Parser<'a, R1, S>,
  P2: Parser<'a, R2, S>,
{
  map(pair(parser1, parser2), |(left, _right)| left)
}

pub fn right<'a, P1: 'a, P2: 'a, R1: 'a, R2: 'a, S: Clone + 'a>(parser1: P1, parser2: P2) -> BoxedParser<'a, R2, S>
where
  P1: Parser<'a, R1, S>,
  P2: Parser<'a, R2, S>,
{
  map(pair(parser1, parser2), |(_left, right)| right)
}

pub fn one_or_more<'a, P, A, S: Clone + 'a>(parser: P) -> impl Parser<'a, Vec<A>, S>
where
  P: Parser<'a, A, S>,
{
  one_or_more_with_ending(false, parser)
}

pub fn one_or_more_till_end<'a, P, A, S: Clone + 'a>(parser: P) -> impl Parser<'a, Vec<A>, S>
where
  P: Parser<'a, A, S>,
{
  one_or_more_with_ending(true, parser)
} 

pub fn one_or_more_with_ending<'a, P, A, S: Clone + 'a>(till_end: bool, parser: P) -> impl Parser<'a, Vec<A>, S>
where
  P: Parser<'a, A, S>,
{
  move |mut input, mut location, mut state: S| {
    let mut result = Vec::new();

    match parser.parse(input, location, state.clone()) {
      ParseResult::ParseOk {
        input: next_input,
        output: first_item,
        location: next_location,
        state: next_state,
      } => {
        input = next_input;
        location = next_location;
        state = next_state;
        result.push(first_item);
      }
      ParseResult::ParseError {
        message: error_message,
        from,
        to,
        state
      } => {
        return ParseResult::ParseError {
          message: error_message,
          from,
          to,
          state,
        };
      }
    }

    loop {
      match parser.parse(input, location, state.clone()) {
        ParseResult::ParseOk {
          input: next_input,
          output: next_item,
          location: next_location,
          state: next_state,
        } => {
          input = next_input;
          location = next_location;
          state = next_state;
          result.push(next_item);
        },
        ParseResult::ParseError {
          message: error_message,
          from,
          to,
          state,
        } => if till_end && input != "" {
          return ParseResult::ParseError {
            message: error_message,
            from,
            to,
            state,
          };
        } else {
          break;
        }
      }
    }

    ParseResult::ParseOk {
      input: input,
      output: result,
      location: location,
      state: state,
    }
  }
}

pub fn zero_or_more<'a, P: 'a, A, S: Clone + 'a>(parser: P) -> BoxedParser<'a, Vec<A>, S>
where
  P: Parser<'a, A, S>,
{
  BoxedParser::new(
    move |mut input, mut location, mut state: S| {
    let mut result = Vec::new();

    while let ParseResult::ParseOk {
      input: next_input,
      output: next_item,
      location: next_location,
      state: next_state,
    } = parser.parse(input, location, state.clone())
    {
      input = next_input;
      location = next_location;
      state = next_state;
      result.push(next_item);
    }

    ParseResult::ParseOk {
      input: input,
      output: result,
      location: location,
      state: state,
    }
  })
}

pub fn any_char<'a, S: Clone + 'a>() -> impl Parser<'a, char, S> {
  |input: &'a str, location: Location, state| match input.chars().next() {
    Some(character) => ParseResult::ParseOk {
      input: &input[character.len_utf8()..],
      output: character,
      location: increment_col(character.len_utf8(), location),
      state,
    },
    _ => ParseResult::ParseError {
      message: "I'm expecting any character but reached the end of input.".to_string(),
      from: location,
      to: location,
      state,
    },
  }
}

fn pred<'a, P, F, A: std::fmt::Display, S: Clone + 'a>(parser: P, predicate: F, expecting: &'a str) -> impl Parser<'a, A, S>
where
  P: Parser<'a, A, S>,
  F: Fn(&A) -> bool,
{
  move |input, location, state: S| match parser.parse(input, location, state.clone()) {
    ParseResult::ParseOk {
      input: next_input,
      output: content,
      location: next_location,
      state: next_state,
    } => if predicate(&content) {
      ParseResult::ParseOk {
        input: next_input,
        output: content,
        location: next_location,
        state: next_state,
      }
    } else {
      ParseResult::ParseError {
        message: format!(
          "I'm expecting {} but found {}.",
          expecting,
          display_token(Some(content)),
        )
        .to_string(),
        from: location,
        to: next_location,
        state: next_state,
        }
    },
    _ => ParseResult::ParseError {
      message: format!(
        "I'm expecting {} but found {}.",
        expecting,
        display_token(input.chars().next())
      )
      .to_string(),
      from: location,
      to: location,
      state,
    },
  }
}

pub fn space_char<'a, S: Clone + 'a>() -> BoxedParser<'a, (), S> {
  any_char().pred(
    |character| *character == ' ',
    "a whitespace",
  ).ignore()
}

pub fn newline_char<'a, S: Clone + 'a>() -> BoxedParser<'a, (), S> {
  BoxedParser::new(
    (move |input, location, state: S| {
      let mut next_input: &str = input;
      let mut next_location: Location = location;
      let mut next_state: S = state.clone();
      let result1 = any_char().pred(
        |character| *character == '\r',
        "a carriage return",
      ).parse(input, location, state);
      match result1 {
        ParseResult::ParseOk {
          input,
          location,
          state,
          ..
        } => {
          next_input = input;
          next_location = location;
          next_state = state;
        }
        _ => {}
      }
      let result = any_char().pred(
        |character| *character == '\n',
        "a newline",
      ).parse(next_input, next_location, next_state);
      match result {
        ParseResult::ParseOk {
          input: next_input,
          output,
          location: next_location,
          state: next_state,
        } => ParseResult::ParseOk {
          input: next_input,
          output: output,
          location: increment_row(1, next_location),
          state: next_state,
        },
        ParseResult::ParseError {
          message: error_message,
          from,
          to,
          state,
        } => ParseResult::ParseError {
          message: error_message,
          from,
          to,
          state,
        }
      }
    }).ignore()
  )
}

fn newline0<'a, S: Clone + 'a>(indentations: usize) -> BoxedParser<'a, (), S> {
  zero_or_more(
    ignore_chain(vec![
      indents(indentations),
      newline_char(),
    ])
  ).ignore()
}

pub fn newline1<'a, S: Clone + 'a>(indentations: usize) -> BoxedParser<'a, (), S> {
  ignore_chain(vec![
    newline_char(),
    newline0(indentations),
  ])
}

pub fn space0<'a, S: Clone + 'a>() -> BoxedParser<'a, (), S> {
  zero_or_more(space_char()).ignore()
}

pub fn space1<'a, S: Clone + 'a>() -> BoxedParser<'a, (), S> {
  one_or_more(space_char()).ignore()
}

pub fn indent<'a, S: Clone + 'a>() -> BoxedParser<'a, (), S> {
  ignore_chain(vec![
    space_char(),
    space_char(),
  ]).map_err(|_| "I'm expecting an indentation.\nAll indentations should be two spaces.".to_string())
}

pub fn indents<'a, S: Clone + 'a>(indentations: usize) -> BoxedParser<'a, (), S> {
  repeat(
    indentations,
    indent(),
  ).map_err(|_| "I'm expecting an indentation.\nAll indentations should be two spaces.".to_string())
  .ignore()
}

fn repeat<'a, A, P, S: Clone + 'a>(times: usize, parser: P)
  -> impl Parser<'a, Vec<A>, S>
  where
    P: Parser<'a, A, S>
{
  move |mut input, mut location, mut state: S| {
    let mut result = Vec::new();

    if times == 0 {
      return ParseResult::ParseOk {
        input,
        location,
        output: result,
        state,
      }
    }

    let mut counter = 0;

    while let ParseResult::ParseOk {
      input: next_input,
      output: next_item,
      location: next_location,
      state: next_state,
      ..
    } = parser.parse(input, location, state.clone())
    {
      if counter >= times {
        break;
      }
      input = next_input;
      location = next_location;
      state = next_state;
      result.push(next_item);
      counter = counter + 1;
    }

    ParseResult::ParseOk {
      input: input,
      output: result,
      location: location,
      state: state,
    }
  }
}

pub fn choose3<'a, A: 'a, P: 'a, S: Clone + 'a>(parser1: P, parser2: P, parser3: P)
  -> BoxedParser<'a, A, S>
  where
    P: Parser<'a, A, S>
{
  either(
    BoxedParser::new(parser1),
    either(
      parser2,
      parser3,
    )
  )
}

pub fn either<'a, A, P: 'a, S: Clone + 'a>(parser1: P, parser2: P)
  -> BoxedParser<'a, A, S>
  where
    P: Parser<'a, A, S>
{
  BoxedParser::new(
    move |input, location, state: S| {
      let result = match parser1.parse(input, location, state.clone()) {
        ok @ ParseResult::ParseOk {..} => ok,
        ParseResult::ParseError {..} =>
          parser2.parse(input, location, state)
      };
      // println!("either result state: {:#?}", match result {
      //   ParseResult::ParseOk { state, ..} => state,
      //   ParseResult::ParseError { state, ..} => state,
      // });
      result
    }
  )
}

pub fn optional<'a, A: Clone + 'a, P: 'a, S: Clone + 'a>(default: A, parser: P)
  -> BoxedParser<'a, A, S>
  where
    P: Parser<'a, A, S>
{
  either(
    BoxedParser::new(
      parser
    ),
    BoxedParser::new(
      move |input, location, state|
      ParseResult::ParseOk {
          input,
          location,
          output: default.clone(),
          state,
        }
      )
  )
}

pub fn newline_with_comment<'a, S: Clone + 'a>(comment_symbol: &'static str) -> impl Parser<'a, (), S> {
  either(
    ignore_chain(vec![
      space0(),
      line_comment(comment_symbol),
    ]),
    ignore_chain(vec![
      space0(),
      newline_char()
    ]),
  )
}

pub fn line_comment<'a, S: Clone + 'a>(comment_symbol: &'static str) -> BoxedParser<'a, (), S> {
  ignore_chain(vec![
    token(comment_symbol).ignore(),
    zero_or_more(any_char().pred(
      |character| *character != '\n' && *character != '\r',
      "any character",
    )).ignore(),
    newline_char(),
  ])
}

pub fn line_comments<'a, S: Clone + 'a>(indentations: usize) -> BoxedParser<'a, (), S> {
  either(
    one_or_more(
      ignore_chain(vec![
        newline0(indentations),
        indents(indentations),
        token("--").ignore(),
        zero_or_more(any_char().pred(
          |character| *character != '\n' && *character != '\r',
          "any character",
        )).ignore(),
        newline1(indentations),
      ])
    ).ignore(),
    newline0(indentations),
  )
}

pub fn ignore_chain<'a, S: Clone + 'a>(parsers: Vec<BoxedParser<'a, (), S>>) -> BoxedParser<'a, (), S>
{
  BoxedParser::new(
    move | mut input, mut location, mut state | {
    for parser in &parsers {
      match parser.parse(input, location, state) {
        ParseResult::ParseOk {
          input: next_input,
          location: next_location,
          state: next_state,
          ..
        } => {
          input = next_input;
          location = next_location;
          state = next_state;
        },
        error @ ParseResult::ParseError {..} => {
          return error;
        }
      }
    }
    ParseResult::ParseOk {
      input,
      location,
      output: (),
      state,
    }
  })
}

pub fn whole_decimal<'a, S: Clone + 'a>() -> impl Parser<'a, usize, S> {
  one_or_more(
    any_char().pred(
    | character |
      character.is_digit(10)
    , "a whole decimal number"
    )
  ).map(| digits | digits.iter().collect::<String>().parse().unwrap())
}

pub fn located<'a, P: 'a, A, S: Clone + 'a>(parser: P) -> impl Parser<'a, Located<A>, S>
  where
    P: Parser<'a, A, S>
{
  move |input, location, state|
  match parser.parse(input, location, state) {
    ParseResult::ParseOk {
      input: next_input,
      output,
      location: next_location,
      state: next_state,
    } => ParseResult::ParseOk {
        input: next_input,
        output: Located {
          value: output,
          from: Location {
            row: location.row,
            col: location.col,
          },
          to: Location {
            row: next_location.row,
            col: next_location.col,
          },
        },
        location: next_location,
        state: next_state,
      },
    ParseResult::ParseError {
      message: error_message,
      from,
      to,
      state,
    } =>
      ParseResult::ParseError {
        message: error_message,
        from,
        to,
        state,
      }
  }
}

pub fn display_error(source: &str, error_message: String, from: Location, to: Location) -> String {
  let row = from.row;
  let col = from.col;
  let error_length = if to.col == from.col {
    1
  } else {
    to.col - from.col
  };
  let error_line = row.to_string() + "| " + source.split("\n").collect::<Vec<&str>>()[row - 1];
  let error_pointer = " ".repeat(col - 1 + row.to_string().len() + 2) + &"^".repeat(error_length);
  let error_report =
    error_line + "\n" + &error_pointer + "\n" + "⚠️" + &error_message;
  error_report
}

pub fn update_state<'a, P, A: Clone, S: Clone + 'a, F>(parser: P, f: F) -> impl Parser<'a, A, S>
  where
    P: Parser<'a, A, S>,
    F: Fn(A, S) -> S
{
  move |input, location, state| {
    match parser.parse(input, location, state) {
      ParseResult::ParseOk {
        input: next_input,
        location: next_location,
        state: next_state,
        output,
      } =>
        ParseResult::ParseOk {
          input: next_input,
          output: output.clone(),
          location: next_location,
          state: f(output, next_state),
        },
      ParseResult::ParseError {
        message,
        from,
        to,
        state,
      } =>
        ParseResult::ParseError {
          message,
          from,
          to,
          state,
        }
    }
  }
}