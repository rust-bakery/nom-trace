//! # nom-trace
//!
//! This crate provides a way to trace a parser execution,
//! storing positions in the input data, positions in the parser
//! tree and parser results.
//!
//! As an example, if you run the following code:
//!
//! ```rust
//! #[macro_use] extern crate nom;
//! #[macro_use] extern crate nom-trace;
//!
//! pub fn main() {
//!   named!(parser<Vec<&[u8]>>,
//!     //wrap a parser with tr!() to add a trace point
//!     tr!(preceded!(
//!       tr!(tag!("data: ")),
//!       tr!(delimited!(
//!         tag!("("),
//!         separated_list!(
//!           tr!(tag!(",")),
//!           tr!(digit)
//!         ),
//!         tr!(tag!(")"))
//!       ))
//!     ))
//!   );
//!
//!   println!("parsed: {:?}", parser(&b"data: (1,2,3)"[..]));
//!
//!   // prints the last parser trace
//!   print_trace!();
//!
//!   // the list of trace events can be cleared
//!   reset_trace!();
//! }
//! ```
//!
//! You would get the following result
//! ```
//! parsed: Ok(("", ["1", "2", "3"]))
//! preceded        "data: (1,2,3)"
//!
//!         tag     "data: (1,2,3)"
//!
//!         -> Ok("data: ")
//!         delimited       "(1,2,3)"
//!
//!                 digit   "1,2,3)"
//!
//!                 -> Ok("1")
//!                 tag     ",2,3)"
//!
//!                 -> Ok(",")
//!                 digit   "2,3)"
//!
//!                 -> Ok("2")
//!                 tag     ",3)"
//!
//!                 -> Ok(",")
//!                 digit   "3)"
//!
//!                 -> Ok("3")
//!                 tag     ")"
//!
//!                 -> Error(Code(")", Tag))
//!                 tag     ")"
//!
//!                 -> Ok(")")
//!         -> Ok(["1", "2", "3"])
//! -> Ok(["1", "2", "3"])
//! ```
//!
//! Parser level is indicated through indentation. For each trace point, we have:
//!
//! - indent level, then parser or combinator name, then input position
//! - traces for sub parsers
//! - `->` followed by the parser's result
//!
//! You can add intermediate names instead of combinator names for the trace,
//! like this: `tr!(PARENS, delimited!( ... ))`
//! this would replace the name `delimited` in the trace print, with `PARENS`
//!
//! This tracer works with parsers based on `&[u8]` and `&str` input types.
//! For `&[u8]`, input positions will be displayed as a hexdump.
//!
#[macro_use]
extern crate nom;

use std::fmt::{self,Debug};

/// the main structure hoding trace events. It is stored in a thread level
/// storage variable
pub struct Trace {
  pub events: Vec<TraceEvent>,
  pub level: usize,
  pub active: bool,
}

impl Trace {
  pub fn new() -> Self {
    Trace {
      events: Vec::new(),
      level: 0,
      active: true,
    }
  }

  pub fn reset(&mut self) {
    self.events.clear();
    self.level = 0;
  }

  pub fn print(&self) {
    for event in self.events.iter() {
      event.print();
    }
  }

  pub fn open<T>(&mut self, input: T, location: &'static str)
    where Input: From<T> {
    if self.active {
      self.events.push(TraceEvent::new(
        self.level,
        input,
        location,
        TraceEventType::Open,
      ));

      self.level += 1;
    }
  }

  pub fn close_ok<T>(&mut self, input: T, location: &'static str, result: String)
    where Input: From<T> {
    if self.active {
      self.level -= 1;
      self.events.push(TraceEvent::new(
        self.level,
        input,
        location,
        TraceEventType::CloseOk(result),
      ));
    }
  }

  pub fn close_error<T>(&mut self, input: T, location: &'static str, result: String)
    where Input: From<T> {
    if self.active {
      self.level -= 1;
      self.events.push(TraceEvent::new(
        self.level,
        input,
        location,
        TraceEventType::CloseError(result),
      ));
    }
  }

  pub fn close_failure<T>(&mut self, input: T, location: &'static str, result: String)
    where Input: From<T> {
    if self.active {
      self.level -= 1;
      self.events.push(TraceEvent::new(
        self.level,
        input,
        location,
        TraceEventType::CloseFailure(result),
      ));
    }
  }

  pub fn close_incomplete<T>(&mut self, input: T, location: &'static str, needed: nom::Needed)
    where Input: From<T> {
    if self.active {
      self.level -= 1;
      self.events.push(TraceEvent::new(
        self.level,
        input,
        location,
        TraceEventType::CloseIncomplete(needed),
      ));
    }
  }
}

#[derive(Clone,Debug)]
pub struct TraceEvent {
  pub level: usize,
  pub input: Input,
  pub location: &'static str,
  pub event: TraceEventType,
}

#[derive(Clone,Debug)]
pub enum TraceEventType {
  Open,
  CloseOk(String),
  CloseError(String),
  CloseFailure(String),
  CloseIncomplete(nom::Needed),
}

impl TraceEvent {
  pub fn new<T>(level: usize, input: T, location: &'static str, event: TraceEventType) -> Self
    where Input: From<T> {
    TraceEvent {
      level,
      input: Input::from(input),
      location,
      event,
    }
  }

  pub fn print(&self) {
    let indent = std::iter::repeat('\t').take(self.level).collect::<String>();
    match &self.event {
      TraceEventType::Open => {
        println!("{}{}\t{:?}\n", indent, self.location, self.input);
      },
      TraceEventType::CloseOk(result) => {
        println!("{}-> Ok({})", indent, result);
      },
      TraceEventType::CloseError(e) => {
        println!("{}-> Error({})", indent, e);
      },
      TraceEventType::CloseFailure(e) => {
        println!("{}-> Failure({})", indent, e);
      },
      TraceEventType::CloseIncomplete(i) => {
        println!("{}-> Incomplete({:?})", indent, i);
      },
    }
  }
}

#[derive(Clone)]
pub enum Input {
  Bytes(*const u8, usize),
  String(*const u8, usize),
}

impl From<&[u8]> for Input {
  fn from(input: &[u8]) -> Self {
    Input::Bytes(input.as_ptr(), input.len())
  }
}

impl From<&str> for Input {
  fn from(input: &str) -> Self {
    Input::String(input.as_ptr(), input.len())
  }
}

impl Debug for Input {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Input::String(ptr, len) => {
        let s = unsafe {
          std::str::from_utf8_unchecked(std::slice::from_raw_parts(*ptr, *len))
        };
        write!(f, "\"{}\"", s)
      },
      Input::Bytes(ptr, len) => {
        let s: &[u8] = unsafe {
          std::slice::from_raw_parts(*ptr, *len)
        };
        write!(f, "{}", to_hex(s, 16))
      }
    }
  }
}

fn to_hex(input: &[u8], chunk_size: usize) -> String {
  let mut v = Vec::with_capacity(input.len() * 3);
  let mut i = 0;

  if input.len() <= chunk_size {
    to_hex_chunk(input, i, input.len(), &mut v);
  } else {
    for chunk in input.chunks(chunk_size) {
      //to_hex_chunk(&input[i..std::cmp::min(i+chunk_size, input.len())],
      to_hex_chunk(chunk,
        i, chunk_size, &mut v);
      i += chunk_size;
      v.push(b'\n');
    }
  }

  String::from_utf8_lossy(&v[..]).into_owned()
}

static CHARS: &'static [u8] = b"0123456789abcdef";

fn to_hex_chunk(chunk: &[u8], i: usize, chunk_size: usize, v: &mut Vec<u8>) {
  let s = format!("{:08x}", i);
  for &ch in s.as_bytes().iter() {
    v.push(ch);
  }
  v.push(b'\t');

  for &byte in chunk {
    v.push(CHARS[(byte >> 4) as usize]);
    v.push(CHARS[(byte & 0xf) as usize]);
    v.push(b' ');
  }
  if chunk_size > chunk.len() {
    for _ in 0..(chunk_size - chunk.len()) {
      v.push(b' ');
      v.push(b' ');
      v.push(b' ');
    }
  }
  v.push(b'\t');

  for &byte in chunk {
    if (byte >= 32 && byte <= 126) || byte >= 128 {
      v.push(byte);
    } else {
      v.push(b'.');
    }
  }
}

thread_local! {
  pub static NOM_TRACE: ::std::cell::RefCell<Trace> = ::std::cell::RefCell::new(Trace::new());
}

/// print the trace events to stdout
#[macro_export]
macro_rules! print_trace (
 () => {
  $crate::NOM_TRACE.with(|trace| {
    trace.borrow().print();
  });
 };
);

/// clears the list of events
#[macro_export]
macro_rules! reset_trace (
 () => {
  $crate::NOM_TRACE.with(|trace| {
    trace.borrow_mut().reset();
  });
 };
);

/// activates tracing (it is activated by default)
#[macro_export]
macro_rules! activate_trace (
 () => {
  $crate::NOM_TRACE.with(|trace| {
    trace.borrow_mut().active = true;
  });
 };
);

/// deactivates tracing (it is activated by default)
#[macro_export]
macro_rules! deactivate_trace (
 () => {
  $crate::NOM_TRACE.with(|trace| {
    trace.borrow_mut().active = false;
  });
 };
);

/// wrap a nom parser or combinator with this macro to add a trace point
#[macro_export]
macro_rules! tr (
  ($i:expr, $name:ident, $submac:ident!( $($args:tt)* )) => (
    tr!(__impl $i, stringify!($name), $submac!($($args)*))
  );
  ($i:expr, $name:ident, $f:expr) => (
    tr!(__impl $i, stringify!($name), call!($f))
  );
  ($i:expr, $submac:ident!( $($args:tt)* )) => (
    tr!(__impl $i, stringify!($submac), $submac!($($args)*))
  );
  ($i:expr, $f:expr) => (
    tr!(__impl $i, stringify!($f), call!($f))
  );
  (__impl $i:expr, $name:expr, $submac:ident!( $($args:tt)* )) => (
    {
      use ::nom::Err;

      let input = $i;
      $crate::NOM_TRACE.with(|trace| {
        (*trace.borrow_mut()).open(input, $name);
      });

      let res = $submac!(input, $($args)*);
      match &res {
        Ok((_, o)) => {
          $crate::NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_ok(input, $name,
              format!("{:?}", o));
          });
        }
        Err(Err::Error(e)) =>  {
          $crate::NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_error(input, $name,
              format!("{:?}", e));
          });
        },
        Err(Err::Failure(e)) =>  {
          $crate::NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_failure(input, $name,
              format!("{:?}", e));
          });
        },
        Err(Err::Incomplete(i)) =>  {
          $crate::NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_incomplete(input, $name, i.clone());
          });
        },
      };

      res
    }
  );
  (__impl $i:expr, $submac:ident!( $($args:tt)* )) => (
    {
      use ::nom::Err;

      let input = $i;
      $crate::NOM_TRACE.with(|trace| {
        (*trace.borrow_mut()).open(input, stringify!($submac));
      });

      let res = $submac!(input, $($args)*);
      match &res {
        Ok((_, o)) => {
          $crate::NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_ok(input, stringify!($submac!($($args)*)),
              format!("{:?}", o));
          });
        }
        Err(Err::Error(e)) =>  {
          $crate::NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_error(input, stringify!($submac!($($args)*)),
              format!("{:?}", e));
          });
        },
        Err(Err::Failure(e)) =>  {
          $crate::NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_failure(input, stringify!($submac!($($args)*)),
              format!("{:?}", e));
          });
        },
        Err(Err::Incomplete(i)) =>  {
          $crate::NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_incomplete(input, stringify!($submac!($($args)*)), i.clone());
          });
        },
      };

      res
    }
  );
  (__impl $i:expr, $f:expr) => (
    {
      use nom::Err;

      let input = $i;
      $crate::NOM_TRACE.with(|trace| {
        (*trace.borrow_mut()).open(input, stringify!($f));
      });

      let res = $f(input);
      match &res {
        Ok((_, o)) => {
          $crate::NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_ok(input, stringify!($f),
              format!("{:?}", o));
          });
        }
        Err(Err::Error(e)) =>  {
          $crate::NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_error(input, stringify!($f),
              format!("{:?}", e));
          });
        },
        Err(Err::Failure(e)) =>  {
          $crate::NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_failure(input, stringify!($f),
              format!("{:?}", e));
          });
        },
        Err(Err::Incomplete(i)) =>  {
          $crate::NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_incomplete(input, stringify!($f), i.clone());
          });
        },
      };

      res
    }
  );
);


#[cfg(test)]
mod tests {
  use super::*;
  use nom::character::complete::digit1 as digit;

  #[test]
  pub fn trace_bytes_parser() {
    named!(parser<Vec<&[u8]>>,
      tr!(preceded!(
        tr!(tag!("data: ")),
        tr!(delimited!(
          tag!("("),
          separated_list!(
            tr!(tag!(",")),
            tr!(digit)
          ),
          tr!(tag!(")"))
        ))
      ))
    );

    println!("parsed: {:?}", parser(&b"data: (1,2,3)"[..]));

    print_trace!();
    reset_trace!();
    panic!();
  }

  #[test]
  pub fn trace_str_parser() {
    named!(parser<&str, Vec<&str>>,
      tr!(ROOT, preceded!(
        tr!(tag!("data: ")),
        tr!(PARENS, delimited!(
          tag!("("),
          separated_list!(
            tr!(tag!(",")),
            tr!(digit)
          ),
          tr!(tag!(")"))
        ))
      ))
    );

    deactivate_trace!();
    activate_trace!();
    println!("parsed: {:?}", parser("data: (1,2,3)"));

    print_trace!();
    reset_trace!();
    panic!();
  }
}
