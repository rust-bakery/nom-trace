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
//! # Recording multiple traces
//!
//! Used directly, macros will record a trace under the "default" tag. But
//! if you want to record multiple traces at the same time, add a static string
//! as first argument.
//!
//! As an example, in the following code, the root trace will record in "default",
//! while traces inside the `separated_list` will go in the "in list" trace.
//!
//! You can then print it by doing `print_trace!("in list")`.
//!
//! ```rust,ignore
//!   named!(parser<Vec<&[u8]>>,
//!     //wrap a parser with tr!() to add a trace point
//!     tr!(preceded!(
//!       tag!("data: "),
//!       delimited!(
//!         tag!("("),
//!         separated_list!(
//!           tr!("in list", tag!(",")),
//!           tr!("in list", digit)
//!         ),
//!         tag!(")")
//!       )
//!     ))
//!   );
//! ```
//!
//! # nom 5 functions support
//!
//! The [tr] function supports the same combinator design as introduced in nom 5.
//! Unfortunately, it cannot manipulate its arguments directly from inside the
//! code like macros do, so it must receive explicitely the `tag` argument,
//! and a `name` for this trace point (in macros, that name is generated
//! from a `stringify` call of the argument of `tr!`).
//!
//! So using `tr` directly, you would need to to tr("default", "name", parser1)`.
//! It is recommended to make you own trace parser, as follows:
//!
//! ```rust,ignore
//! fn t<I,O,E,F>(name: &'static str, f: F) -> impl Fn(I) -> IResult<I,O,E>
//!   where Input: From<I>,
//!         F: Fn(I) -> IResult<I,O,E>,
//!         I: Clone,
//!         O: Debug,
//!         E: Debug {
//!   tr(name, f)
//! }
//! ```
#[macro_use]
extern crate nom;

use std::fmt::{self,Debug};
use std::collections::HashMap;
use nom::IResult;

pub struct TraceList {
  pub traces: HashMap<&'static str, Trace>,
}

impl TraceList {
  pub fn new() -> Self {
    let mut traces = HashMap::new();
    traces.insert("default", Trace::new());

    TraceList { traces }
  }

  pub fn reset(&mut self, tag: &'static str) {
    let t = self.traces.entry(tag).or_insert(Trace::new());
    t.reset();
  }

  pub fn print(&self, tag: &'static str) {
    self.traces.get(tag).map(|t| t.print());
  }

  pub fn activate(&mut self, tag: &'static str) {
    let t = self.traces.entry(tag).or_insert(Trace::new());
    t.active = true;
  }

  pub fn deactivate(&mut self, tag: &'static str) {
    let t = self.traces.entry(tag).or_insert(Trace::new());
    t.active = false;
  }

  pub fn open<T>(&mut self, tag: &'static str, input: T, location: &'static str)
    where Input: From<T> {
    let t = self.traces.entry(tag).or_insert(Trace::new());
    t.open(input, location);
  }

  pub fn close<I,O:Debug,E:Debug>(&mut self, tag: &'static str, input: I, location: &'static str, result: &nom::IResult<I,O,E>)
    where Input: From<I> {
    let t = self.traces.entry(tag).or_insert(Trace::new());
    t.close(input, location, result);
  }
}

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

  pub fn close<I,O:Debug,E:Debug>(&mut self, input: I, location: &'static str, result: &nom::IResult<I,O,E>)
    where Input: From<I> {
    if self.active {
      self.level -= 1;
      let event_type = match result {
        Ok((_,o)) => TraceEventType::CloseOk(format!("{:?}", o)),
        Err(nom::Err::Error(e)) => TraceEventType::CloseError(format!("{:?}", e)),
        Err(nom::Err::Failure(e)) => TraceEventType::CloseFailure(format!("{:?}", e)),
        Err(nom::Err::Incomplete(i)) => TraceEventType::CloseIncomplete(i.clone()),
      };
      self.events.push(TraceEvent::new(
        self.level,
        input,
        location,
        event_type
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
  pub static NOM_TRACE: ::std::cell::RefCell<TraceList> = ::std::cell::RefCell::new(TraceList::new());
}

/// print the trace events to stdout
#[macro_export]
macro_rules! print_trace (
 () => {
  $crate::NOM_TRACE.with(|trace| {
    trace.borrow().print("default");
  });
 };
 ($tag:expr) => {
  $crate::NOM_TRACE.with(|trace| {
    trace.borrow().print($tag);
  });
 };
);

/// clears the list of events
#[macro_export]
macro_rules! reset_trace (
 () => {
  $crate::NOM_TRACE.with(|trace| {
    trace.borrow_mut().reset("default");
  });
 };
 ($tag:expr) => {
  $crate::NOM_TRACE.with(|trace| {
    trace.borrow_mut().reset($tag);
  });
 };
);

/// activates tracing (it is activated by default)
#[macro_export]
macro_rules! activate_trace (
 () => {
  $crate::NOM_TRACE.with(|trace| {
    trace.borrow_mut().activate("default");
  });
 };
 ($tag:expr) => {
  $crate::NOM_TRACE.with(|trace| {
    trace.borrow_mut().activate($tag);
  });
 };
);

/// deactivates tracing (it is activated by default)
#[macro_export]
macro_rules! deactivate_trace (
 () => {
  $crate::NOM_TRACE.with(|trace| {
    trace.borrow_mut().deactivate("default");
  });
 };
 ($tag:expr) => {
  $crate::NOM_TRACE.with(|trace| {
    trace.borrow_mut().deactivate($tag);
  });
 };
);

/// function tracer
pub fn tr<I,O,E,F>(tag: &'static str, name: &'static str, f: F) -> impl Fn(I) -> IResult<I,O,E>
  where Input: From<I>,
        F: Fn(I) -> IResult<I,O,E>,
        I: Clone,
        O: Debug,
        E: Debug {
  move |i: I| {
    let input1 = i.clone();
    let input2 = i.clone();
    NOM_TRACE.with(|trace| {
      (*trace.borrow_mut()).open(tag, input1, name);
    });

    let res = f(i);

    NOM_TRACE.with(|trace| {
      (*trace.borrow_mut()).close(tag, input2, name, &res);
    });

    res
  }
}

/// wrap a nom parser or combinator with this macro to add a trace point
#[macro_export]
macro_rules! tr (
  ($i:expr, $name:ident, $submac:ident!( $($args:tt)* )) => (
    tr!(__impl $i, "default", stringify!($name), $submac!($($args)*))
  );
  ($i:expr, $name:ident, $f:expr) => (
    tr!(__impl $i, "default", stringify!($name), call!($f))
  );
  ($i:expr, $submac:ident!( $($args:tt)* )) => (
    tr!(__impl $i, "default", stringify!($submac), $submac!($($args)*))
  );
  ($i:expr, $f:expr) => (
    tr!(__impl $i, "default", stringify!($f), call!($f))
  );
  (__impl $i:expr, $name:expr, $submac:ident!( $($args:tt)* )) => (
    tr!(__impl $i, "default", $name, $submac!($($args)*))
  );
  (__impl $i:expr, $submac:ident!( $($args:tt)* )) => (
    tr!(__impl $i, "default", $submac!($($args)*))
  );
  (__impl $i:expr, $f:expr) => (
    tr!(__impl $i, "default", $f)
  );

  ($i:expr, $tag:expr, $name:ident, $submac:ident!( $($args:tt)* )) => (
    tr!(__impl $i, $tag, stringify!($name), $submac!($($args)*))
  );
  ($i:expr, $tag:expr, $name:ident, $f:expr) => (
    tr!(__impl $i, $tag, stringify!($name), call!($f))
  );
  ($i:expr, $tag:expr, $submac:ident!( $($args:tt)* )) => (
    tr!(__impl $i, $tag, stringify!($submac), $submac!($($args)*))
  );
  ($i:expr, $tag:expr, $f:expr) => (
    tr!(__impl $i, $tag, stringify!($f), call!($f))
  );
  (__impl $i:expr, $tag:expr, $name:expr, $submac:ident!( $($args:tt)* )) => (
    {
      let input = $i;
      $crate::NOM_TRACE.with(|trace| {
        (*trace.borrow_mut()).open($tag, input, $name);
      });

      let res = $submac!(input, $($args)*);
      $crate::NOM_TRACE.with(|trace| {
        (*trace.borrow_mut()).close($tag, input, $name, &res);
      });

      res
    }
  );
  (__impl $i:expr, $tag:expr, $submac:ident!( $($args:tt)* )) => (
    {
      let input = $i;
      $crate::NOM_TRACE.with(|trace| {
        (*trace.borrow_mut()).open($tag, input, stringify!($submac));
      });

      let res = $submac!(input, $($args)*);
      $crate::NOM_TRACE.with(|trace| {
        (*trace.borrow_mut()).close($tag, input, $name, &res);
      });

      res
    }
  );
  (__impl $i:expr, $tag:expr, $f:expr) => (
    {
      let input = $i;
      $crate::NOM_TRACE.with(|trace| {
        (*trace.borrow_mut()).open($tag, input, stringify!($f));
      });

      let res = $f(input);
      $crate::NOM_TRACE.with(|trace| {
        (*trace.borrow_mut()).close($tag, input, $name, &res);
      });

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
