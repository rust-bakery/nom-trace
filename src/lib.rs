#[macro_use]
extern crate nom;

use std::fmt::{self,Debug};
use nom::HexDisplay;

pub struct Trace {
  pub events: Vec<TraceEvent>,
  pub level: usize,
}

impl Trace {
  pub fn new() -> Self {
    Trace {
      events: Vec::new(),
      level: 0,
    }
  }

  pub fn print(&self) {
    for (i, event) in self.events.iter().enumerate() {
      event.print();
    }
  }

  pub fn open<T>(&mut self, input: T, location: &'static str)
    where Input: From<T> {

    self.events.push(TraceEvent::new(
      self.level,
      input,
      location,
      TraceEventType::Open,
    ));

    self.level += 1;
  }

  pub fn close_ok<T>(&mut self, input: T, location: &'static str, result: String)
    where Input: From<T> {
    self.level -= 1;
    self.events.push(TraceEvent::new(
      self.level,
      input,
      location,
      TraceEventType::CloseOk(result),
    ));
  }

  pub fn close_error<T>(&mut self, input: T, location: &'static str, result: String)
    where Input: From<T> {
    self.level -= 1;
    self.events.push(TraceEvent::new(
      self.level,
      input,
      location,
      TraceEventType::CloseError(result),
    ));
  }

  pub fn close_failure<T>(&mut self, input: T, location: &'static str, result: String)
    where Input: From<T> {
    self.level -= 1;
    self.events.push(TraceEvent::new(
      self.level,
      input,
      location,
      TraceEventType::CloseFailure(result),
    ));
  }

  pub fn close_incomplete<T>(&mut self, input: T, location: &'static str, needed: nom::Needed)
    where Input: From<T> {
    self.level -= 1;
    self.events.push(TraceEvent::new(
      self.level,
      input,
      location,
      TraceEventType::CloseIncomplete(needed),
    ));
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
        println!("{}{} | {:?}", indent, self.location, self.input);
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
  bytes(*const u8, usize),
  string(*const u8, usize),
}

impl From<&[u8]> for Input {
  fn from(input: &[u8]) -> Self {
    Input::bytes(input.as_ptr(), input.len())
  }
}

impl From<&str> for Input {
  fn from(input: &str) -> Self {
    Input::string(input.as_ptr(), input.len())
  }
}

impl Debug for Input {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Input::string(ptr, len) => {
        let s = unsafe {
          std::str::from_utf8_unchecked(std::slice::from_raw_parts(*ptr, *len))
        };
        write!(f, "\"{}\"", s)
      },
      Input::bytes(ptr, len) => {
        let s: &[u8] = unsafe {
          std::slice::from_raw_parts(*ptr, *len)
        };
        write!(f, "{}", s.to_hex(16))
      }
    }
  }
}

#[macro_export]
macro_rules! declare_trace (
 ($t:expr) => (
    thread_local! {
      //pub static NOM_TRACE: RefCell<Trace<$t>> = RefCell::new(Trace::new());
      pub static NOM_TRACE: RefCell<Trace<&str>> = RefCell::new(Trace::new());
    }
  );
);

#[macro_export]
macro_rules! tr (
  ($i:expr, $submac:ident!( $($args:tt)* )) => (
    {
      use $crate::nom::{Err,ErrorKind};

      let input = $i;
      NOM_TRACE.with(|trace| {
        //(*trace.borrow_mut()).open(input.to_string(), stringify!($submac!($($args)*)));
        (*trace.borrow_mut()).open(input, stringify!($submac));
      });

      let res = $submac!(input, $($args)*);
      match &res {
        Ok((i, o)) => {
          NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_ok(input, stringify!($submac!($($args)*)),
              format!("{:?}", o));
          });
        }
        Err(Err::Error(e)) =>  {
          NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_error(input, stringify!($submac!($($args)*)),
              format!("{:?}", e));
          });
        },
        Err(Err::Failure(e)) =>  {
          NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_failure(input, stringify!($submac!($($args)*)),
              format!("{:?}", e));
          });
        },
        Err(Err::Incomplete(i)) =>  {
          NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_incomplete(input, stringify!($submac!($($args)*)), i.clone());
          });
        },
      };

      res
    }
  );
  ($i:expr, $f:expr) => (
    {
      use $crate::nom::{Err,ErrorKind};

      let input = $i;
      NOM_TRACE.with(|trace| {
        (*trace.borrow_mut()).open(input, stringify!($f));
      });

      let res = $f(input);
      match &res {
        Ok((i, o)) => {
          NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_ok(input, stringify!($f),
              format!("{:?}", o));
          });
        }
        Err(Err::Error(e)) =>  {
          NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_error(input, stringify!($f),
              format!("{:?}", e));
          });
        },
        Err(Err::Failure(e)) =>  {
          NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_failure(input, stringify!($f),
              format!("{:?}", e));
          });
        },
        Err(Err::Incomplete(i)) =>  {
          NOM_TRACE.with(|trace| {
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
  use std::cell::RefCell;
  use nom::digit;

  //declare_trace!(&'static str);
  thread_local! {
    //pub static NOM_TRACE: RefCell<Trace<&'static str>> = RefCell::new(Trace::new());
    pub static NOM_TRACE: RefCell<Trace> = RefCell::new(Trace::new());
  }

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

    NOM_TRACE.with(|trace| {
      trace.borrow().print();
    });
    panic!();
  }

  #[test]
  pub fn trace_str_parser() {
    named!(parser<&str, Vec<&str>>,
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

    println!("parsed: {:?}", parser("data: (1,2,3)"));

    NOM_TRACE.with(|trace| {
      trace.borrow().print();
    });
    panic!();
  }
}
