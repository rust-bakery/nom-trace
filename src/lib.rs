#[macro_use]
extern crate nom;

use std::fmt::Debug;

/*
pub struct Trace<I> {
  pub events: Vec<TraceEvent<I>>,
}

#[derive(Clone,Debug)]
pub struct TraceEvent<I> {
  pub input: I,
  pub event: TraceEventType,
}
*/
pub struct Trace {
  pub events: Vec<TraceEvent>,
  pub level: usize,
}

#[derive(Clone,Debug)]
pub struct TraceEvent {
  pub level: usize,
  pub input: String,
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

//impl<I:Clone+Debug> Trace<I> {
impl Trace {
  pub fn new() -> Self {
    Trace {
      events: Vec::new(),
      level: 0,
    }
  }

  pub fn print(&self) {
    for (i, event) in self.events.iter().enumerate() {
      let indent = std::iter::repeat('\t').take(event.level).collect::<String>();
      match &event.event {
        TraceEventType::Open => {
          println!("{}{} | {:?}", indent, event.location, event.input);
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
      //println!("{}:{}{:?}", i, indent, event);
    }
  }

  //pub fn open(&mut self, input: I, location: &'static str) {
  pub fn open(&mut self, input: String, location: &'static str) {

    self.events.push(TraceEvent {
      level: self.level,
      input,
      location,
      event: TraceEventType::Open,
    });

    self.level += 1;
  }

  pub fn close_ok(&mut self, input: String, location: &'static str, result: String) {
    self.level -= 1;
    self.events.push(TraceEvent {
      level: self.level,
      input,
      location,
      event: TraceEventType::CloseOk(result),
    });
  }

  pub fn close_error(&mut self, input: String, location: &'static str, result: String) {
    self.level -= 1;
    self.events.push(TraceEvent {
      level: self.level,
      input,
      location,
      event: TraceEventType::CloseError(result),
    });
  }

  pub fn close_failure(&mut self, input: String, location: &'static str, result: String) {
    self.level -= 1;
    self.events.push(TraceEvent {
      level: self.level,
      input,
      location,
      event: TraceEventType::CloseFailure(result),
    });
  }

  pub fn close_incomplete(&mut self, input: String, location: &'static str, needed: nom::Needed) {
    self.level -= 1;
    self.events.push(TraceEvent {
      level: self.level,
      input,
      location,
      event: TraceEventType::CloseIncomplete(needed),
    });
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
        (*trace.borrow_mut()).open(input.to_string(), stringify!($submac));
      });

      let res = $submac!(input, $($args)*);
      match &res {
        Ok((i, o)) => {
          NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_ok(input.to_string(), stringify!($submac!($($args)*)),
              format!("{:?}", o));
          });
        }
        Err(Err::Error(e)) =>  {
          NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_error(input.to_string(), stringify!($submac!($($args)*)),
              format!("{:?}", e));
          });
        },
        Err(Err::Failure(e)) =>  {
          NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_failure(input.to_string(), stringify!($submac!($($args)*)),
              format!("{:?}", e));
          });
        },
        Err(Err::Incomplete(i)) =>  {
          NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_incomplete(input.to_string(), stringify!($submac!($($args)*)), i.clone());
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
        //(*trace.borrow_mut()).open(input.to_string(), stringify!($f));
        (*trace.borrow_mut()).open(input.to_string(), stringify!($f));
      });

      let res = $f(input);
      match &res {
        Ok((i, o)) => {
          NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_ok(input.to_string(), stringify!($f),
              format!("{:?}", o));
          });
        }
        Err(Err::Error(e)) =>  {
          NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_error(input.to_string(), stringify!($f),
              format!("{:?}", e));
          });
        },
        Err(Err::Failure(e)) =>  {
          NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_failure(input.to_string(), stringify!($f),
              format!("{:?}", e));
          });
        },
        Err(Err::Incomplete(i)) =>  {
          NOM_TRACE.with(|trace| {
            (*trace.borrow_mut()).close_incomplete(input.to_string(), stringify!($f), i.clone());
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
  pub fn trace_parser() {
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
