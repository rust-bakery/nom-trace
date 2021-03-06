//#![feature(trace_macros)]
#[macro_use]
extern crate nom;
#[macro_use]
extern crate nom_trace;

use nom::{number::complete::recognize_float, character::complete::space0 as sp};

use std::str;
use std::collections::HashMap;

pub fn is_string_character(c: char) -> bool {
  c != '"' && c != '\\'
}

#[derive(Debug, PartialEq)]
pub enum JsonValue {
  Str(String),
  Boolean(bool),
  Num(f64),
  Array(Vec<JsonValue>),
  Object(HashMap<String, JsonValue>),
}

named!(float<&str,f64>, flat_map!(recognize_float, parse_to!(f64)));

//FIXME: handle the cases like \u1234
named!(
  string<&str,&str>,
  delimited!(
    tr!(char!('\"')),
    escaped!(take_while1!(is_string_character), '\\', one_of!("\"bfnrt\\")),
    char!('\"')
  )
);

named!(
  boolean<&str,bool>,
  alt!(value!(false, tag!("false")) | value!(true, tag!("true")))
);

named!(
  array<&str,Vec<JsonValue>>,
  preceded!(sp, delimited!(
    char!('['),
    separated_list!(char!(','), value),
    preceded!(sp, char!(']'))
  ))
);

named!(
  key_value<&str,(&str, JsonValue)>,
  preceded!(sp, separated_pair!(tr!(string), tr!(char!(':')), tr!(value)))
);

named!(
  hash<&str,HashMap<String, JsonValue>>,
  preceded!(sp, map!(
    delimited!(
      char!('{'),
      separated_list!(char!(','), tr!(key_value)),
      preceded!(sp, char!('}'))
    ),
    |tuple_vec| tuple_vec
      .into_iter()
      .map(|(k, v)| (String::from(k), v))
      .collect()
  ))
);

named!(
  value<&str,JsonValue>,
  preceded!(sp, tr!(alt!(
    tr!(hash)    => { |h| JsonValue::Object(h)            } |
    tr!(array)   => { |v| JsonValue::Array(v)             } |
    tr!(string)  => { |s| JsonValue::Str(String::from(s)) } |
    tr!(float)   => { |f| JsonValue::Num(f)               } |
    tr!(boolean) => { |b| JsonValue::Boolean(b)           }
  )))
);

named!(
  root<&str,JsonValue>,
  tr!(delimited!(
    call!(sp),
    tr!(alt!(
      map!(tr!(hash), JsonValue::Object) |
      map!(tr!(array), JsonValue::Array)
    )),
    not!(complete!(sp))
  ))
);

fn main() {
  let data = "  { \"a\"\t: 42, \"b\": [ \"x\", \"y\", 12 ] ,  \"c\": { \"hello\" : \"world\" }  }  ";

  match root(data) {
    Ok(val) => println!("parsed value: {:#?}", val),
    Err(e) => {
      println!("parse error: {:?}", e);
      print_trace!();
    }
  }
  reset_trace!();
}

