//#![feature(trace_macros)]
#[macro_use]
extern crate nom;
#[macro_use]
extern crate nom_trace;

use nom::{alphanumeric, recognize_float, sp};

use std::str;
use std::collections::HashMap;

pub fn is_string_character(c: u8) -> bool {
  //FIXME: should validate unicode character
  c != b'"' && c != b'\\'
}

#[derive(Debug, PartialEq)]
pub enum JsonValue {
  Str(String),
  Boolean(bool),
  Num(f64),
  Array(Vec<JsonValue>),
  Object(HashMap<String, JsonValue>),
}

named!(float<f64>, flat_map!(recognize_float, parse_to!(f64)));

//FIXME: handle the cases like \u1234
named!(
  string<&str>,
  delimited!(
    char!('\"'),
    map_res!(
      escaped!(take_while1!(is_string_character), '\\', one_of!("\"bfnrt\\")),
      str::from_utf8
    ),
    char!('\"')
  )
);

named!(
  boolean<bool>,
  alt!(value!(false, tag!("false")) | value!(true, tag!("true")))
);

named!(
  array<Vec<JsonValue>>,
  preceded!(sp, delimited!(
    char!('['),
    separated_list!(char!(','), value),
    preceded!(sp, char!(']'))
  ))
);

named!(
  key_value<(&str, JsonValue)>,
  preceded!(sp, separated_pair!(string, char!(':'), value))
);

named!(
  hash<HashMap<String, JsonValue>>,
  preceded!(sp, map!(
    delimited!(
      char!('{'),
      separated_list!(char!(','), key_value),
      preceded!(sp, char!('}'))
    ),
    |tuple_vec| tuple_vec
      .into_iter()
      .map(|(k, v)| (String::from(k), v))
      .collect()
  ))
);

named!(
  value<JsonValue>,
  preceded!(sp, alt!(
    hash    => { |h| JsonValue::Object(h)            } |
    array   => { |v| JsonValue::Array(v)             } |
    string  => { |s| JsonValue::Str(String::from(s)) } |
    float   => { |f| JsonValue::Num(f)               } |
    boolean => { |b| JsonValue::Boolean(b)           }
  ))
);

named!(
  root<JsonValue>,
  delimited!(
    call!(nom::sp),
    alt!(
      map!(hash, JsonValue::Object) |
      map!(array, JsonValue::Array)
    ),
    not!(complete!(nom::sp))
  )
);

fn main() {
  //loop {
    //let data = include_bytes!("../../data.json");
    let data = b"  { \"a\"\t: 42,
    \"b\": [ \"x\", \"y\", 12 ] ,
    \"c\": { \"hello\" : \"world\"
    }
    }  ";

    root(data).unwrap();
  //}
}

