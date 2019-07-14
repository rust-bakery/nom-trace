# nom-trace

This crate provides a way to trace a parser execution,
storing positions in the input data, positions in the parser
tree and parser results.

As an example, if you run the following code:

```rust
#[macro_use] extern crate nom;
#[macro_use] extern crate nom_trace;

pub fn main() {
  named!(parser<&str, Vec<&str>>,
    //wrap a parser with tr!() to add a trace point
    tr!(preceded!(
      tr!(tag!("data: ")),
      tr!(delimited!(
        tag!("("),
        separated_list!(
          tr!(tag!(",")),
          tr!(nom::digit)
        ),
        tr!(tag!(")"))
      ))
    ))
  );

  println!("parsed: {:?}", parser("data: (1,2,3)"));

  // prints the last parser trace
  print_trace!();

  // the list of trace events can be cleared
  reset_trace!();
}
```

You would get the following result
```
parsed: Ok(("", ["1", "2", "3"]))
preceded        "data: (1,2,3)"

        tag     "data: (1,2,3)"

        -> Ok("data: ")
        delimited       "(1,2,3)"

                digit   "1,2,3)"

                -> Ok("1")
                tag     ",2,3)"

                -> Ok(",")
                digit   "2,3)"

                -> Ok("2")
                tag     ",3)"

                -> Ok(",")
                digit   "3)"

                -> Ok("3")
                tag     ")"

                -> Error(Code(")", Tag))
                tag     ")"

                -> Ok(")")
        -> Ok(["1", "2", "3"])
-> Ok(["1", "2", "3"])
```

Parser level is indicated through indentation. For each trace point, we have:

- indent level, then parser or combinator name, then input position
- traces for sub parsers
- `->` followed by the parser's result

You can add intermediate names instead of combinator names for the trace,
like this: `tr!(PARENS, delimited!( ... ))`
this would replace the name `delimited` in the trace print, with `PARENS`

This tracer works with parsers based on `&[u8]` and `&str` input types.
For `&[u8]`, input positions will be displayed as a hexdump.


# Recording multiple traces

Used directly, macros will record a trace under the "default" tag. But
if you want to record multiple traces at the same time, add a static string
as first argument.

As an example, in the following code, the root trace will record in "default",
while traces inside the `separated_list` will go in the "in list" trace.

You can then print it by doing `print_trace!("in list")`.

```rust,ignore
  named!(parser<Vec<&[u8]>>,
    //wrap a parser with tr!() to add a trace point
    tr!(preceded!(
      tag!("data: "),
      delimited!(
        tag!("("),
        separated_list!(
          tr!("in list", tag!(",")),
          tr!("in list", digit)
        ),
        tag!(")")
      )
   ))
  );
```

# nom 5 functions support

The `tr` function supports the same combinator design as introduced in nom 5.
Unfortunately, it cannot manipulate its arguments directly from inside the
code like macros do, so it must receive explicitely the `tag` argument,
and a `name` for this trace point (in macros, that name is generated
from a `stringify` call of the argument of `tr!`).

So using `tr` directly, you would need to to tr("default", "name", parser1)`.
It is recommended to make you own trace parser, as follows:

```rust,ignore
fn t<I,O,E,F>(name: &'static str, f: F) -> impl Fn(I) -> IResult<I,O,E>
  where Input: From<I>,
        F: Fn(I) -> IResult<I,O,E>,
        I: Clone,
        O: Debug,
        E: Debug {
  tr(name, f)
}
```
