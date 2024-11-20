# report

[![Package Version](https://img.shields.io/hexpm/v/report)](https://hex.pm/packages/report)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/report/)

Create informative & beautiful source code reports.

> Note: this package is not ready for production yet.

![example report output](./assets/example_output.png)

## Usage

Install with the `gleam` CLI. This package works on all targets!

```sh
gleam add report
```

The following code will display the demo report found in the image above.

```gleam
import gleam/io
import report

/// The source code to be annotated in the report.
const source = "fizz₂ : Nat -> String
fizz₂ num =
    case (mod num 5) (mod num 3) of
        0 0 => \"FizzBuzz\"
        0 _ => \"Fizz\"
        _ 0 => \"Buzz\"
        _ _ => num"

pub fn main() {
  let file = "FizzBuzz.fun"
  let message = "`case` clauses have incompatible types"

  // Labels add annotations to the source code. There is normally one primary
  // label and any number of secondary and context labels.
  let labels = [
    report.primary_label(
      message: "expected `String`, found `Nat`",
      from: #(7, 16),
      to: #(7, 19),
    ),
    report.context_label(
      message: "`case` clauses have incompatible types",
      from: #(3, 5),
      to: #(7, 19),
    ),
    report.secondary_label(
      message: "this is found to be of type `String`",
      from: #(4, 16),
      to: #(4, 26),
    ),
    report.secondary_label(
      message: "this is found to be of type `String`",
      from: #(5, 16),
      to: #(5, 22),
    ),
    report.secondary_label(
      message: "this is found to be of type `String`",
      from: #(6, 16),
      to: #(6, 22),
    ),
    report.secondary_label(
      message: "expected type `String` found here",
      from: #(1, 16),
      to: #(1, 22),
    ),
  ]

  // Additional information can be added to the end of a report.
  let info = [
    report.Rows(
      rows: [#("expected type", "String"), #("found type", "Nat")],
      divider: " = ",
    ),
    report.Text(
      "All `case` clauses must evaluate to the same type, but the indicated branch does not have the same type as the others.",
    ),
  ]

  // Create an error report. This could also be `report.warning` or `report.info`.
  report.error(file:, source:, message:, labels:, info:)
  // Finally, turn the report into a string with styling enabled then log the
  // result to the console.
  |> report.to_string(style: True)
  |> io.println_error
}
```

Documentation can be found at <https://hexdocs.pm/report>.

## Versioning

This package follows semver, but stylistic changes such as tweaking colors are not covered by a major version. There will be no breaking changes to the actual API without a major bump.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```

## Inspiration

This package is inspired by similar projects such as [`hug`](https://hexdocs.pm/hug/), [`diagnose`](https://github.com/Mesabloo/diagnose), and [`codespan-reporting`](https://github.com/brendanzab/codespan). The example above is based on the example in `codespan-reporting`'s README.
