import birdie
import gleam/string
import gleeunit
import report

pub fn main() {
  gleeunit.main()
}

fn snap_styled(report: report.Report, message: String) -> Nil {
  report
  |> report.to_string(style: True)
  |> birdie.snap(message <> " (styled)")
}

fn snap_unstyled(report: report.Report, message: String) -> Nil {
  report
  |> report.to_string(style: False)
  |> birdie.snap(message <> " (unstyled)")
}

// CONSTANTS -------------------------------------------------------------------

const file = "report_test.gleam"

const source = "imprt gleam

type Foo {
  Bar
  Baz
}

pub fn main() {
  let foo = Baz

  case foo {
    Bar -> 3.14
    Baz -> 50
  }
}
"

const lorem = "Lorem ipsum odor amet, consectetuer adipiscing elit. Rhoncus placerat sem vitae hendrerit euismod netus dapibus orci. Commodo donec non felis amet natoque accumsan?"

const readme_test_message = "README example"

// TESTS -----------------------------------------------------------------------

fn readme_report() -> report.Report {
  let file = "FizzBuzz.fun"
  let source =
    "fizz₂ : Nat -> String
fizz₂ num =
    case (mod num 5) (mod num 3) of
        0 0 => \"FizzBuzz\"
        0 _ => \"Fizz\"
        _ 0 => \"Buzz\"
        _ _ => num"

  let message = "`case` clauses have incompatible types"
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

  let info = [
    report.Rows(
      rows: [#("expected type", "String"), #("found type", "Nat")],
      divider: " = ",
    ),
    report.Text(
      "All `case` clauses must evaluate to the same type, but the indicated branch does not have the same type as the others.",
    ),
  ]

  report.error(file:, source:, message:, labels:, info:)
}

pub fn readme_styled_test() {
  readme_report() |> snap_styled(readme_test_message)
}

pub fn readme_unstyled_test() {
  readme_report() |> snap_unstyled(readme_test_message)
}

const labels_test_message = "a simple report with no labels"

fn no_labels_report() -> report.Report {
  let message = "A very informative message for you to read"
  let labels = []

  report.info(file:, source:, message:, labels:, info: [])
}

pub fn no_labels_styled_test() {
  no_labels_report() |> snap_styled(labels_test_message)
}

pub fn no_labels_unstyled_test() {
  no_labels_report() |> snap_unstyled(labels_test_message)
}

const error_test_message = "a minimal error report"

fn error_report() -> report.Report {
  let message = "Invalid syntax"
  let labels = [
    report.primary_label(
      message: "I don't know what this means",
      from: #(1, 1),
      to: #(1, 6),
    ),
  ]

  report.error(file:, source:, message:, labels:, info: [])
}

pub fn error_styled_test() {
  error_report() |> snap_styled(error_test_message)
}

pub fn error_unstyled_test() {
  error_report() |> snap_unstyled(error_test_message)
}

const warning_test_message = "a minimal warning report"

fn warning_report() -> report.Report {
  let message = "Invalid syntax"
  let labels = [
    report.primary_label(
      message: "I don't know what this means",
      from: #(1, 1),
      to: #(1, 6),
    ),
  ]

  report.warning(file:, source:, message:, labels:, info: [])
}

pub fn warning_styled_test() {
  warning_report() |> snap_styled(warning_test_message)
}

pub fn warning_unstyled_test() {
  warning_report() |> snap_unstyled(warning_test_message)
}

const info_test_message = "a minimal info report"

fn info_report() -> report.Report {
  let message = "Invalid syntax"
  let labels = [
    report.primary_label(
      message: "I don't know what this means",
      from: #(1, 1),
      to: #(1, 6),
    ),
  ]

  report.info(file:, source:, message:, labels:, info: [])
}

pub fn info_styled_test() {
  info_report() |> snap_styled(info_test_message)
}

pub fn info_unstyled_test() {
  info_report() |> snap_unstyled(info_test_message)
}

const location_test_message = "positions are pointed to"

fn location_report() -> report.Report {
  let message = "Type mismatch"
  let labels = [
    report.primary_label(
      message: "this is the wrong type",
      from: #(13, 12),
      to: #(13, 14),
    ),
  ]

  report.error(file:, source:, message:, labels:, info: [])
}

pub fn location_styled_test() {
  location_report() |> snap_styled(location_test_message)
}

pub fn location_unstyled_test() {
  location_report() |> snap_unstyled(location_test_message)
}

const lines_test_message = "positions can span multiple lines"

fn lines_report() -> report.Report {
  let message = "something's wrong with the code"
  let labels = [
    report.primary_label(message: "I don't like this", from: #(11, 12), to: #(
      14,
      4,
    )),
  ]

  report.error(file:, source:, message:, labels:, info: [])
}

pub fn lines_styled_test() {
  lines_report() |> snap_styled(lines_test_message)
}

pub fn lines_unstyled_test() {
  lines_report() |> snap_unstyled(lines_test_message)
}

const additional_info_test_message = "reports can have attached notes, hints, and text"

fn additional_info_report() -> report.Report {
  let message = "Type mismatch"
  let labels = [
    report.primary_label(
      message: "this is the wrong type",
      from: #(13, 12),
      to: #(13, 14),
    ),
  ]
  let info = [
    report.Text(lorem),
    report.Note("Hint", lorem),
    report.Note("Note", lorem),
  ]

  report.error(file:, source:, message:, labels:, info:)
}

pub fn additional_info_styled_test() {
  additional_info_report()
  |> snap_styled(additional_info_test_message)
}

pub fn additional_info_unstyled_test() {
  additional_info_report()
  |> snap_unstyled(additional_info_test_message)
}

const context_test_message = "reports can have context labels"

fn context_report() -> report.Report {
  let message = "Type mismatch"
  let labels = [
    report.primary_label(
      message: "this is the wrong type",
      from: #(13, 12),
      to: #(13, 14),
    ),
    report.context_label(
      message: "in this case expression",
      from: #(11, 3),
      to: #(11, 7),
    ),
  ]

  report.error(file:, source:, message:, labels:, info: [])
}

pub fn context_styled_test() {
  context_report() |> snap_styled(context_test_message)
}

pub fn context_unstyled_test() {
  context_report() |> snap_unstyled(context_test_message)
}

const multiple_labels_test_message = "labels on the same line are rendered stepwise"

fn multiple_labels_report() -> report.Report {
  let message = "Type mismatch"
  let labels = [
    report.primary_label(
      message: "this is the wrong type",
      from: #(13, 12),
      to: #(13, 14),
    ),
    report.secondary_label(message: "pattern", from: #(13, 5), to: #(13, 8)),
    report.context_label(message: "in this case branch", from: #(13, 9), to: #(
      13,
      11,
    )),
  ]

  report.error(file:, source:, message:, labels:, info: [])
}

pub fn multiple_labels_styled_test() {
  multiple_labels_report() |> snap_styled(multiple_labels_test_message)
}

pub fn multiple_labels_unstyled_test() {
  multiple_labels_report() |> snap_unstyled(multiple_labels_test_message)
}

const overlapping_labels_test_message = "labels can overlap each other"

fn overlapping_labels_report() -> report.Report {
  let source = "[43, 23, 34 43]"
  let message = "Syntax error"
  let labels = [
    report.primary_label(message: "wanted , or ] here", from: #(1, 13), to: #(
      1,
      15,
    )),
    report.secondary_label(message: "after this number", from: #(1, 10), to: #(
      1,
      12,
    )),
    report.context_label(message: "in this list", from: #(1, 1), to: #(1, 16)),
  ]

  report.error(file:, source:, message:, labels:, info: [])
}

pub fn overlapping_labels_styled_test() {
  overlapping_labels_report() |> snap_styled(overlapping_labels_test_message)
}

pub fn overlapping_labels_unstyled_test() {
  overlapping_labels_report() |> snap_unstyled(overlapping_labels_test_message)
}

const overlapping_multi_labels_test_message = "multi-line labels can overlap eachother"

fn overlapping_multi_labels_report() -> report.Report {
  let message = "They overlap!"
  let labels = [
    report.primary_label(
      message: "this type is pointless",
      from: #(3, 1),
      to: #(6, 2),
    ),
    report.secondary_label(
      message: "what are these even for?",
      from: #(4, 3),
      to: #(5, 6),
    ),
    report.context_label(message: "very cool", from: #(5, 3), to: #(13, 8)),
  ]

  report.info(file:, source:, message:, labels:, info: [])
}

pub fn overlapping_multi_labels_styled_test() {
  overlapping_multi_labels_report()
  |> snap_styled(overlapping_multi_labels_test_message)
}

pub fn overlapping_multi_labels_unstyled_test() {
  overlapping_multi_labels_report()
  |> snap_unstyled(overlapping_multi_labels_test_message)
}

const number_padding_test_message = "line numbers are padded according to the largest width"

fn number_padding_report() -> report.Report {
  let source = string.repeat("a line\n", 100)
  let message = "look at the number padding (also note the break)"
  let labels = [
    report.context_label(message: "start", from: #(1, 1), to: #(1, 7)),
    report.context_label(message: "middle", from: #(50, 1), to: #(50, 7)),
    report.context_label(message: "end", from: #(100, 1), to: #(100, 7)),
  ]

  report.info(file:, source:, message:, labels:, info: [])
}

pub fn number_padding_styled_test() {
  number_padding_report() |> snap_styled(number_padding_test_message)
}

pub fn number_padding_unstyled_test() {
  number_padding_report() |> snap_unstyled(number_padding_test_message)
}

const non_overlapping_multi_labels_test_message = "multi-line labels are not nested if they don't overlap"

fn non_overlapping_multi_labels_report() -> report.Report {
  let message = lorem
  let labels = [
    report.primary_label(message: "this is a type", from: #(3, 1), to: #(6, 2)),
    report.secondary_label(message: "this is a function", from: #(8, 1), to: #(
      15,
      2,
    )),
    report.secondary_label(
      message: "this is a case expression",
      from: #(11, 3),
      to: #(14, 4),
    ),
  ]

  report.warning(file:, source:, message:, labels:, info: [])
}

pub fn non_overlapping_multi_labels_styled_test() {
  non_overlapping_multi_labels_report()
  |> snap_styled(non_overlapping_multi_labels_test_message)
}

pub fn non_overlapping_multi_labels_unstyled_test() {
  non_overlapping_multi_labels_report()
  |> snap_unstyled(non_overlapping_multi_labels_test_message)
}

const rows_test_message = "info rows are formatted"

fn rows_report() -> report.Report {
  let message = "What nice rows!"
  let labels = []
  let info = [
    report.Rows(
      rows: [#("foo", "bar"), #("foobar", "baz"), #("foobarbaz", "qux")],
      divider: ": ",
    ),
    report.Rows(
      rows: [
        #("expected type", "SomeType"),
        #("found type", "SomeOtherType"),
        #("reason", lorem),
      ],
      divider: " -> ",
    ),
  ]

  report.info(file:, source:, message:, labels:, info:)
}

pub fn rows_styled_test() {
  rows_report() |> snap_styled(rows_test_message)
}

pub fn rows_unstyled_test() {
  rows_report() |> snap_unstyled(rows_test_message)
}
