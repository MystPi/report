import birdie
import gleam/string
import gleeunit
import report

pub fn main() {
  gleeunit.main()
}

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

const style = False

const lorem = "Lorem ipsum odor amet, consectetuer adipiscing elit. Rhoncus placerat sem vitae hendrerit euismod netus dapibus orci. Commodo donec non felis amet natoque accumsan?"

pub fn error_test() {
  report.error(
    file:,
    source:,
    message: "Invalid syntax",
    from: #(1, 1),
    to: #(1, 6),
    label: "I don't know what this means",
  )
  |> report.to_string(style:)
  |> birdie.snap("a minimal error report")
}

pub fn warning_test() {
  report.warning(
    file:,
    source:,
    message: "Invalid syntax",
    from: #(1, 1),
    to: #(1, 6),
    label: "I don't know what this means",
  )
  |> report.to_string(style:)
  |> birdie.snap("a minimal warning report")
}

pub fn info_test() {
  report.info(
    file:,
    source:,
    message: "Invalid syntax",
    from: #(1, 1),
    to: #(1, 6),
    label: "I don't know what this means",
  )
  |> report.to_string(style:)
  |> birdie.snap("a minimal info report")
}

pub fn location_test() {
  report.error(
    file:,
    source:,
    message: "Type mismatch",
    from: #(13, 12),
    to: #(13, 14),
    label: "this is the wrong type",
  )
  |> report.to_string(style:)
  |> birdie.snap("positions are pointed to")
}

pub fn lines_test() {
  report.error(
    file:,
    source:,
    message: "something's wrong with the code",
    from: #(11, 12),
    to: #(14, 4),
    label: "I don't like this",
  )
  |> report.to_string(style:)
  |> birdie.snap("positions can span multiple lines")
}

pub fn notes_test() {
  report.error(
    file:,
    source:,
    message: "Type mismatch",
    from: #(13, 12),
    to: #(13, 14),
    label: "this is the wrong type",
  )
  |> report.with_notes([
    report.Text(lorem),
    report.Hint(lorem),
    report.Note(lorem),
  ])
  |> report.to_string(style:)
  |> birdie.snap("reports can have attached notes, hints, and text")
}

pub fn context_test() {
  report.error(
    file:,
    source:,
    message: "Type mismatch",
    from: #(13, 12),
    to: #(13, 14),
    label: "this is the wrong type",
  )
  |> report.with_context(#(11, 3), "in this case expression")
  |> report.to_string(style:)
  |> birdie.snap("reports can have additional context")
}

pub fn context_past_pos_test() {
  report.error(
    file:,
    source:,
    message: "Type mismatch",
    from: #(13, 12),
    to: #(13, 14),
    label: "this is the wrong type",
  )
  |> report.with_context(#(14, 3), "ending here")
  |> report.to_string(style:)
  |> birdie.snap("context cannnot be past the reported position")
}

pub fn single_line_test() {
  report.error(
    file:,
    source:,
    message: "Type mismatch",
    from: #(13, 12),
    to: #(13, 14),
    label: "this is the wrong type",
  )
  |> report.with_context(#(13, 9), "in this case branch")
  |> report.to_string(style:)
  |> birdie.snap("context and positions can refer to the same line")
}

pub fn number_padding_test() {
  report.info(
    file:,
    source: string.repeat("\n", 100),
    message: "look at the number padding (also note that empty lines do not have a pointer)",
    from: #(9, 1),
    to: #(100, 6),
    label: "",
  )
  |> report.to_string(style:)
  |> birdie.snap("line numbers are padded according to their width")
}

pub fn string_width_test() {
  report.info(
    file:,
    source: string.repeat("안녕하세요\n", 3),
    message: "editors may not display characters the same way",
    from: #(1, 5),
    to: #(3, 3),
    label: "",
  )
  |> report.to_string(style:)
  |> birdie.snap("pointer based on string width on non-ending lines")
}
