import gleam/bool
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam_community/ansi
import term_size

// TYPES -----------------------------------------------------------------------

/// Create a report with the [`error`](#error), [`warning`](#warning), and
/// [`info`](#info) functions.
pub opaque type Report {
  Report(
    file: String,
    source: String,
    message: String,
    pos: Span,
    level: Level,
    label: String,
    notes: List(Note),
    context: Option(Context),
  )
}

type Level {
  Error
  Warning
  Info
}

/// A note is some additional information attached to the end of a report.
pub type Note {
  /// Results in `Note: <message>`
  Note(message: String)
  /// Results in `Hint: <message>`
  Hint(message: String)
  /// Results in the text passed in without any prefix. This is an improvement
  /// over simply appending text to the generated report since the message will
  /// be wrapped nicely on multiple lines as needed.
  Text(message: String)
}

type Context {
  Context(message: String, pos: Position)
}

type Position {
  Position(line: Int, column: Int)
}

type Span {
  Span(from: Position, to: Position)
}

// CONSTRUCTORS ----------------------------------------------------------------

/// Create an error report.
///
/// Parameters:
/// - `file`: The name or path of the source file.
/// - `source`: The source code of the file.
/// - `message`: A short message describing the error, such as `Type mismatch`.
/// - `from`: The start position of the error in the source code.
/// - `to`: The end position of the error in the source code.
/// - `label`: The label for the pointer pointing to the error position. This
///   could be `""` if you don't want to include a label.
pub fn error(
  file file: String,
  source source: String,
  message message: String,
  from from: #(Int, Int),
  to to: #(Int, Int),
  label label: String,
) {
  Report(
    file:,
    source:,
    message:,
    pos: span(from, to),
    level: Error,
    label:,
    notes: [],
    context: None,
  )
}

/// Create a warning report.
///
/// Parameters:
/// - `file`: The name or path of the source file.
/// - `source`: The source code of the file.
/// - `message`: A short message describing the warning, such as `Unused function`.
/// - `from`: The start position of the warning in the source code.
/// - `to`: The end position of the warning in the source code.
/// - `label`: The label for the pointer pointing to the warning position. This
///    could be `""` if you don't want to include a label.
pub fn warning(
  file file: String,
  source source: String,
  message message: String,
  from from: #(Int, Int),
  to to: #(Int, Int),
  label label: String,
) {
  Report(
    file:,
    source:,
    message:,
    pos: span(from, to),
    level: Warning,
    label:,
    notes: [],
    context: None,
  )
}

/// Create an informational report.
///
/// Parameters:
/// - `file`: The name or path of the source file.
/// - `source`: The source code of the file.
/// - `message`: A short message describing the report
/// - `from`: The start position of the report in the source code.
/// - `to`: The end position of the report in the source code.
/// - `label`: The label for the pointer pointing to the report position. This
///    could be `""` if you don't want to include a label.
pub fn info(
  file file: String,
  source source: String,
  message message: String,
  from from: #(Int, Int),
  to to: #(Int, Int),
  label label: String,
) {
  Report(
    file:,
    source:,
    message:,
    pos: span(from, to),
    level: Info,
    label:,
    notes: [],
    context: None,
  )
}

fn span(from: #(Int, Int), to: #(Int, Int)) -> Span {
  Span(
    from: Position(line: from.0, column: from.1),
    to: Position(line: to.0, column: to.1),
  )
}

/// Add [`Note`](#Note)s to a report.
///
/// ```
/// report.error(...)
/// |> report.with_notes([
///   report.Note("this is a note"),
///   report.Hint("this is a hint"),
/// ])
/// ```
pub fn with_notes(report: Report, notes: List(Note)) -> Report {
  Report(..report, notes:)
}

/// Context enables an additional location to be pointed out in the source with
/// a message. The position can be anywhere in the source *preceeding* (or equal
/// to) the line of the report's main location.
///
/// For example, if a syntax error was found in a list for whatever reason, the
/// outer list could be annotated with this function to point out where the list
/// started.
///
/// ```
/// report.error(...)
/// |> report.with_context(at: #(2, 5), saying: "in this list")
/// ```
///
/// The result would look something like this:
///
/// ```txt
///     ┌─ in this list
///     ┆
/// foo([1, 2 3])
///           ^ I wanted a comma
/// ```
pub fn with_context(
  report: Report,
  at pos: #(Int, Int),
  saying message: String,
) -> Report {
  Report(
    ..report,
    context: Some(Context(message: message, pos: Position(pos.0, pos.1))),
  )
}

// RENDERING -----------------------------------------------------------------

/// This is the function that does all the magic! It takes a `Report` and
/// transforms it into a `String`. Styling, though it looks nice, can be disabled
/// by passing `style: False`.
///
/// ```
/// report.error(...)
/// |> report.to_string(style: True)
/// //-> "Error: ..."
/// ```
pub fn to_string(report: Report, style has_style: Bool) -> String {
  let ctx = init_ctx(report, has_style)

  let header = render_header(report, ctx)
  let location = render_location(report, ctx)
  let lines =
    ctx.relevant_lines
    |> list.flat_map(render_line(_, report, ctx))
    |> string.join("\n")
  let notes =
    report.notes |> list.map(render_note(_, ctx)) |> string.join("\n\n")

  header
  <> "\n\n"
  <> location
  <> "\n"
  <> lines
  <> case report.notes {
    [] -> ""
    _ -> "\n\n" <> notes
  }
}

/// This record holds a set of computed values that are used across the renderer,
/// mainly so we don't have to pass lots of parameters around or compute things
/// multiple times.
type Ctx {
  Ctx(
    // Colors
    color: fn(String) -> String,
    dim: fn(String) -> String,
    bold: fn(String) -> String,
    hint_color: fn(String) -> String,
    note_color: fn(String) -> String,
    context_color: fn(String) -> String,
    // Max width for rendering, used for wrapping text and other things
    max_width: Int,
    // Lines that should be rendered
    relevant_lines: List(#(Int, String)),
    // This number refers to the width (in characters) of the largest line number
    // in the `relevant_lines`
    number_offset: Int,
  )
}

fn init_ctx(report: Report, has_style: Bool) -> Ctx {
  let relevant_lines = get_relevant_lines(report)

  let number_offset =
    list.fold(relevant_lines, 0, fn(acc, line) { int.max(acc, line.0) })
    |> int.to_string
    |> string.length

  let style = fn(style_fn) {
    case has_style {
      True -> style_fn
      False -> fn(x) { x }
    }
  }

  let color =
    case report.level {
      Error -> ansi.red
      Warning -> ansi.yellow
      Info -> ansi.blue
    }
    |> style

  Ctx(
    color:,
    dim: style(ansi.dim),
    bold: style(ansi.bold),
    hint_color: style(ansi.green),
    note_color: style(ansi.blue),
    context_color: style(ansi.cyan),
    max_width: term_size.columns() |> result.unwrap(80) |> int.min(80),
    relevant_lines:,
    number_offset:,
  )
}

fn render_header(report: Report, ctx: Ctx) -> String {
  let level = case report.level {
    Error -> "Error"
    Warning -> "Warning"
    Info -> "Info"
  }

  let prefix = ctx.color(level <> ": ")

  report.message |> wrap_with_prefix(prefix, ctx) |> ctx.bold
}

fn render_location(report: Report, ctx: Ctx) -> String {
  let span =
    pos_to_string(report.pos.from) <> "-" <> pos_to_string(report.pos.to)
  let loc = "[" <> report.file <> "@" <> span <> "]"
  let start = gutter(inner: "", after: "┌─" <> loc, ctx:)
  let end =
    string.repeat("─", ctx.max_width - string.length(start) - 1) <> "╼"

  ctx.dim(start <> end)
}

fn render_line(line: #(Int, String), report: Report, ctx: Ctx) -> List(String) {
  let #(line_number, text) = line

  let source_line =
    gutter(inner: int.to_string(line_number), after: "│ " <> text, ctx:)
    |> ctx.dim

  let pointer = render_pointer(report, line, ctx)

  let context = case report.context {
    Some(context) if line_number == context.pos.line ->
      render_context(context, ctx)
    _ -> []
  }

  list.concat([context, [source_line], pointer])
}

fn render_context(context: Context, ctx: Ctx) -> List(String) {
  let prefix = gutter(inner: "", after: "│", ctx:)
  let padding = string.repeat(" ", context.pos.column)
  let start = prefix <> padding

  [
    start <> ctx.context_color("┌─ " <> context.message),
    start <> ctx.context_color("┆"),
  ]
}

fn render_pointer(
  report: Report,
  line: #(Int, String),
  ctx: Ctx,
) -> List(String) {
  let #(line_number, text) = line
  let pos = report.pos

  // If the line is empty or not pointed to, we don't want to render the pointer
  use <- bool.guard(
    return: [],
    when: string.trim(text) == ""
      || !{ line_number >= pos.from.line && line_number <= pos.to.line },
  )

  let prefix = case line_number == pos.to.line {
    True -> ctx.dim("╾" <> string.repeat("─", ctx.number_offset) <> "┘")
    False -> gutter(inner: "·", after: "│", ctx:) |> ctx.dim
  }

  let padding =
    case line_number == pos.from.line {
      True -> pos.from.column
      False -> 1
    }
    |> string.repeat(" ", _)

  let width = case pos.from.line, pos.to.line {
    // The pointer starts and ends on the same line
    from, to if from == to -> pos.to.column - pos.from.column

    // The pointer starts on this line and ends on another
    from, _ if line_number == from ->
      string.length(text) - string.length(padding) + 1

    // The pointer ends on this line
    _, to if line_number == to -> pos.to.column - 1

    // An intermediate line between the start and end
    _, _ -> string.length(text)
  }

  let pointer =
    string.repeat("^", width)
    <> case line_number == pos.to.line {
      True -> " " <> report.label
      False -> ""
    }

  [prefix <> padding <> ctx.color(pointer)]
}

fn render_note(note: Note, ctx: Ctx) -> String {
  let prefix = case note {
    Note(_) -> ctx.note_color("Note: ")
    Hint(_) -> ctx.hint_color("Hint: ")
    Text(_) -> ""
  }

  note.message |> wrap_with_prefix(prefix, ctx)
}

/// Given a report, find its 'relevant lines' (i.e. the lines that will be
/// displayed).
fn get_relevant_lines(report: Report) -> List(#(Int, String)) {
  let is_relevant = fn(pair) {
    let #(line, _) = pair
    let is_pointed = line >= report.pos.from.line && line <= report.pos.to.line

    let in_context =
      report.context
      |> option.map(fn(context) {
        line >= context.pos.line && line <= report.pos.to.line
      })
      |> option.unwrap(False)

    is_pointed || in_context
  }

  report.source
  |> string.split("\n")
  |> list.index_map(fn(line, i) { #(i + 1, line) })
  |> list.filter(is_relevant)
}

// HELPERS ---------------------------------------------------------------------

/// Given an inner and after string, return a formatted 'gutter' based on the
/// ctx's `number_offset`.
///
/// ```
/// gutter(inner: "1", after: "|", ctx:) // given number_offset = 3
/// //-> "  1 |"
/// ```
fn gutter(inner inner: String, after after: String, ctx ctx: Ctx) -> String {
  string.repeat(" ", ctx.number_offset - string.length(inner))
  <> inner
  <> " "
  <> after
}

fn pos_to_string(pos: Position) -> String {
  let line = pos.line |> int.to_string
  let column = pos.column |> int.to_string
  line <> ":" <> column
}

fn wrap_with_prefix(text: String, prefix: String, ctx: Ctx) -> String {
  let prefix_length = string.length(ansi.strip(prefix))

  case to_lines(text, ctx.max_width - prefix_length) {
    [] -> prefix
    [line, ..lines] -> {
      use acc, line <- list.fold(lines, from: prefix <> line)
      acc <> "\n" <> string.repeat(" ", prefix_length) <> line
    }
  }
}

/// Stolen from:
/// https://github.com/giacomocavalieri/birdie/blob/2dfaad38dd84e14e8b2fe9a9adf07a760e873fa2/src/birdie.gleam#L709
/// Thanks Jak!! 💖
fn to_lines(string: String, max_length: Int) -> List(String) {
  // We still want to keep the original lines, so we work line by line.
  use line <- list.flat_map(string.split(string, on: "\n"))
  let words = string.split(line, on: " ")
  do_to_lines([], "", 0, words, max_length)
}

fn do_to_lines(
  lines: List(String),
  line: String,
  line_length: Int,
  words: List(String),
  max_length: Int,
) -> List(String) {
  case words {
    [] ->
      case line == "" {
        True -> list.reverse(lines)
        False -> list.reverse([line, ..lines])
      }

    [word, ..rest] -> {
      let word_length = string.length(word)
      let new_line_length = word_length + line_length + 1
      // ^ With the +1 we account for the whitespace that separates words!
      case new_line_length > max_length {
        True -> do_to_lines([line, ..lines], "", 0, words, max_length)
        False -> {
          let new_line = case line {
            "" -> word
            _ -> line <> " " <> word
          }
          do_to_lines(lines, new_line, new_line_length, rest, max_length)
        }
      }
    }
  }
}
