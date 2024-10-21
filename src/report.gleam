import gleam/bool
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam_community/ansi
import string_width

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
    info: List(Info),
    context: Option(Context),
  )
}

type Level {
  Error
  Warning
  Info
}

/// Some additional information attached to the end of a report.
pub type Info {
  /// Results in the text passed in without any prefix. This is an improvement
  /// over simply appending text to the generated report since the message will
  /// be wrapped nicely on multiple lines as needed.
  Text(message: String)
  /// Results in `<prefix>: <message>`
  Note(prefix: String, message: String)
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
    info: [],
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
    info: [],
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
    info: [],
    context: None,
  )
}

fn span(from: #(Int, Int), to: #(Int, Int)) -> Span {
  Span(
    from: Position(line: from.0, column: from.1),
    to: Position(line: to.0, column: to.1),
  )
}

/// Add [`Info`](#Info) to a report.
///
/// ```
/// report.error(...)
/// |> report.with_info([
///   report.Note("Hint", "this is a hint"),
///   report.Text("lorem..."),
/// ])
/// ```
pub fn with_info(report: Report, info: List(Info)) -> Report {
  Report(..report, info:)
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
  let lines =
    ctx.relevant_lines
    |> list.flat_map(render_line(_, report, ctx))
    |> string.join("\n")
  let info = report.info |> list.map(render_info(_, ctx)) |> string.join("\n\n")

  header
  <> "\n"
  <> full_line("┌", ctx)
  <> "\n"
  <> lines
  <> "\n"
  <> full_line("└", ctx)
  <> case report.info {
    [] -> ""
    _ -> "\n" <> info
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
    context_color: fn(String) -> String,
    terminal_width: Int,
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

  let terminal_width =
    string_width.get_terminal_size()
    |> result.map(fn(size) { size.columns })
    |> result.unwrap(80)
  let max_width = terminal_width |> int.min(80)

  Ctx(
    color:,
    dim: style(ansi.dim),
    bold: style(ansi.bold),
    context_color: style(ansi.cyan),
    terminal_width:,
    max_width:,
    relevant_lines:,
    number_offset:,
  )
}

fn render_header(report: Report, ctx: Ctx) -> String {
  let level = case report.level {
    Error -> "error:"
    Warning -> "warning:"
    Info -> "info:"
  }

  let in = "in:"

  let message =
    report.message
    |> wrap_with_prefix(ctx.color(level), at: ctx.max_width)
    |> ctx.bold

  let padding = string.repeat(" ", string.length(level) - string.length(in))
  let span =
    pos_to_string(report.pos.from) <> "-" <> pos_to_string(report.pos.to)
  let location =
    { report.file <> ctx.dim("@" <> span) }
    |> wrap_with_prefix(padding <> ctx.color(in), at: ctx.max_width)

  message <> "\n" <> location
}

fn render_line(line: #(Int, String), report: Report, ctx: Ctx) -> List(String) {
  let #(line_number, text) = line

  let source_line =
    gutter(
      inner: Some(int.to_string(line_number) |> ctx.dim),
      after: ctx.dim("│ ") <> text,
      ctx:,
    )

  let pointer = render_pointer(report, line, ctx)

  let context = case report.context {
    Some(context) if line_number == context.pos.line ->
      render_context(context, ctx)
    _ -> []
  }

  list.concat([context, [source_line], pointer])
}

fn render_context(context: Context, ctx: Ctx) -> List(String) {
  let prefix = gutter(inner: None, after: "│", ctx:) |> ctx.dim
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

  let prefix = gutter(inner: None, after: "│", ctx:) |> ctx.dim

  let padding =
    case line_number == pos.from.line {
      True -> pos.from.column
      False -> 1
    }
    |> string.repeat(" ", _)

  let width =
    case pos.from.line, pos.to.line {
      // The pointer starts and ends on the same line
      from, to if from == to -> pos.to.column - pos.from.column

      // The pointer starts on this line and ends on another
      from, _ if line_number == from ->
        string_width.line(text) - string.length(padding) + 1

      // The pointer ends on this line
      _, to if line_number == to -> pos.to.column - 1

      // An intermediate line between the start and end
      _, _ -> string_width.line(text)
    }
    |> int.max(1)

  let pointer =
    string.repeat("^", width)
    <> case line_number == pos.to.line {
      True -> " " <> report.label
      False -> ""
    }

  [prefix <> padding <> ctx.color(pointer)]
}

fn render_info(info_bit: Info, ctx: Ctx) -> String {
  let at = ctx.max_width - ctx.number_offset - 1
  let message = case info_bit {
    Text(message) -> message |> wrap_with_prefix("", at:)
    Note(prefix:, message:) ->
      message |> wrap_with_prefix(ctx.bold(prefix <> ":"), at:)
  }

  string_width.stack_horizontal(
    [string.repeat(" ", ctx.number_offset), message],
    place: string_width.Top,
    gap: 1,
    with: " ",
  )
}

fn full_line(prefix: String, ctx: Ctx) -> String {
  let padding = string.repeat(" ", ctx.number_offset + 1)
  ctx.dim(
    padding
    <> prefix
    <> string.repeat("─", ctx.terminal_width - ctx.number_offset - 2),
  )
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
/// // given number_offset = 3
///
/// gutter(inner: Some("1"), after: "| foo", ctx:)
/// //-> "  1 | foo"
///
/// gutter(inner: None, after: "| foo", ctx:)
/// //-> "    | foo"
/// ```
fn gutter(
  inner inner: Option(String),
  after after: String,
  ctx ctx: Ctx,
) -> String {
  case inner {
    None -> string.repeat(" ", ctx.number_offset)
    Some(inner) ->
      string_width.align(
        inner,
        align: string_width.Right,
        to: ctx.number_offset,
        with: " ",
      )
  }
  <> " "
  <> after
}

fn pos_to_string(pos: Position) -> String {
  let line = pos.line |> int.to_string
  let column = pos.column |> int.to_string
  line <> ":" <> column
}

const max_lines = 999_999

fn wrap_with_prefix(text: String, prefix: String, at width: Int) -> String {
  let prefix_length = string_width.line(prefix)
  let gap = case prefix_length {
    0 -> 0
    _ -> 1
  }

  [
    prefix,
    text
      |> string_width.limit(
        string_width.Size(rows: max_lines, columns: width - prefix_length - gap),
        ellipsis: "",
      ),
  ]
  |> string_width.stack_horizontal(place: string_width.Top, gap:, with: " ")
}
