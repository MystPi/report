import gleam/bool
import gleam/int
import gleam/iterator
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/pair
import gleam/queue.{type Queue}
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
    level: Level,
    info: List(Info),
    labels: List(Label),
  )
}

type Level {
  Err
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
  /// Create a formatted group of rows of text. The divider string  is inserted
  /// between each row *header* and *content*.
  ///
  /// ```
  /// Rows(
  ///   rows: [
  ///     #("expected type", "SomeType"),
  ///     #("found type", "SomeOtherType"),
  ///   ],
  ///   divider: " = ",
  /// )
  /// ```
  ///
  /// Rendered as (also would have additional styling):
  ///
  /// ```text
  /// expected type = SomeType
  ///    found type = SomeOtherType
  /// ```
  Rows(rows: List(#(String, String)), divider: String)
}

type Position {
  Position(line: Int, column: Int)
}

type Span {
  Span(from: Position, to: Position)
}

type LabelStyle {
  Primary
  Secondary
  Context
}

/// Create a label with functions ending in `_label`.
pub opaque type Label {
  Label(style: LabelStyle, message: String, pos: Span)
}

// CONSTRUCTORS ----------------------------------------------------------------

/// Create an error report.
///
/// Parameters:
/// - `file`: The name or path of the source file.
/// - `source`: The source code of the file.
/// - `message`: A short message describing the report, such as `Type mismatch`.
/// - `labels`: A list of labels to annotate the source code. If multiple labels
/// in the list overlap, the first in the list will take precedence.
/// - `info`: A list of [`Info`](#Info) messages to add to the end of the report.
pub fn error(
  file file: String,
  source source: String,
  message message: String,
  labels labels: List(Label),
  info info: List(Info),
) {
  Report(file:, source:, message:, level: Err, info:, labels:)
}

/// Create a warning report.
///
/// Create an error report.
///
/// Parameters:
/// - `file`: The name or path of the source file.
/// - `source`: The source code of the file.
/// - `message`: A short message describing the report, such as `Unused function`.
/// - `labels`: A list of labels to annotate the source code. If multiple labels
/// in the list overlap, the first in the list will take precedence.
/// - `info`: A list of [`Info`](#Info) messages to add to the end of the report.
pub fn warning(
  file file: String,
  source source: String,
  message message: String,
  labels labels: List(Label),
  info info: List(Info),
) {
  Report(file:, source:, message:, level: Warning, info:, labels:)
}

/// Create an informational report.
///
/// Parameters:
/// - `file`: The name or path of the source file.
/// - `source`: The source code of the file.
/// - `message`: A short message describing the report.
/// - `labels`: A list of labels to annotate the source code. If multiple labels
/// in the list overlap, the first in the list will take precedence.
/// - `info`: A list of [`Info`](#Info) messages to add to the end of the report.
pub fn info(
  file file: String,
  source source: String,
  message message: String,
  labels labels: List(Label),
  info info: List(Info),
) {
  Report(file:, source:, message:, level: Info, info:, labels:)
}

fn span(from: #(Int, Int), to: #(Int, Int)) -> Span {
  Span(
    from: Position(line: from.0, column: from.1),
    to: Position(line: to.0, column: to.1),
  )
}

/// Create a *primary* label. Primary labels represent the main reason for the
/// report (though multiple can be included as needed). The first primary label
/// in the list of labels will have its location included in the report's header.
pub fn primary_label(
  message message: String,
  from start: #(Int, Int),
  to end: #(Int, Int),
) {
  Label(style: Primary, message:, pos: span(start, end))
}

/// Create a *secondary* label. Secondary labels support the reason for the
/// report by giving additional information and context.
pub fn secondary_label(
  message message: String,
  from start: #(Int, Int),
  to end: #(Int, Int),
) {
  Label(style: Secondary, message:, pos: span(start, end))
}

/// Create a *context* label. Context labels provide locational context to the
/// report, normally by pointing out landmarks in the source code such as a
/// surrounding list or function. This can be especially useful when reporting
/// parsing errors:
///
/// ```text
/// foo([1, 2 3])
///     ┬     ━ I wanted a comma
///     ╰╴ in this list
/// ```
pub fn context_label(
  message message: String,
  from start: #(Int, Int),
  to end: #(Int, Int),
) {
  Label(style: Context, message:, pos: span(start, end))
}

// RENDERING -------------------------------------------------------------------

/// Transform a [`Report`](#Report) into a string, ready to be presented to the
/// end user. Styling is optional but recommended for most use cases.
///
/// ```
/// report.error(...)
/// |> report.to_string(style: True)
/// //-> "Error: ..."
/// ```
pub fn to_string(report: Report, style has_style: Bool) -> String {
  let ctx = init_ctx(report, has_style)

  let header = render_header(report, ctx)
  let info = report.info |> list.map(render_info(_, ctx)) |> string.join("\n\n")

  // If there are no labels, just return the header and information.
  use <- bool.lazy_guard(when: report.labels == [], return: fn() {
    header
    <> case report.info {
      [] -> ""
      _ -> "\n" <> info
    }
  })

  let lines =
    ctx.relevant_lines
    |> list.flat_map(render_line(_, ctx))
    |> string.join("\n")

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

fn render_header(report: Report, ctx: Ctx) -> String {
  let level = case report.level {
    Err -> "error:"
    Warning -> "warning:"
    Info -> "info:"
  }

  let in = "in:"

  let message =
    report.message
    |> wrap_with_prefix(ctx.color(level), at: ctx.max_width)
    |> ctx.bold

  let padding = string.repeat(" ", string.length(level) - string.length(in))
  let span = case ctx.primary_label {
    None -> ""
    Some(label) ->
      ctx.dim(
        "@"
        <> pos_to_string(label.pos.from)
        <> "-"
        <> pos_to_string(label.pos.to),
      )
  }
  let location =
    { report.file <> span }
    |> wrap_with_prefix(padding <> ctx.color(in), at: ctx.max_width)

  message <> "\n" <> location
}

/// Render a line of source code with labels.
///
/// ```text
/// 2 │ foo(42)
///   │ ┬╌╌ ━━ this is the wrong type
///   │ ╰╴ in this function call
/// ```
fn render_line(line: Line, ctx: Ctx) -> List(String) {
  case line {
    Break(_) -> [gutter(number: None, line: "·", ctx:) |> ctx.dim]
    Line(number:, source:) -> {
      let relevant = find_relevant_labels(number, ctx)

      let inner_gutter = inner_gutter(relevant.multi_labels, on: number, ctx:)

      let source_line = {
        let gutter =
          gutter(
            number: Some(int.to_string(number) |> ctx.dim),
            line: "│",
            ctx:,
          )
        let highlighted = highlight_source(source, number, ctx)
        gutter <> inner_gutter <> highlighted
      }

      let underline =
        render_single_labels(relevant.single_labels, ctx)
        |> list.map(fn(underline) {
          let gutter = gutter(number: None, line: "│", ctx:)
          gutter <> inner_gutter <> underline
        })

      let multi_starts =
        render_multi_label_starts(
          relevant.multi_starts,
          relevant.without_starts,
          ctx:,
        )
        |> list.map(fn(contents) {
          gutter(number: None, line: "│", ctx:) <> contents
        })

      let multi_ends =
        render_multi_label_ends(
          relevant.multi_ends,
          relevant.without_ends,
          ctx:,
        )
        |> list.map(fn(contents) {
          gutter(number: None, line: "│", ctx:) <> contents
        })

      [source_line, ..list.concat([underline, multi_starts, multi_ends])]
    }
  }
}

/// Relevant labels are the labels that are relevant for a certain line, i.e.
/// the label's span contains the line number at some point. Use
/// `find_relevant_labels` to find the relevant labels for a given line.
type RelevantLabels {
  RelevantLabels(
    /// Labels that don't span across multiple lines.
    single_labels: List(Label),
    /// Labels that DO span across multiple lines.
    multi_labels: List(#(Int, Label)),
    /// Multi-line labels that start on the given line.
    multi_starts: List(#(Int, Label)),
    /// Multi-line labels that end on the given line.
    multi_ends: List(#(Int, Label)),
    /// Relevant multi-line labels that don't start on the given line.
    without_starts: List(#(Int, Label)),
    /// Relevant multi-line labels that don't end on the given line.
    without_ends: List(#(Int, Label)),
  )
}

/// Find the relevant labels for a given line number.
fn find_relevant_labels(line: Int, ctx: Ctx) -> RelevantLabels {
  let single_labels =
    list.filter(ctx.single_labels, fn(label) { line == label.pos.from.line })

  let #(multi_labels, multi_starts, multi_ends, without_starts, without_ends) = {
    let add_if = fn(acc, predicate, label) {
      case predicate {
        True -> [label, ..acc]
        False -> acc
      }
    }

    use acc, tagged <- list.fold(ctx.multi_labels, #([], [], [], [], []))
    let #(_, label) = tagged
    let #(labels, starts, ends, without_starts, without_ends) = acc

    let is_relevant = line >= label.pos.from.line && line <= label.pos.to.line

    let is_start = line == label.pos.from.line
    let is_end = line == label.pos.to.line

    #(
      add_if(labels, is_relevant, tagged),
      add_if(starts, is_start, tagged),
      add_if(ends, is_end, tagged),
      add_if(without_starts, is_relevant && !is_start, tagged),
      add_if(without_ends, is_relevant && !is_end, tagged),
    )
  }

  RelevantLabels(
    single_labels:,
    multi_labels:,
    multi_starts:,
    multi_ends:,
    without_starts:,
    without_ends:,
  )
}

/// Highlight a line of source code based on the primary label's span and color.
fn highlight_source(source: String, line: Int, ctx: Ctx) -> String {
  case ctx.primary_label {
    Some(label) if line >= label.pos.from.line && line <= label.pos.to.line -> {
      let length = string.length(source)

      let from = case line == label.pos.from.line {
        True -> label.pos.from.column - 1
        False -> 0
      }

      let to = case line == label.pos.to.line {
        True -> label.pos.to.column - 1
        False -> length
      }

      let left = string.slice(source, at_index: 0, length: from)
      let middle = string.slice(source, at_index: from, length: to - from)
      let right = string.slice(source, at_index: to, length: length - to)

      left <> ctx.color(middle) <> right
    }
    _ -> source
  }
}

/// Given a list of labels, render them into helpful underlines and messages.
///
/// ```text
/// ┬╌╌ ━━ this is the wrong type
/// ╰╴ in this function call
/// ```
fn render_single_labels(labels: List(Label), ctx: Ctx) -> List(String) {
  use <- bool.guard(return: [], when: labels == [])

  // It might seem like there's a lot of list traversing going on here and in
  // related functions (which there is), but in reality the list of labels will
  // almost never be very long, at most 3 or 4 elements. Having lots of labels
  // on a single line is very rare in practice.

  // Sort the labels by their first column. Labels are sorted so that they will
  // be rendered stepwise in the correct order. This is very important!
  let sorted_labels =
    list.sort(labels, fn(a, b) {
      int.compare(a.pos.from.column, b.pos.from.column)
    })

  // Guaranteed to be non-empty (see guard above)
  let assert [first, ..] = sorted_labels
  let from = first.pos.from.column
  let #(_, to, trailing_label, reversed_labels) =
    list.fold(sorted_labels, #(0, 0, False, []), fn(acc, label) {
      let #(from, to, _, reversed) = acc
      #(
        int.max(from, label.pos.from.column),
        int.max(to, label.pos.to.column),
        label.pos.from.column >= from && label.pos.to.column >= to,
        [label, ..reversed],
      )
    })
  let #(trailing, without_trailing) = case trailing_label, reversed_labels {
    True, [trailing, ..rest] -> #(Some(trailing), list.reverse(rest))
    _, _ -> #(None, list.reverse(reversed_labels))
  }
  let underline = build_underline(labels, from, to, trailing, ctx)

  let label_queue = queue.from_list(without_trailing)
  let message_lines = build_message_lines(label_queue, [], ctx)

  [underline, ..message_lines]
}

/// Build an underline based on a list of labels and start/end columns.
///
/// ```text
/// ┬╌╌ ━━ this is the wrong type
/// ```
///
/// 1. Generate a range between the start column and the end column.
/// 2. Iterate through the range and choose an appropriate character and color
///    based on if the current column is part of a label or not.
/// 3. If there is a trailing underline, append its message to the underline.
fn build_underline(
  labels: List(Label),
  from: Int,
  to: Int,
  trailing: Option(Label),
  ctx: Ctx,
) -> String {
  let padding = string.repeat(" ", from - 1)

  let underline = {
    use acc, column <- list.fold(over: list.range(from, to - 1), from: "")

    let found_label =
      list.find(labels, fn(label) {
        column >= label.pos.from.column && column < label.pos.to.column
      })

    case found_label {
      // There is no label/underline for this column
      Error(_) -> acc <> " "

      Ok(label) -> {
        let color = color_of(label, ctx)

        let render_start_char =
          column == label.pos.from.column && trailing != Some(label)

        // The first column in an underline is a '┬' unless the label is trailing.
        let char = case render_start_char, label.style {
          True, Primary -> "┯"
          False, Primary -> "━"
          False, Context -> "╌"
          True, _ -> "┬"
          False, _ -> "─"
        }

        acc <> color(char)
      }
    }
  }

  let message = case trailing {
    Some(label) -> " " <> color_of(label, ctx)(label.message)
    None -> ""
  }

  padding <> underline <> message
}

/// Given a queue of labels, iterate through them and build a list of lines.
///
/// ```text
/// ╰╴ in this function call
/// ```
///
/// 1. If labels queue is empty, return the lines
/// 2. Iterate through the queue by passing it to `build_message_line` then
///    popping the *last* label off the queue, effectively reducing the amount
///    of labels by one each iteration. This is why we use a queue in the first
///    place—we need to remove elements from both the front and back.
fn build_message_lines(
  label_queue: Queue(Label),
  result: List(String),
  ctx: Ctx,
) -> List(String) {
  case queue.is_empty(label_queue) {
    True -> list.reverse(result)
    False -> {
      let line = build_message_line("", label_queue, 0, ctx)
      let assert Ok(#(_, without_last)) = queue.pop_back(label_queue)
      build_message_lines(without_last, [line, ..result], ctx)
    }
  }
}

/// Build a line of message pointers given a queue of available labels. The last
/// label in the queue will have its message written on the far right.
///
/// ```text
/// ╰╴ in this function call
/// ```
///
/// 1. If labels queue is empty, return built string
/// 2. Iterate through the queue by popping the first label off each iteration,
///    accumulating the result.
fn build_message_line(
  result: String,
  labels: Queue(Label),
  offset: Int,
  ctx: Ctx,
) -> String {
  case queue.is_empty(labels) {
    True -> result
    False -> {
      let assert Ok(#(label, without_first)) = queue.pop_front(labels)

      let padding =
        string.repeat(
          " ",
          label.pos.from.column - offset - 1,
          // The `1` accounts for the written character (`│`)
        )

      // We're at the end of the line if there are no more labels in the queue
      let end = case queue.is_empty(without_first) {
        True -> "╰╴ " <> label.message
        False -> "│"
      }

      let result = result <> padding <> color_of(label, ctx)(end)

      build_message_line(result, without_first, label.pos.from.column, ctx)
    }
  }
}

/// Given a number and line string, return a formatted 'gutter' based on the
/// ctx's `number_offset`.
///
/// ```
/// // given number_offset = 3
///
/// gutter(number: Some("1"), line: "|", ctx:)
/// //-> "  1 | "
///
/// gutter(number: None, line: "|", ctx:)
/// //-> "    | "
/// ```
fn gutter(
  number inner: Option(String),
  line separator: String,
  ctx ctx: Ctx,
) -> String {
  let inner = case inner {
    None -> string.repeat(" ", ctx.number_offset)
    Some(inner) ->
      string_width.align(
        inner,
        align: string_width.Right,
        to: ctx.number_offset,
        with: " ",
      )
  }

  inner <> " " <> ctx.dim(separator) <> " "
}

/// The inner gutter continues the vertical lines of multi-line labels. It may
/// not look like much on its own, but it is very important to the overal report.
///
/// ```text
/// │ │   │
/// ```
fn inner_gutter(
  labels: List(#(Int, Label)),
  on line: Int,
  ctx ctx: Ctx,
) -> String {
  use column, filler <- build_columns(ctx.max_overlapping)

  case list.key_find(labels, column) {
    Error(_) -> #(string.repeat(filler, 2), filler)
    Ok(label) -> {
      let color = color_of(label, ctx)

      let chars = case line == label.pos.from.line {
        True -> string.repeat(filler, 2)
        False -> "│" <> filler
      }

      #(color(chars), filler)
    }
  }
}

/// Render the starts of multi-line labels, each given its own line.
fn render_multi_label_starts(
  starts: List(#(Int, Label)),
  others: List(#(Int, Label)),
  ctx ctx: Ctx,
) -> List(String) {
  let starts_outside_in = list.sort(starts, fn(a, b) { int.compare(a.0, b.0) })

  do_render_multi_label_starts(starts_outside_in, [], others, ctx)
}

fn do_render_multi_label_starts(
  starts: List(#(Int, Label)),
  result: List(String),
  others: List(#(Int, Label)),
  ctx: Ctx,
) -> List(String) {
  case starts {
    [] -> list.reverse(result)
    [start, ..rest_starts] -> {
      let rendered = render_multi_label_start(start, others, ctx)

      do_render_multi_label_starts(
        rest_starts,
        [rendered, ..result],
        // Notice how the start is added to the `others` list. This allows the
        // label to be continued vertically while the rest of the starts are
        // rendered line-by-line.
        [start, ..others],
        ctx,
      )
    }
  }
}

fn render_multi_label_start(
  start: #(Int, Label),
  labels: List(#(Int, Label)),
  ctx ctx: Ctx,
) -> String {
  let #(start_col, start_label) = start
  let start_color = color_of(start_label, ctx)

  let result_start = {
    use column, filler <- build_columns(ctx.max_overlapping)

    case column == start_col {
      True -> #(start_color("╭─"), start_color("─"))
      False ->
        case list.key_find(labels, column) {
          Ok(label) -> {
            let color = color_of(label, ctx)
            #(color("│") <> filler, filler)
          }
          Error(_) -> #(string.repeat(filler, 2), filler)
        }
    }
  }

  let pointer = string.repeat("─", start_label.pos.from.column - 1) <> "^"

  result_start <> start_color(pointer)
}

fn render_multi_label_ends(
  ends: List(#(Int, Label)),
  others: List(#(Int, Label)),
  ctx ctx: Ctx,
) -> List(String) {
  let ends_inside_out =
    list.sort(ends, fn(a, b) { order.negate(int.compare(a.0, b.0)) })

  do_render_multi_label_ends(ends_inside_out, [], others, ctx)
}

fn do_render_multi_label_ends(
  ends: List(#(Int, Label)),
  result: List(String),
  others: List(#(Int, Label)),
  ctx: Ctx,
) -> List(String) {
  case ends {
    [] -> list.reverse(result)
    [end, ..rest_ends] -> {
      let rendered =
        render_multi_label_end(end, list.concat([others, rest_ends]), ctx)

      do_render_multi_label_ends(rest_ends, [rendered, ..result], others, ctx)
    }
  }
}

///
///
/// ```text
/// │ ╰─│─────' this is a message
/// ```
fn render_multi_label_end(
  end: #(Int, Label),
  labels: List(#(Int, Label)),
  ctx: Ctx,
) -> String {
  let #(end_col, end_label) = end
  let end_color = color_of(end_label, ctx)

  let start = {
    use column, filler <- build_columns(ctx.max_overlapping)

    case column == end_col {
      True -> #(end_color("╰─"), end_color("─"))
      False ->
        case list.key_find(labels, column) {
          Ok(label) -> {
            let color = color_of(label, ctx)
            #(color("│") <> filler, filler)
          }
          Error(_) -> #(string.repeat(filler, 2), filler)
        }
    }
  }

  let pointer =
    string.repeat("─", end_label.pos.to.column - 2)
    <> "^ "
    <> end_label.message

  start <> end_color(pointer)
}

/// Build up a string based on a certain number of 'columns' (grid columns) with
/// a rendering function. The rendering function takes the current column number
/// and a filler character (more on this below) and returns the rendered string
/// for the column and a new filler character.
///
/// The filler character can be used to fill in the gaps between columns. This
/// begins as the space character (' ') but can change to anything else such as
/// '─', making rendering something like this possible:
///
/// ```text
/// │ ╰─│─
/// ^ ^ ^
/// 1 2 3
/// ```
///
/// Here `^` marks the start of each column. Note that columns start at 1.
fn build_columns(
  columns: Int,
  do: fn(Int, String) -> #(String, String),
) -> String {
  use <- bool.guard(return: "", when: columns < 1)

  list.fold(list.range(1, columns), #("", " "), fn(acc, column) {
    let #(result, filler) = acc
    let #(rendered, filler) = do(column, filler)
    #(result <> rendered, filler)
  })
  |> pair.first
}

fn render_info(info_bit: Info, ctx: Ctx) -> String {
  let at = ctx.max_width - ctx.number_offset - 1
  let message = case info_bit {
    Text(message) -> message |> wrap_with_prefix("", at:)
    Note(prefix:, message:) ->
      message |> wrap_with_prefix(ctx.bold(prefix <> ":"), at:)
    Rows(rows:, divider:) -> render_rows(rows, divider, at, ctx)
  }

  string_width.stack_horizontal(
    [string.repeat(" ", ctx.number_offset), message],
    place: string_width.Top,
    gap: 1,
    with: " ",
  )
}

fn render_rows(
  rows: List(#(String, String)),
  divider: String,
  width: Int,
  ctx: Ctx,
) -> String {
  let left_width =
    list.fold(rows, 0, fn(w, row) { int.max(w, string_width.line(row.0)) })

  list.map(rows, fn(row) {
    let #(left, right) = row

    [
      string_width.align(
        left,
        to: left_width,
        align: string_width.Right,
        with: " ",
      )
        |> ctx.bold,
      ctx.dim(divider),
      string_width.limit(
        right,
        to: string_width.Size(
          rows: max_lines,
          columns: width - left_width - string_width.line(divider),
        ),
        ellipsis: "",
      ),
    ]
    |> string_width.stack_horizontal(place: string_width.Top, gap: 0, with: " ")
  })
  |> string_width.stack_vertical(align: string_width.Left, gap: 0, with: " ")
}

fn full_line(prefix: String, ctx: Ctx) -> String {
  let padding = string.repeat(" ", ctx.number_offset + 1)
  ctx.dim(
    padding
    <> prefix
    <> string.repeat("─", ctx.terminal_width - ctx.number_offset - 2),
  )
}

// CONTEXT ---------------------------------------------------------------------

/// This record holds a set of computed values that are used across the renderer,
/// mainly so we don't have to pass lots of parameters around or compute things
/// multiple times.
type Ctx {
  Ctx(
    // Coloring functions
    color: fn(String) -> String,
    dim: fn(String) -> String,
    bold: fn(String) -> String,
    secondary_color: fn(String) -> String,
    // Terminal width
    terminal_width: Int,
    // Max width for rendering, used for wrapping text and other things
    max_width: Int,
    // Lines that should be rendered
    relevant_lines: List(Line),
    // This number refers to the width (in characters) of the largest line number
    // in the `relevant_lines`
    number_offset: Int,
    // Maximum number of overlapping multiline labels. This number is useful for
    // determining how many spaces to pad things with.
    max_overlapping: Int,
    // The 'primary' label is the first `Primary` label in the report's list of
    // labels, if it exists.
    primary_label: Option(Label),
    /// Single labels are labels that fit on a single line.
    single_labels: List(Label),
    /// Multi labels span multiple lines.
    multi_labels: List(#(Int, Label)),
  )
}

type Line {
  /// Represents a break between two lines that are not consecutive, rendered
  /// as an empty line with a dot.
  Break(number: Int)
  /// A normal line of source code.
  Line(number: Int, source: String)
}

fn init_ctx(report: Report, has_style: Bool) -> Ctx {
  let #(min_line, max_line, single_labels, multi_labels, primary_label) =
    list.fold(report.labels, #(0, 0, [], [], None), fn(acc, label) {
      let #(min, max, single_labels, multi_labels, primary_label) = acc

      let min = case min {
        0 -> label.pos.from.line
        _ -> int.min(label.pos.from.line, min)
      }
      let max = int.max(max, label.pos.to.line)

      let #(single_labels, multi_labels) = case
        label.pos.from.line == label.pos.to.line
      {
        True -> #([label, ..single_labels], multi_labels)
        False -> #(single_labels, [label, ..multi_labels])
      }

      let primary_label = case primary_label, label.style {
        None, Primary -> Some(label)
        _, _ -> primary_label
      }

      #(min, max, single_labels, multi_labels, primary_label)
    })

  let single_labels = list.reverse(single_labels)
  let #(tagged_multi_labels, max_overlapping) = tag_multi_labels(multi_labels)

  let relevant_lines =
    report.source
    |> string.split("\n")
    |> iterator.from_list
    |> iterator.index
    |> iterator.filter(fn(pair) {
      let line = pair.1 + 1
      line >= min_line && line <= max_line
    })
    |> iterator.fold([], fn(lines, pair) {
      let #(source, i) = pair
      let line = i + 1

      let has_label =
        list.any(report.labels, fn(label) {
          line >= label.pos.from.line && line <= label.pos.to.line
        })

      case has_label, lines {
        // A Break is not added if there is already one directly preceeding it
        False, [Break(_), ..] -> lines
        False, _ -> [Break(number: line), ..lines]
        True, _ -> [Line(number: line, source:), ..lines]
      }
    })
    |> list.reverse

  let number_offset =
    max_line
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
      Err -> ansi.red
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
    secondary_color: style(ansi.cyan),
    terminal_width:,
    max_width:,
    relevant_lines:,
    number_offset:,
    max_overlapping:,
    primary_label:,
    single_labels:,
    multi_labels: tagged_multi_labels,
  )
}

/// 'Tag' multiline labels with a number that indicates how nested within other
/// multiline labels they are. Also returns the max amount of overlapping labels
/// at a single time.
fn tag_multi_labels(labels: List(Label)) -> #(List(#(Int, Label)), Int) {
  let #(labels, max_overlapping, _, _, _) =
    list.sort(labels, by: fn(a, b) {
      int.compare(a.pos.from.line, b.pos.from.line)
    })
    |> list.fold(#([], 0, 1, 0, 0), fn(acc, label) {
      let #(
        labels,
        max_overlapping,
        overlapping_labels,
        greatest_start,
        greatest_end,
      ) = acc

      let start = label.pos.from.line
      let end = label.pos.to.line

      let overlaps = start >= greatest_start && start <= greatest_end

      let overlapping_labels = case overlaps {
        True -> overlapping_labels + 1
        False -> 1
      }

      let labels = [#(overlapping_labels, label), ..labels]

      #(
        labels,
        int.max(overlapping_labels, max_overlapping),
        overlapping_labels,
        start,
        int.max(end, greatest_end),
      )
    })

  #(labels, max_overlapping)
}

// HELPERS ---------------------------------------------------------------------

// fn count_start_spaces(text: String) -> Int {
//   do_count_start_spaces(text, 0)
// }

// fn do_count_start_spaces(text: String, count: Int) -> Int {
//   case text {
//     " " <> rest -> do_count_start_spaces(rest, count + 1)
//     "\t" <> rest -> do_count_start_spaces(rest, count + 1)
//     _ -> count
//   }
// }

fn pos_to_string(pos: Position) -> String {
  let line = pos.line |> int.to_string
  let column = pos.column |> int.to_string
  line <> ":" <> column
}

fn color_of(label: Label, ctx: Ctx) -> fn(String) -> String {
  case label.style {
    Primary -> ctx.color
    Secondary -> ctx.secondary_color
    Context -> ctx.dim
  }
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
