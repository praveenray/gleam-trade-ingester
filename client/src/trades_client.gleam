import gleam/string
import lustre/event
import lustre/attribute
import gleam/list
import gleam/int
import gleam/dynamic
import gleam/result
import lustre/element/html
import lustre
import gleam/io
import lustre/element.{type Element}
import lustre/effect.{type Effect}

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
  Nil
}

type State {
  State(
    files: List(String),
    count: Int
  )
}

type Msg {
  FileSelectionChanged(files: List(String))
  IncrementedCount(count: Int)
}
fn init(_) -> #(State, Effect(Msg)) {
  #(State(files: [], count: 0),effect.none())
}

fn update(model: State, msg: Msg) -> #(State, Effect(Msg)) {
  let new_state = case msg {
    FileSelectionChanged(files: files) -> {
      io.debug(files)
      State(files: files, count: list.count(files, fn(_){True}))
    }
    IncrementedCount(count) -> {
      State(..model, count: count)
    }
  }
  #(new_state, effect.none())
}

fn on_change(event: dynamic.Dynamic) -> Result(Msg, List(dynamic.DecodeError)) {
  use target <- result.try(dynamic.field("target", dynamic.dynamic)(event))
  use file_list <- result.try(dynamic.field("files", dynamic.dynamic)(target))
  let file_names = retrieve_file_name_from_file_obj(0, file_list, [])
  Ok(FileSelectionChanged(files: file_names))
}

fn retrieve_file_name_from_file_obj(index: Int, file_list: dynamic.Dynamic, accumulator: List(String)) -> List(String) {
  let field = dynamic.field(int.to_string(index), dynamic.dynamic)
  case field(file_list) {
    Ok(file_obj) -> {
      let new_accumulator = [extract_from_file(file_obj), ..accumulator]
      retrieve_file_name_from_file_obj(index + 1, file_list, new_accumulator)
    }
    _ -> accumulator
  }
}

fn extract_from_file(file: dynamic.Dynamic) -> String {
  let field = dynamic.field("name", dynamic.string)
  case field(file) {
    Ok(str) -> str
    _ -> ""
  }
}

fn view(model: State) -> Element(Msg) {
  html.div([], [

    html.form([
      attribute.method("post"),
      attribute.action("/submit"),
      attribute.enctype("multipart/form-data")
    ], [
      html.input([
        attribute.type_("file"),
        attribute.attribute("multiple", ""),
        attribute.attribute("webkitdirectory", ""),
        event.on("change", on_change)
      ]),
      html.button([
        attribute.class("btn"),
        attribute.type_("submit")
      ], [
        html.text("Submit")
      ])
    ]),

    html.input([
      attribute.value(int.to_string(model.count)),
      attribute.on("change", increment_count)
    ]),
    html.input([
        attribute.type_("file"),
        attribute.attribute("multiple", ""),
        attribute.attribute("webkitdirectory", ""),
        //event.on("change", on_change)
    ]),
    file_table(model.files),
  ])
}

fn file_table(files: List(String)) -> Element(Msg) {
  html.table([], list.map(files, fn(file){
    html.tr([], [
      html.td([], [html.text(file)]),
      html.td([], [html.button([], [html.text("D")])]),
    ])
  }))
}

fn increment_count(event: dynamic.Dynamic) -> Result(Msg, List(dynamic.DecodeError)) {
  use value <- result.try(event.value(event))
  case int.parse(value) {
    Ok(existing) -> Ok(IncrementedCount(existing + 1))
    Error(_) -> Error([dynamic.DecodeError(expected: "a number", found: value, path: [])])
  }
}

