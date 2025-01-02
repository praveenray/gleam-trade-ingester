import gleam/io
import gleam/int
import gleam/erlang/process
import globals
import gleam/erlang.{type Reference}
import youid/uuid
import gleam/http
import wisp.{type Request, type Response}
import layouts/top_across_menu as layout
import pages/render_utils
import lustre/element/html.{div, text}
import lustre/attribute as a
import gleam/option.{type Option, Some, None}
import lustre/element.{type Element}
import gleam/list
import trades/data_types as types
import trades/writer_actor_messages as messages
import shared/utils as utl

pub const path_prefix = "load-trade-files"
const load_summary_prefix = "load-summary"

pub fn accept(r: Request, global_table: Reference) -> Response {
  case r.method {
    http.Get -> case wisp.path_segments(r) {
      [path] if path_prefix == path -> index(None)
      [_path,summary] if summary == load_summary_prefix -> load_summary(r, global_table)
      _ -> wisp.bad_request()
    }
    http.Post -> do_files_load(r, global_table)
    _ -> wisp.bad_request()
  }
}

fn index(error: Option(String)) -> Response {
  let page = layout.layout(
    mjs_path: None,
    init_json: None,
    body: body(error),
  )
  render_utils.send_element(page, 200)
}

fn do_files_load(req: Request, global_table: Reference) -> Response {
  let req = wisp.set_max_files_size(req, 10*1024*1024)
  use form_data <- wisp.require_form(req)

  case form_data.files {
    [] -> failed("Missing Uploaded Files")
    _ -> {
      let files = list.map(form_data.files, fn(f){
        let #(_name, file) = f
        file.path
      })
      case globals.get_trade_loader_subject(global_table) {
        Some(subject) -> {
          case process.try_call(subject, fn(sender) {
            messages.LoadFromFiles(files: files, sender: sender)
          }, 10_000) {
            Ok(messages.LoadedFromFiles(_, results)) -> {
              let key = uuid.v4_string()
              case insert_into_table(global_table, #(key, fill_file_name_from_path(form_data.files, results))) {
                True -> case utl.create_uri(
                    path_prefix,
                    Some(load_summary_prefix),
                    [#("load-id",key)]
                  ) {
                  Ok(redirect_to) -> wisp.redirect(redirect_to)
                  _ -> failed("Loaded but failed to create redirect uri")
                }
                False -> failed("Loaded but failed to store ID")
              }
            }
            _ -> failed("Error Loading Files")
          }
        }
        _ -> failed("Internal Error! (Missing Subject)")
      }
    }
  }
}

fn fill_file_name_from_path(uploaded_files: List(#(String, wisp.UploadedFile)), load_results: List(types.FileLoadResult)) -> List(types.FileLoadResult) {
  list.fold(uploaded_files, [], fn(acc, upload) {
    let #(_, uploaded) = upload
    case list.find(load_results, fn(load_result) {
      load_result.file_path == uploaded.path
    }) {
      Ok(result) -> [types.FileLoadResult(..result, file_name: uploaded.file_name), ..acc]
      _ -> acc
    }
  })
}

fn load_summary(r: Request, global_table: Reference) -> Response {
  let create_html = fn(results: List(types.FileLoadResult)) {
    let rows = list.map(results, fn(result: types.FileLoadResult) {
      html.tr([],[
        html.td([], [text(result.file_name)]),
        html.td([], [text(int.to_string(result.loaded_count))]),
      ])
    })
    let assert Ok(back) = utl.create_uri(path_prefix, None, [])
    let results_table = div([],[
      html.div([a.style([#("color", "green")])],[text("Summary Of Load")]),
      html.table([], [
        html.tr([], [
          html.th([],[text("File Name")]),
          html.th([],[text("Loaded Count")]),
        ]), ..rows]),
      html.a([a.href(back)], [text("Back")])
    ])
    let page = layout.layout(
      mjs_path: None,
      init_json: None,
      body: results_table
    )
    render_utils.send_element(page, 200)
  }

  case list.find(wisp.get_query(r), fn(pair) {
    let #(name, _) = pair
    name == "load-id"
  }) {
    Ok(#(_, load_id)) -> case lookup_load_id(global_table, load_id) {
      [#(_, list)] -> {
        globals.delete_ets_key(global_table, load_id)
        create_html(list)
      }
      _ -> index(Some("No Results Found for Load ID " <> load_id))
    }
    _ -> index(Some("No Load ID Found"))
  }
}

fn failed(error: String) -> Response {
  index(Some(error))
}

fn body(error: Option(String)) -> Element(a) {
  let error_element = case error {
    Some(msg) -> div([
        a.style([#("color", "red")])
      ],[html.text(msg)]
    )
    _ -> html.div([],[])
  }

  div([], [
    error_element,
    html.form([
      a.method("post"),
      a.action("/" <> path_prefix),
      a.enctype("multipart/form-data")
    ], [
      html.input([
        a.type_("file"),
        a.name("myfile"),
        a.attribute("multiple", ""),
        a.attribute("webkitdirectory", ""),
      ]),
      html.button([
        a.class("btn"),
        a.type_("submit")
      ], [
        html.text("Submit")
      ])
    ])
  ])
}
@external(erlang, "writer_erlang", "insert_into_table")
pub fn insert_into_table(table_name: Reference, data: a) -> Bool

@external(erlang, "globals_erlang", "lookup_string_key")
pub fn lookup_load_id(table: Reference, key: String) -> List(#(String, List(types.FileLoadResult)))

@external(erlang, "writer_erlang", "tab2file")
pub fn tab2file(table_name: Reference, file_name: String) -> Result(String, String)
