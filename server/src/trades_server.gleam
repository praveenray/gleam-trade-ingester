import gleam/dynamic
import pages/query/query
import gleam/erlang.{type Reference}
import pages/home/home
import pages/manual_trade/manual_trade
import gleam/string_tree
import mist
import wisp/wisp_mist
import wisp.{type Request, type Response}
import gleam/http
import gleam/io
import gleam/otp/supervisor as sup
import gleam/otp/actor
import gleam/erlang/process
import trades/writer_actor
import globals
import shared/utils
import pages/load_files/load_files

const app_name = "trades_server"
const static_prefix = "/static"

pub const ets_table_name = "globals"
pub const ets_trades_table = "trades"
pub const ets_trades_file = "trades.ets"

pub fn main() {

  wisp.configure_logger()
  let assert Ok(global_table) = globals.create_ets_table(ets_table_name)
  let assert Ok(trades_table) = globals.ets_table_from_file(ets_trades_file, ets_trades_table)

  let loader_worker = sup.worker(fn(_) {
    writer_actor.start_actor(global_table, trades_table)
  })

  let mist_worker = sup.worker(fn(_) {
    case wisp_mist.handler(fn(r) {
      my_handler(r, global_table, trades_table, ets_trades_file)
    } , "secret-key")
    |> mist.new()
    |> mist.bind("0.0.0.0")
    |> mist.port(8080)
    |> mist.start_http() {
      Ok(s) -> Ok(s)
      Error(_) -> Error(actor.InitCrashed(dynamic.from("Failed")))
    }
  })


  let spec = sup.Spec(
    argument: Nil,
    max_frequency: 100,
    frequency_period: 1,
    init: fn(children) {
      children
      |> sup.add(loader_worker)
      |> sup.add(mist_worker)
    }
  )

  let assert Ok(_) = sup.start_spec(spec)

  process.sleep_forever()
}

fn my_handler(r: Request, global_table: Reference, trades_table: Reference, trades_file: String) -> Response {
  use req <- middleware(r)
  case wisp.path_segments(req) {
    [] -> wisp.redirect("/home")
    ["home"] -> home.render(r)
    [prefix] if prefix == manual_trade.path_prefix ->
      manual_trade.accept(r, global_table) |> save_trades_to_file(trades_table, trades_file)
    [prefix, .._rest] if prefix == load_files.path_prefix ->
      load_files.accept(r, global_table) |> save_trades_to_file(trades_table, trades_file)
    [prefix, .._rest] if prefix == query.path_prefix -> query.accept(r, trades_table)
    _ -> wisp.html_response(string_tree.from_string("This ain't here"), 404)
  }
}

fn save_trades_to_file(response: Response, trades_table: Reference, file_name: String) -> Response {
  let assert Ok(_) = tab2file(trades_table, file_name)
  response
}

fn allow_cors(req: Request, handler: fn(Request) -> Response) -> Response {
  case req.method {
    http.Options -> wisp.response(200)
    _ -> handler(req)
  }
  |> wisp.set_header("Access-Control-Allow-Origin", "*")
  |> wisp.set_header("Access-Control-Allow-Headers", "*")
}

fn middleware(req: Request, handler: fn(Request) -> Response) -> Response {
  use <- wisp.log_request(req)
  use req <- allow_cors(req)
  use <- wisp.rescue_crashes()
  let assert Ok(priv_dir) = wisp.priv_directory(app_name)
  let static_dir = utils.join_paths([priv_dir, "static"])
  use <- wisp.serve_static(req, under: static_prefix,  from: static_dir)
  handler(req)
}

@external(erlang, "globals_erlang", "tab2file")
pub fn tab2file(table_name: Reference, file_name: String) -> Result(String, String)
