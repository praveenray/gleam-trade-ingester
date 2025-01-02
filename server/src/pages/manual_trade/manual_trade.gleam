import gleam/io
import globals
import gleam/erlang/process
import gleam/erlang.{type Reference}
import gleam/bool
import gleam/json
import gleam/string
import wisp.{type Request, type Response}
import gleam/option.{Some, None}
import gleam/http
import lustre/attribute
import lustre/element/html
import layouts/top_across_menu as layout
import pages/render_utils
import shared/data_types as types
import shared
import shared/utils
import trades/data_types as trades_types
import trades/writer_actor_messages as messages

pub const path_prefix = "manual-trade"

pub fn accept(r: Request, global_table: Reference) -> Response {
  case r.method {
    http.Get -> index(r)
    http.Post -> do_trade(r, global_table)
    _ -> wisp.bad_request()
  }
}

fn index(r: Request) -> Response {
  use <- wisp.require_method(r, http.Get)
  let page = layout.layout(
    mjs_path: Some("/static/pages/manual_trade/manual_trade.mjs"),
    init_json: None,
    body: html.div([attribute.id("app")],[])
  )
  render_utils.send_element(page, 200)
}

fn do_trade(r: Request, global_table: Reference) -> Response {
  use incoming_body <- wisp.require_json(r)
  let state_validate = shared.state_from_dynamic_best_case(incoming_body, types.new_raw_trade())
              |> fn(s) {types.RawTrade(..s, error: None)}
              |> shared.validate()
  let state = case state_validate {
    Ok(s) | Error(s) -> s
  }
  io.debug("incoming body received " <> string.inspect(state))
  bool.guard(types.has_error(state), wisp.bad_request(), fn() { add_one_trade(state, global_table) })
  |> wisp.json_body(state |> types.rawtrade_to_json |> json.to_string_tree)
}

fn add_one_trade(raw_trade: types.RawTrade, global_table: Reference) -> wisp.Response {
  case convert_raw_trade(raw_trade), globals.get_trade_loader_subject(global_table) {
    Ok(raw_trade),Some(subject) -> {
      case process.try_call(subject, fn(sender) {
        messages.InsertOneTrade(trade: raw_trade, sender: sender)
      }, 10_000) {
        Ok(_) -> wisp.ok()
        Error(_) -> wisp.internal_server_error()
      }
    }
    _, _ -> wisp.internal_server_error()
  }
}

// must be called only after validation has passed on raw trade
fn convert_raw_trade(raw: types.RawTrade) -> Result(trades_types.Trade, Nil) {
  let ticker = string.trim(raw.ticker)
  case utils.string_to_date(raw.date),
       utils.string_to_positive_float(raw.quantity),
       utils.string_to_positive_float(raw.price),
       trades_types.string_to_action(raw.action),
       string.length(ticker) {
    Some(date), Some(quantity), Some(price), Some(action), ticker_len
      if ticker_len > 0 -> case action {
        trades_types.Buy|trades_types.Sell -> Ok(trades_types.Trade(
          symbol: ticker,
          run_date: date,
          price: price,
          quantity: quantity,
          action: action
        ))
        _ -> Error(Nil)
      }
    _,_,_,_,_ -> Error(Nil)
  }
}
