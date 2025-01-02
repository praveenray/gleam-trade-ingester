import gleam/dynamic.{type Dynamic}
import gleam/result
import gleam/option.{type Option, Some, None}
import lustre/element/html.{div,input,button,text}
import lustre/effect
import lustre_http as lh
import lustre
import lustre/element.{type Element}
import lustre/attribute as a
import lustre/event
import gleam/io
import shared/data_types as types
import shared

pub type Msg {
  TickerUpdated(value: String)
  QuantityUpdated(value: String)
  PriceUpdated(value: String)
  ActionUpdated(value: String)
  DateUpdated(value: String)
  TradeValuesEntered()
  TradeValuesResponseReceived(response_state: types.RawTrade)
}

pub fn main() {
  io.debug("Manual Trade Starting")
  case lustre.start(lustre.application(init, update, view), "#app", Nil) {
    Ok(_) -> io.debug("Manual Trade Started")
    _ -> io.debug("Failed to Start App")
  }
}

fn init(_) -> #(types.RawTrade, effect.Effect(Msg)) {
  #(types.new_raw_trade(), effect.none())
}

fn update(state: types.RawTrade, msg: Msg) -> #(types.RawTrade, effect.Effect(Msg)) {
  case msg {
    TickerUpdated(value) -> #(types.RawTrade(..state, ticker: value), effect.none())
    PriceUpdated(value) -> #(types.RawTrade(..state, price: value), effect.none())
    QuantityUpdated(value) -> #(types.RawTrade(..state, quantity: value), effect.none())
    DateUpdated(value) -> #(types.RawTrade(..state, date: value), effect.none())
    ActionUpdated(value) -> #(types.RawTrade(..state, action: value), effect.none())
    TradeValuesEntered -> {
      let with_no_errors = types.reset_errors(state)
      #(with_no_errors, post_api(with_no_errors))
    }
    TradeValuesResponseReceived(response_state) -> #(response_state, effect.none())
  }
}

fn post_api(state: types.RawTrade) -> effect.Effect(Msg) {
  lh.post("http://localhost:1235/manual-trade", types.rawtrade_to_json(state),
    lh.expect_json(populate_state_with_response, fn(result) -> Msg {
      populate_state_with_error_response(result, state)
      |> TradeValuesResponseReceived()
    })
  )
}

fn populate_state_with_error_response(result: Result(types.RawTrade, lh.HttpError), client_state: types.RawTrade) -> types.RawTrade {
  case result {
    Ok(_) -> types.RawTrade(..types.new_raw_trade(), success: Some("Trade Saved Successfully"))
    Error(err) -> {
      case err {
        lh.OtherError(_http_code, err_msg) -> shared.state_from_string_best_case(err_msg, client_state)
        lh.JsonError(_e)  -> {
          types.RawTrade(..client_state, error: Some("Error parsing response JSON"))
        }
        lh.Unauthorized  -> types.RawTrade(..client_state, error: Some("Unauthorized Request"))
        lh.NetworkError | lh.BadUrl(_) | lh.NotFound -> types.RawTrade(..client_state, error: Some("Network Error"))
        lh.InternalServerError(_) -> types.RawTrade(..client_state, error: Some("Server Error Saving Trade"))
      }
    }
  }
}

fn populate_state_with_response(target: Dynamic) -> Result(types.RawTrade, List(dynamic.DecodeError)) {
  shared.state_from_dynamic_best_case(target, types.new_raw_trade()) |> Ok()
}

fn view(state: types.RawTrade) -> Element(Msg) {
  let buy = "Buy"
  let sell = "Sell"
  let action_selected = state.action
  io.debug(action_selected)
  div([], [
    show_page_error(state.error),
    show_page_success(state.success),
    input([
      a.placeholder("Ticker"),
      a.name("ticker"),
      a.required(True),
      a.attribute("maxlength", "10"),
      a.attribute("minlength", "1"),
      a.value(state.ticker),
      event.on("change", to_msg(_,TickerUpdated)),
    ]),
    show_error(state.ticker_error),
    
    html.label([], [html.text("Quantity")]),
    input([
      a.placeholder("Quantity"),
      a.name("quantity"),
      a.required(True),
      a.value(state.quantity),
      event.on("change", to_msg(_,QuantityUpdated)),
    ]),
    show_error(state.quantity_error),
    
    html.label([], [html.text("Price")]),
    input([
      a.placeholder("Price"),
      a.name("price"),
      a.required(True),
      a.value(state.price),
      event.on("change", to_msg(_,PriceUpdated)),
    ]),
    show_error(state.price_error),
    html.label([], [html.text("Date: ")]),
    input([
      a.placeholder("Date (YYYY-MM-DD)"),
      a.name("date"),
      a.required(True),
      a.attribute("maxlength", "10"),
      a.attribute("minlength", "10"),
      a.pattern("[1-9]{4}-[0-9]{2}-[0-9]{2}"),
      a.value(state.date),
      event.on("change", to_msg(_, DateUpdated)),
    ]),
    show_error(state.date_error),
    html.select([
      event.on("change", to_msg(_, ActionUpdated))
    ],[
      html.option([a.value("")], ""),
      html.option([a.value(buy), a.selected(buy == action_selected)], buy),
      html.option([a.value(sell), a.selected(sell == action_selected)], sell),
    ]),
    show_error(state.action_error),
    button([event.on_click(TradeValuesEntered)],[html.text("Save")])
  ])
}

fn to_msg(event: Dynamic, msg: fn(String) -> Msg) -> Result(Msg, List(dynamic.DecodeError)) {
  use target <- result.try(dynamic.field("target", dynamic.dynamic)(event))
  use value <- result.try(dynamic.field("value", dynamic.string)(target))
  Ok(msg(value))
}

fn show_page_error(error: Option(String)) -> Element(a) {
  case error {
    Some(msg) -> html.span([
      a.style([
        #("color", "red")
      ])
    ],[text(msg)])
    None -> html.span([],[])
  }
}

fn show_page_success(msg: Option(String)) -> Element(a) {
  case msg {
    Some(msg) -> html.span([
      a.style([
        #("color","red")
      ])
    ], [text(msg)])
    None -> html.span([],[])
  }
}

fn show_error(error: Option(String)) -> Element(a) {
  case error {
    None -> html.span([],[])
    Some(error) -> html.span([
      a.style([
        #("color", "red")
      ])
    ],[text(error)])
  }
}
// gleam run -m lustre/dev build --entry=pages/manual_trade/manual_trade --outdir=../server/priv/static/
