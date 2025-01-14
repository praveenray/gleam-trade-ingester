import gleam/list
import gleam/string
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
import lustre/ui/combobox
import lustre/ui/input
import lustre/ui/button
import lustre/ui/card
import shared/utils as shared_utils

pub const path_prefix = "/manual-trade"

pub type Msg {
  TickerUpdated(value: String)
  QuantityUpdated(value: String)
  PriceUpdated(value: String)
  ActionUpdated(value: String)
  DateUpdated(value: String)
  TradeValuesEntered()
  TradeValuesResponseReceived(response_state: types.RawTrade)
}

pub type State {
  State(raw_trade: types.RawTrade, effect: effect.Effect(Msg), waiting: Bool)
}

pub fn main() {
  io.debug("Manual Trade Starting")
  let assert Ok(_) = combobox.register()
  
  case lustre.start(lustre.application(init, update, create_form), "#app", Nil) {
    Ok(_) -> io.debug("Manual Trade Started")
    _ -> io.debug("Failed to Start App")
  }
}

fn init(_) -> #(State, effect.Effect(Msg)) {
  let state = State(types.new_raw_trade(), effect.none(), False)
  #(state, state.effect)
}

fn update(mystate: State, msg: Msg) -> #(State, effect.Effect(Msg)) {
  let state = mystate.raw_trade
  let new_state = case msg {
    TickerUpdated(value) -> State(types.RawTrade(..state, ticker: value), effect.none(), False)
    PriceUpdated(value) -> State(types.RawTrade(..state, price: value), effect.none(), False)
    QuantityUpdated(value) -> State(types.RawTrade(..state, quantity: value), effect.none(), False)
    DateUpdated(value) -> State(types.RawTrade(..state, date: value), effect.none(), False)
    ActionUpdated(value) -> State(types.RawTrade(..state, action: value), effect.none(), False)
    TradeValuesEntered -> {
      let with_no_errors = types.reset_errors(state)
      State(with_no_errors, post_api(with_no_errors), True)
    }
    TradeValuesResponseReceived(response_state) -> {
      
      State(response_state, effect.none(), False)
    }
  }
  #(new_state, new_state.effect)
}

fn post_api(state: types.RawTrade) -> effect.Effect(Msg) {
  let full_url = shared_utils.full_url(path_prefix)
  io.debug("Posting to : " <> full_url)
  lh.post(full_url, types.rawtrade_to_json(state),
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
        lh.NetworkError  -> types.RawTrade(..client_state, error: Some("Network Error"))
        lh.BadUrl(_)     -> types.RawTrade(..client_state, error: Some("Bad URL"))
        lh.NotFound      -> types.RawTrade(..client_state, error: Some("Not Found"))
        lh.InternalServerError(_) -> types.RawTrade(..client_state, error: Some("Server Error Saving Trade"))
      }
    }
  }
}

fn populate_state_with_response(target: Dynamic) -> Result(types.RawTrade, List(dynamic.DecodeError)) {
  shared.state_from_dynamic_best_case(target, types.new_raw_trade()) |> Ok()
}

fn form_control_style(styles: List(#(String, String))) -> a.Attribute(a) {
  a.style(list.append(styles, [
    #("display", "flex"),
    #("width", "650px"),
  ]))
}

fn control_label(label: String) -> Element(a) {
  html.label([a.style([
      #("width", "25%")
  ])], [text(label <> ": ")])
}

fn control_with_error_styles() -> List(#(String, String)) {
  [
    #("display","flex"),
    #("flex-direction","column"),
    #("flex-grow","1"),
  ]
}
fn input_element(
  name: String,
  value: String,
  label: String,
  msg: fn(String) -> Msg,
  error: Option(String),
) -> Element(Msg) {
  div([
    form_control_style([])
  ],[
    control_label(label),
    div([
      a.style(control_with_error_styles())
    ],[
      input.input([
        a.name(name),
        a.attribute("maxlength", "10"),
        a.attribute("minlength", "1"),
        a.value(value),
        event.on("change", to_msg(_,msg)),
      ]),
      show_error(error),
    ])
  ])
}

fn action_choices(
  trade: types.RawTrade
) -> Element(Msg) {
  let buy = "Buy"
  let sell = "Sell"
  div([
    form_control_style([])
  ], [
    control_label("Action"),
    div([a.style(control_with_error_styles())],[
      combobox.combobox([
        a.style([#("flex-grow","1")]),
        combobox.value(trade.action),
        combobox.on_change(ActionUpdated)
      ],[
        combobox.option(value: "", label: ""),
        combobox.option(value: buy, label: buy),
        combobox.option(value: sell, label: sell),
      ]),
      show_error(trade.action_error),
    ])
  ])
}

fn submit_button(state: State) -> Element(Msg) {
  case state.waiting {
    True -> button.button([a.disabled(True)],[text("Waiting")])
    False -> button.button([
      event.on_click(TradeValuesEntered),
    ],[text("Save")])
  }
}

fn create_form(state: State) -> Element(Msg) {
  let trade = state.raw_trade
  let action_selected = trade.action

  io.debug(action_selected)
  card.card([],[
    card.header([],[html.h4([
      a.style([
        #("width", "100%"),
        #("display", "flex"),
        #("justify-content","center"),
      ])
    ],[text("Trade Data")])]),
    card.content([], [
      div([
        a.style([
          #("width", "100%"),
          #("min-height", "300px"),
          #("display", "flex"),
          #("flex-direction", "column"),
          #("justify-content","space-evenly"),
          #("gap", "20px"),
          #("align-items", "center"),
        ])
      ],[
        show_page_error(trade.error),
        show_page_success(trade.success),
        input_element("ticker", trade.ticker, "Ticker", TickerUpdated, trade.ticker_error),
        input_element("quantity", trade.quantity, "Quantity", QuantityUpdated,  trade.quantity_error),
        input_element("price", trade.price, "Price", PriceUpdated, trade.price_error),
        input_element("date", trade.date, "Date (YYYY-MM-DD)", DateUpdated, trade.date_error),
        action_choices(trade),
        div([
          form_control_style([
            #("justify-content", "flex-end"),
            #("align-items", "center"),
            #("gap","10px"),
          ])
        ],[
          submit_button(state),
          html.a([a.href("/home")],[text("Cancel")])
        ])
      ])
    ])
  ])
}

fn to_msg(event: Dynamic, msg: fn(String) -> Msg) -> Result(Msg, List(dynamic.DecodeError)) {
  use target <- result.try(dynamic.field("target", dynamic.dynamic)(event))
  io.debug("Target " <> string.inspect(target))
  use value <- result.try(dynamic.field("value", dynamic.string)(target))
  io.debug("Called " <> value)
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
        #("color","green")
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
        #("color", "red"),
        #("display","flex"),
        #("justify-content","flex-end"),
      ])
    ],[text(error)])
  }
}

// gleam run -m lustre/dev start --entry=pages/manual_trade/manual_trade
// gleam run -m lustre/dev build --entry=pages/manual_trade/manual_trade --outdir=../server/priv/static/
