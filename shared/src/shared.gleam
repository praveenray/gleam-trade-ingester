import gleam/order
import gleam/result
import gleam/dynamic.{type Dynamic}
import gleam/option.{Some, None}
import gleam/string
import gleam/int
import gleam/bool
import gleam/list
import gleam/json
import shared/data_types as types
import shared/utils

pub fn state_from_dynamic_best_case(root: Dynamic, client_state: types.RawTrade) -> types.RawTrade {
  let extract_string_field = fn(field_name,default) {
    dynamic.field(field_name, dynamic.string)(root)
    |> result.unwrap(default)
    |> string.trim()
  }
  let to_option = fn(str) {
    case str {
      "" -> None
      s -> Some(s)
    }
  }

  types.RawTrade(
    ticker: extract_string_field("ticker", client_state.ticker),
    ticker_error: extract_string_field("ticker_error","") |> to_option(),
    price: extract_string_field("price", client_state.price),
    price_error: extract_string_field("price_error", "") |> to_option(),
    quantity: extract_string_field("quantity", client_state.quantity),
    quantity_error: extract_string_field("quantity_error", "") |> to_option(),
    date: extract_string_field("date", client_state.date),
    date_error: extract_string_field("date_error", "") |> to_option(),
    action: extract_string_field("action", client_state.action),
    action_error: extract_string_field("action_error", "") |> to_option(),
    error: extract_string_field("error","") |> to_option(),
    success: extract_string_field("success","") |> to_option(),
  )
}

pub fn state_from_string_best_case(resp: String, client_state: types.RawTrade) -> types.RawTrade {
  let st = json.decode(resp, fn(root) {
    state_from_dynamic_best_case(root, client_state) |> Ok()
  })
  
  case st {
    Ok(state) -> state
    Error(_)  -> types.RawTrade(..client_state, error: Some("error parsing response into types.RawTrade Object"))
  }
}

pub fn validate_ticker(state: types.RawTrade) -> types.RawTrade {
  let ticker = string.trim(state.ticker)
  case ticker {
    "" -> types.RawTrade(..state, ticker: "", ticker_error: Some("Missing Ticker"))
    t  -> case int.compare(string.length(t), 8) {
        order.Lt | order.Eq -> types.RawTrade(..state, ticker: t, ticker_error: None)
        _ -> types.RawTrade(..state, ticker: t, ticker_error: Some("Ticker is too big"))
    }
  }
}

pub fn validate_action(state: types.RawTrade) -> types.RawTrade {
  let action = string.trim(state.action)
  let trade = types.RawTrade(..state, action: action)
  case action {
    "Buy"|"Sell" -> types.RawTrade(..trade, action_error: None)
    _ -> types.RawTrade(..trade, action_error: Some("Invalid Action"))
  }
}

pub fn validate_price(state: types.RawTrade) -> types.RawTrade {
  let price = string.trim(state.price)
  let price_rec = types.RawTrade(..state, price: price)
  bool.guard(
    utils.string_to_positive_float(price) == None,
    types.RawTrade(..price_rec, price_error: Some("Price must be a Positive Number")),
    fn() {types.RawTrade(..price_rec, price_error: None)}
  )
}

pub fn validate_quantity(state: types.RawTrade) -> types.RawTrade {
  let qty = string.trim(state.quantity)
  let qty_rec = types.RawTrade(..state, quantity: qty)
  bool.guard(
    utils.string_to_positive_float(qty) == None,
    types.RawTrade(..qty_rec, quantity_error: Some("Quantity must be a Positive Number")),
    fn() {types.RawTrade(..qty_rec, quantity_error: None)},
  )
}

pub fn validate_date(state: types.RawTrade) -> types.RawTrade {
  let dt = string.trim(state.date)
  let date_rec = types.RawTrade(..state, date: dt)
  case utils.string_to_date(dt) {
    types.InvalidDate -> types.RawTrade(..date_rec, date_error: Some("Invalid Date"))
    _ -> types.RawTrade(..date_rec, date_error: None)
  }
}

pub fn validate(state: types.RawTrade) -> Result(types.RawTrade, types.RawTrade) {
  let ticker = validate_ticker(state)
  let price = validate_price(ticker)
  let quantity = validate_quantity(price)
  let action = validate_action(quantity)
  let date = validate_date(action)

  let has_error = list.any([ticker.ticker_error, price.price_error, quantity.quantity_error, date.date_error, action.action_error], fn(e) {
    case e {
      Some(_) -> True
      None -> False
    }
  })
  bool.guard(has_error, Error(date), fn() { Ok(date) })
}
