import gleam/int
import gleam/string
import gleam/list
import gleam/option.{type Option, Some, None}
import gleam/json

pub type Date {
    Date(year: Int, month: Int, day: Int)
    InvalidDate
}

pub type RawTrade {
  RawTrade(
    ticker: String,
    ticker_error: Option(String),
    date: String,
    date_error: Option(String),
    price: String,
    price_error: Option(String),
    quantity: String,
    quantity_error: Option(String),
    action: String,
    action_error: Option(String),
    error: Option(String),
    success: Option(String),
  )
}

pub fn new_raw_trade() -> RawTrade {
  RawTrade(
    ticker: "", ticker_error: None,
    price: "", price_error: None,
    quantity: "", quantity_error: None,
    date: "", date_error: None,
    action: "", action_error: None,
    error: None,
    success: None,
  )
}

pub fn rawtrade_to_json(state: RawTrade) -> json.Json {
  json.object([
    #("ticker", json.string(state.ticker)),
    #("ticker_error", json.string(option.unwrap(state.ticker_error, ""))),
    #("price", json.string(state.price)),
    #("price_error", json.string(option.unwrap(state.price_error, ""))),
    #("quantity", json.string(state.quantity)),
    #("quantity_error", json.string(option.unwrap(state.quantity_error, ""))),
    #("date", json.string(state.date)),
    #("date_error", json.string(option.unwrap(state.date_error, ""))),
    #("action", json.string(state.action)),
    #("action_error", json.string(option.unwrap(state.action_error, ""))),
    #("error", json.string(option.unwrap(state.error, ""))),
    #("success", json.string(option.unwrap(state.success, ""))),
  ])
}

pub fn reset_errors(trade: RawTrade) -> RawTrade {
  RawTrade(..trade,
    ticker_error: None,
    price_error: None,
    quantity_error: None,
    date_error: None,
    action_error: None,
    error: None,
    success: None,
  )
}

pub fn has_error(raw: RawTrade) -> Bool {
  list.any([raw.ticker_error, raw.price_error,raw.quantity_error,raw.date_error,raw.action_error,raw.error], fn(e) {
    case e {
      Some(_) -> True
      None -> False
    }
  })
}
