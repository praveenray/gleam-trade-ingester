import gleam/option.{type Option, Some, None}
import shared/data_types as shared
import shared/utils
import gleam/string

pub type BuySell {
  Buy
  Sell
  NA
}

pub fn string_to_action(str: String) -> Option(BuySell) {
  case string.trim(str) {
    "Buy" -> Some(Buy)
    "Sell" -> Some(Sell)
    "NA" -> Some(NA)
    _ -> None
  }
}

pub type Trade {
  Trade(
    run_date: shared.Date,
    symbol: String,
    quantity: Float,
    price: Float,
    action: BuySell,
  )
}
pub fn new_trade() -> Trade {
  Trade(
    run_date: shared.InvalidDate,
    symbol: "",
    quantity: -1.0, // intentionally wrong!
    price: -1.0,
    action: NA
  )
}

pub type FileLoadResult {
  FileLoadResult(
    file_name: String,
    file_path: String,
    loaded_count: Int,
    errors: List(#(Int, String)),
  )
}
pub fn new_file_load_result() -> FileLoadResult {
  FileLoadResult(
    file_name:  "",
    file_path: "",
    loaded_count: 0,
    errors: [],
  )
}

pub fn validate_trade(trade: Trade) -> Result(Trade, List(String)) {
  let errors = []
  let errors = case trade.run_date {
    shared.InvalidDate -> ["Invalid run date", ..errors]
    _ -> errors
  }
  let symbol = string.trim(trade.symbol)
  let errors = case string.length(symbol) {
    0 -> ["Missing Symbol", ..errors]
    n if n > 8 -> ["Symbol is too large", ..errors]
    _ -> errors
  }
  let errors = case utils.is_float_between(trade.quantity, 1.0, 10_000.0) {
    False -> ["Quantity must be within 1.0 and 10_000", ..errors]
    _ -> errors
  }
  let errors = case utils.is_float_between(trade.price, 0.1, 10_000.0) {
    True -> errors
    False -> ["Price must be within 0.1 and 10_000", ..errors]
  }
  let errors = case trade.action {
    NA -> ["Invalid Action", ..errors]
    _ -> errors
  }
  case errors {
    [] -> Ok(trade)
    _ -> Error(errors)
  }
}
