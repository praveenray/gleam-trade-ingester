import gleam/result
import gleam/order
import shared/data_types as types
import gleam/bool
import gleam/io
import gleam/int
import gleam/float
import gleam/option.{type Option, Some, None}
import gleam/list
import gleam/string
import gleam/uri

pub fn date_to_string(date: types.Date) -> String {
  let two_digits = fn(num: Int) -> String {
    bool.guard(num <= 9, "0", fn(){""}) <> int.to_string(num)
  }
  case date {
    types.InvalidDate -> "Invalid Date"
    types.Date(year,month,day) -> [year, month, day] |> list.map(two_digits(_)) |> string.join("-")
  }
}

pub fn string_to_date(date: String) -> types.Date {
  let dt = string.trim(date)
  let check_date = fn(split_char) {
    case string.split(dt, split_char) |> list.map(string.trim) {
      [y,m,d] -> case int.parse(y), int.parse(m), int.parse(d) {
        Ok(yy),Ok(mm),Ok(dd) -> {
        bool.guard(is_valid_date(yy,mm,dd), types.Date(year: yy, month:mm, day: dd), fn(){types.InvalidDate})
        }
        _,_,_ -> types.InvalidDate
      }
      _ -> types.InvalidDate
    }
  }
  let dash = check_date("-")
  bool.guard(dash == types.InvalidDate, check_date("/"), fn(){dash})
}

pub fn string_to_positive_float(num_string: String) -> Option(Float) {
  let is_positive = fn(value: Float) -> Bool {
    bool.guard(float.compare(value, 0.0) == order.Gt, True, fn(){ False })
  }

  let str = string.trim(num_string)
  case parse_float(str) {
    None -> None
    Some(f) -> bool.guard(is_positive(f), Some(f), fn() {None})
  }
}

pub fn parse_float(v: String) -> Option(Float) {
  case string.trim(v) {
    "" -> None
    value -> case float.parse(value), int.parse(value) {
      Ok(f), Error(_) -> Some(f)
      Ok(f), Ok(_) -> Some(f)
      Error(_), Ok(i) -> Some(int.to_float(i))
      _,_ -> None
    }
  }
}

pub fn is_float_between(value: Float, lower: Float, upper: Float) -> Bool {
  case float.compare(value, lower) {
    order.Lt -> False
    _ -> case float.compare(value, upper) {
      order.Gt -> False
      _ -> True
    }
  }
}

pub fn is_int_between(value: Int, lower: Int, upper: Int) -> Bool {
  case int.compare(value, lower) {
    order.Lt -> False
    _ -> case int.compare(value, upper) {
      order.Gt -> False
      _ -> True
    }
  }
}

pub fn create_uri(base: String, suffix: Option(String), query_params: List(#(String, String))) -> Result(String, String) {
  let front = "^/"
  let back = "/$"

  use base <- result.try(replace_re_with(string.trim(base), front, ""))
  use base <- result.try(replace_re_with(base, back, ""))
   
  let path = case suffix {
    Some(suff) -> {
      use path <- result.try(replace_re_with(string.trim(suff), front, ""))
      use path <- result.try(replace_re_with(path, back, ""))
      Ok(path)
    }
    None -> Ok("")
  }
  case path, query_params {
    Ok(path),[] -> Ok("/" <> base <> "/" <> path)
    Ok(path),qp -> Ok("/" <> base <> "/" <> path <> "?" <> uri.query_to_string(qp))
    error,_ -> error
  }
}

@external(erlang, "erlang_utils", "join_paths")
@external(javascript, "../js_utils.mjs", "join_paths")
pub fn join_paths(paths: List(String)) -> String

@external(erlang, "erlang_utils", "validate_date")
@external(javascript, "../js_utils.mjs", "validate_date")
pub fn is_valid_date(year: Int, month: Int, day: Int) -> Bool

@external(erlang, "erlang_utils", "full_url")
@external(javascript, "../js_utils.mjs", "full_url")
pub fn full_url(path: String) -> String

@external(erlang, "erlang_utils", "replace_re_with")
pub fn replace_re_with(target: String, re: String, replacement: String) -> Result(String, String)

