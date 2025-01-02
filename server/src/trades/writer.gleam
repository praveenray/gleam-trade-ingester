import gleam/float
import gleam/int
import gleam/io
import gleam/result
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import gleam/erlang.{type Reference}
import gleam/string_tree
import file_streams/file_stream.{type FileStream}
import file_streams/file_stream_error
import trades/data_types as types
import shared/data_types as shared

pub fn load_trades(
  folder: String,
  trades_table: Reference
) -> List(types.FileLoadResult) {
  case get_csv_files_in_dir(folder) {
    Ok(list) -> load_csv_files(list, trades_table)
    Error(msg) -> [new_load_result(Some(msg))]
  }
}


pub fn insert_one_trade(
  trade: types.Trade,
  trades_table: Reference
) -> Result(Nil, String) {
  case write_to_ets(trade, 1, trades_table) {
    KeepGoing -> Ok(Nil)
    _ -> Error("Error inserting into ets table")
  }
}

pub fn load_csv_files(list: List(String), ets_table: Reference) -> List(types.FileLoadResult) {
  list
  |> list.fold([], fn(statuses, file) {
    [read_file(file, ets_table), ..statuses]
  })
}

fn record_key(file_record: types.Trade) -> String {
  string_tree.new()
  |> string_tree.append(string.inspect(file_record.run_date))
  |> string_tree.append(file_record.symbol)
  |> string_tree.append(float.to_string(file_record.quantity))
  |> string_tree.append(float.to_string(file_record.price))
  |> string_tree.append(string.inspect(file_record.action))
  |> string_tree.to_string
}

fn read_file(file_name: String, ets_table: Reference) -> types.FileLoadResult {
  let status = types.FileLoadResult(..types.new_file_load_result(), file_path: file_name)
  
  case file_stream.open_read(file_name) {
    Ok(file) -> {
      case skip_bom_if_any(file) {
       Ok(fh) -> case read_header(fh, 0) {
          Ok(#(header_index, line_num)) -> {
            read_lines(fh, header_index, line_num+1, ets_table, write_to_ets, status)
          }
          Error(HeaderReadError(error)) ->
            types.FileLoadResult(..status, errors: [#(-1, "Error reading header " <> error)])
       }
       _ ->  types.FileLoadResult(..status, errors: [#(-1, "Error skipping BOM ")])
      }
    }
    Error(error) -> types.FileLoadResult(
      ..status,
      errors: [#(-1, "Error opening file " <> file_stream_error.describe(error))]
    )
  }
}

fn skip_bom_if_any(fh: file_stream.FileStream) -> Result(file_stream.FileStream, file_stream_error.FileStreamError) {
  use bits <- result.try(file_stream.read_bytes(fh, 3))
  case bits {
    <<0xefbbbf:24>> -> Ok(fh)
    _ -> {
        use _pos <- result.map(file_stream.position(fh, file_stream.BeginningOfFile(0)))
        fh
    }
  }
}

pub fn write_to_ets(record: types.Trade, _line_num: Int, table_ref: Reference) -> ETSWriteResult {
  let record = types.Trade(..record, symbol: string.uppercase(record.symbol))
  let key = record_key(record)
  case insert_key_value(table_ref, key, record) {
    False -> FatalError
    True  -> KeepGoing
  }
}

fn read_header(fh: FileStream, line_num: Int) -> Result(#(HeaderIndex, Int), HeaderReadError) {
  let empty_index =
    HeaderIndex(run_date: 0, symbol: 0, price: 0, quantity: 0, action: 0)

  case file_stream.read_line(fh) {
    Ok(line) -> {
      case string.trim(line) {
        "" -> read_header(fh, line_num+1)
        line -> {
          case string.split(line, ",") {
            [] -> read_header(fh, line_num+1)
            tokens -> {
              let #(header, _) =
                tokens
                |> list.map(string.trim)
                |> list.fold(#(empty_index, 0), fn(acc, token) {
                  let #(header, index) = acc
                  #(fill_header(token, index, header), index + 1)
                })
              Ok(#(header, line_num))
            }
          }
        }
      }
    }
    Error(file_stream_error.Eof) ->
      Error(HeaderReadError(error: "Reached end of file without reading header"))
    Error(error) ->
      Error(HeaderReadError(error: file_stream_error.describe(error)))
  }
}

fn fill_header(
  token: String,
  index: Int,
  current_header: HeaderIndex,
) -> HeaderIndex {
  case string.trim(token) {
    "Run types.Date" -> HeaderIndex(..current_header, run_date: index)
    "Symbol" -> HeaderIndex(..current_header, symbol: index)
    "Quantity" -> HeaderIndex(..current_header, quantity: index)
    "Price" -> HeaderIndex(..current_header, price: index)
    "Action" -> HeaderIndex(..current_header, action: index)
    _ -> current_header
  }
}

fn read_lines(
  fh: FileStream,
  header: HeaderIndex,
  line_num: Int,
  callback_ctx: a,
  callback: fn(types.Trade, Int, a) -> ETSWriteResult,
  result: types.FileLoadResult,
) -> types.FileLoadResult {
  case file_stream.read_line(fh) {
    Ok(line) -> {
      case tokenize(line, header) {
        Some(record) -> {
          case callback(record, line_num, callback_ctx) {
            KeepGoing -> read_lines(
              fh,
              header,
              line_num + 1,
              callback_ctx,
              callback,
              types.FileLoadResult(..result, loaded_count: result.loaded_count+1)
            )
            FatalError -> {
              io.debug("Error writing to ets")
              types.FileLoadResult(
                ..result,
                errors: [#(-1,"Error Writing into ETS Table. Line: " <> int.to_string(line_num))]
              )
            }
            Stop -> result
          }
        }
        None -> read_lines(fh, header, line_num + 1, callback_ctx, callback, result)
      }
    }
    Error(file_stream_error.Eof) -> result
    Error(error) -> {
      let message = "Error with read_line " <> file_stream_error.describe(error)
      io.debug(message)
      types.FileLoadResult(..result, errors: list.append(result.errors, [#(line_num, message)]))
    }
  }
}

fn tokenize(line: String, header_map: HeaderIndex) -> Option(types.Trade) {
  case string.trim(line) {
    "" -> None
    line -> {
      line
      |> string.split(",")
      |> list.map(string.trim)
      |> extract_fields(header_map)
    }
  }
}

fn extract_fields(
  fields: List(String),
  header_map: HeaderIndex,
) -> Option(types.Trade) {
  let empty_record =
    types.Trade(
      run_date: shared.Date(0, 0, 0),
      symbol: "",
      quantity: 0.0,
      price: 0.0,
      action: types.NA,
    )

  let field_record = list.index_fold(fields, Some(empty_record), fn(optional_record, field, index) {
    case optional_record {
      Some(record) ->
        case index {
          _ if index == header_map.run_date -> extract_run_date(field, record)
          _ if index == header_map.symbol -> extract_symbol(field, record)
          _ if index == header_map.quantity -> extract_quantity(field, record)
          _ if index == header_map.price -> extract_price(field, record)
          _ if index == header_map.action -> extract_action(field, record)
          _ -> optional_record
        }
      _ -> None
    }
  })

  case field_record {
    Some(types.Trade(_,"",_,_,types.NA)) -> None
    Some(r) -> Some(r)
    _ -> None
  }
}

fn extract_action(field: String, record: types.Trade) -> Option(types.Trade) {
  case string.contains(field, " BOUGHT ") {
    True -> Some(types.Trade(..record, action: types.Buy))
    False ->
      case string.contains(field, " SOLD ") {
        True -> Some(types.Trade(..record, action: types.Sell))
        False -> None
      }
  }
}

fn extract_price(field: String, record: types.Trade) -> Option(types.Trade) {
  case string.trim(field) {
    "" -> None
    price ->
      case try_float(price) {
        Some(num) -> Some(types.Trade(..record, price: num))
        _ -> None
      }
  }
}

fn extract_quantity(field: String, record: types.Trade) -> Option(types.Trade) {
  case string.trim(field) {
    "" -> None
    qty ->
      case try_float(qty) {
        Some(num) -> Some(types.Trade(..record, quantity: float.absolute_value(num)))
        _ -> None
      }
  }
}

fn try_float(token: String) -> Option(Float) {
  let str_float = case string.contains(token, ".") {
    True -> token
    False -> token <> "." <> "0"
  }
  case float.parse(str_float) {
    Ok(num) -> Some(num)
    Error(_) -> None
  }
}

fn extract_symbol(field: String, record: types.Trade) -> Option(types.Trade) {
  case string.trim(field) {
    "" -> None
    symbol -> Some(types.Trade(..record, symbol: string.uppercase(symbol)))
  }
}

fn extract_run_date(field: String, record: types.Trade) -> Option(types.Trade) {
  case string.split(field, "/") |> list.map(string.trim) {
    [m, d, y] ->
      case int.parse(y), int.parse(m), int.parse(d) {
        Ok(year), Ok(month), Ok(day) ->
          Some(types.Trade(..record, run_date: shared.Date(year, month, day)))
        _, _, _ -> option.None
      }
    _ -> None
  }
}

// HeaderIndex(0, 3, 10, 8, 2)
type HeaderIndex {
  HeaderIndex(
    run_date: Int,
    symbol: Int,
    price: Int,
    quantity: Int,
    action: Int,
  )
}

type HeaderReadError {
  HeaderReadError(error: String)
}

pub type ETSWriteResult {
    KeepGoing
    FatalError
    Stop
}

fn new_load_result(error: Option(String)) -> types.FileLoadResult {
  types.FileLoadResult(
    file_name: "",
    file_path: "",
    loaded_count: 0,
    errors: case error {
      Some(msg) -> [#(-1, msg)]
      None -> []
    },
  )
}

fn create_ets_table(table_name: String, file_name: String) -> Result(Reference, String) {
  case try_ets_table_present(table_name) {
    Error(_) -> case file2tab(file_name) {
      Ok(t) -> Ok(t)
      Error(_) -> create_global_ets_table(table_name)
    }
    Ok(tid) -> Ok(tid)
  }
}

@external(erlang, "writer_erlang", "validate_date")
pub fn is_valid_date(
  year: Int,
  month: Int,
  day: Int,
) -> Result(#(Int, Int, Int), String)

@external(erlang, "writer_erlang", "create_md5")
pub fn create_md5(input: String) -> String

@external(erlang, "globals_erlang", "create_global_ets_table")
pub fn create_global_ets_table(table_name: String) -> Result(Reference, String)

@external(erlang, "globals_erlang", "try_ets_table_present")
pub fn try_ets_table_present(table_name: String) -> Result(Reference, String)

@external(erlang, "writer_erlang", "all_ets_tables")
pub fn all_ets_tables() -> List(String)

pub fn insert_key_value(table_name: Reference, key: String, value: a) -> Bool {
    let data = #(key, value)
    insert_into_table(table_name, data)
}
@external(erlang, "writer_erlang", "insert_into_table")
pub fn insert_into_table(table_name: Reference, data: a) -> Bool

@external(erlang, "writer_erlang", "lookup_table")
pub fn lookup_table(table_name: String, key: String) -> List(#(String, types.Trade))

@external(erlang, "writer_erlang", "delete_table")
pub fn delete_table(table_name: String) -> Bool

@external(erlang, "globals_erlang", "file2tab")
pub fn file2tab(file_name: String) -> Result(Reference, String)

@external(erlang, "writer_erlang", "get_csv_files_in_dir")
fn get_csv_files_in_dir(dir_name: String) -> Result(List(String), String)
