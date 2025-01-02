import gleam/string
import gleam/int
import gleam/option.{Some,None}
import gleam/list
import globals
import trades/writer_actor_messages as messages
import trades/writer
import gleam/string_tree
import gleam/function
import gleam/io
import gleam/otp/actor
import gleam/erlang/process.{type Subject}
import gleam/erlang.{type Reference}
import trades/data_types as types

pub type MyState {
  MyState(trades_table: Reference, avg_load_millis: Int)
}

pub fn start_actor(
  global_table: Reference,
  trades_table: Reference
) -> Result(Subject(messages.Message), actor.StartError) {
  actor.Spec(
    fn() { init(global_table, trades_table) },
    10_000,
    loop
  )
  |> actor.start_spec()
}

fn init(global_table: Reference, trades_table: Reference) -> actor.InitResult(MyState, messages.Message) {
  io.debug("Writer starting " <> string.inspect(process.self()))
  let myself = process.new_subject()
  let assert True = globals.add_trade_loader_subject(myself, global_table)
  io.println("added trades_loader_subject")
  let selector = process.new_selector() |> process.selecting(myself, function.identity)
  actor.Ready(MyState(
    trades_table: trades_table,
    avg_load_millis: 0,
  ), selector)
}

pub fn loop(msg: messages.Message, state: MyState) -> actor.Next(messages.Message, MyState) {
  case msg {
    messages.LoadFromDirectory(dir_name, sender) -> load_from_directory(dir_name, state.trades_table, sender)
    messages.LoadFromFiles(files, sender) -> load_files(files, state.trades_table, sender)
    messages.InsertOneTrade(trade, sender) -> insert_one_trade(trade, state.trades_table, sender)
    _ -> panic as "Invalid Message"
  }
  actor.continue(state)
}

fn load_from_directory(dir_name: String, trades_table: Reference, sender: Subject(messages.MessageResponses)) {
  let results = writer.load_trades(dir_name, trades_table)
  process.send(sender,
    messages.LoadedFromDirectory(dir_name, results)
  )
}

fn load_files(files: List(String), trades_table: Reference, sender: Subject(messages.MessageResponses)) {
  let results = writer.load_csv_files(files, trades_table)
  process.send(sender, messages.LoadedFromFiles(files, results))
}

fn summarize_results(results: List(types.FileLoadResult)) -> String {
  list.fold(results, string_tree.new(), fn(bldr, one_result) {
    bldr
    |> string_tree.append("File: ") |> string_tree.append(one_result.file_name)
    |> string_tree.append(", Loaded Count: ") |>  string_tree.append(int.to_string(one_result.loaded_count))
    |> string_tree.append(", Error Count: ")  |>  string_tree.append(int.to_string(list.length(one_result.errors)))
    |> string_tree.append("\n")
  })
  |> string_tree.to_string()
}

fn insert_one_trade(
  trade: types.Trade,
  trades_table: Reference,
  sender: Subject(messages.MessageResponses)) {
  case writer.insert_one_trade(trade, trades_table) {
    Ok(_) -> process.send(sender, messages.InsertOneTradeResponse(trade: trade, error: None))
    Error(msg) -> process.send(sender, messages.InsertOneTradeResponse(trade: trade, error: Some(msg)))
  }
}
