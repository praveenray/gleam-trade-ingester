import gleam/option.{type Option}
import gleam/erlang/process.{type Subject}
import shared/data_types as shared
import trades/data_types as types

pub type Message {
    LoadFromDirectory(
      dir_name: String,
      sender: Subject(MessageResponses)
    )
    LoadFromFiles(
      files: List(String),
      sender: Subject(MessageResponses)
    )
    InsertOneTrade(
      trade: types.Trade,
      sender: Subject(MessageResponses)
    )
    QueryTradesBetweenDates(
      ticker: String,
      start_date: shared.Date,
      end_date: shared.Date,
      sender: Subject(MessageResponses)
    )
    ProfitLossForTicker(ticker: String, sender: Subject(MessageResponses))
    ProfitLossPerTicker(sender: Subject(MessageResponses))
    ProfitLossBook(sender: Subject(MessageResponses))
    DriverSelfMessage
    Quit
}

pub type MessageResponses {
    LoadedFromDirectory(dir_name: String, results: List(types.FileLoadResult))
    LoadedFromFiles(files: List(String), results: List(types.FileLoadResult))
    InsertedOneTrade(error: Option(String))
    QueriedTradesBetweenDates(
      trades: List(types.Trade),
      ticker: String,
      start_date: shared.Date,
      end_date: shared.Date,
    )
    InsertOneTradeResponse(
      trade: types.Trade,
      error: Option(String),
    )
    ProfitLossForTickerResponse(ticker: String, pl: Float)
    ProfitLossPerTickerResponse(pl: List(#(String, Float)))
    ProfitLossBookResponse(pl: Float)
    QueryError(error: String)
}
