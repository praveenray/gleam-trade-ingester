import gleam/string
import gleam/list
import trades/data_types as trade_types
import gleam/result
import gleam/option.{type Option, Some, None}
import gleam/float
import gleam/erlang.{type Reference}
import gleam/io
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}

pub type SortOrder {
  Asc
  Desc
}
pub type Sort {
  Sort(direction: SortOrder, key: String)
}
pub fn new_sort() {
  Sort(direction: Asc, key: "symbol")
}
pub type Pagination {
    Pagination(start: Int, page_size: Int)
}
pub fn new_pagination() { Pagination(start:1, page_size: 100) }

pub type TickerAggregate {
  TickerAggregate(
    ticker: String,
    buy_qty: Float,
    buy_dollars: Float,
    sell_qty: Float,
    sell_dollars: Float,
    avg_buy_price: Float,
    avg_sell_price: Float,
    profit_loss: Float,
  )
}

fn new_ticker_aggregate(ticker: String) -> TickerAggregate {
  TickerAggregate(ticker: ticker,
    buy_qty: 0.0,
    buy_dollars: 0.0, sell_qty: 0.0, sell_dollars: 0.0, avg_buy_price: 0.0, avg_sell_price: 0.0,
    profit_loss: 0.0,
  )
}

@external(erlang, "query_erlang", "match_trades_for_ticker")
pub fn match_trades_for_ticker(ticker: String, ets_table: Reference) -> Result(List(trade_types.Trade), String)

pub fn profit_loss_for_ticker(ticker: String, trades_table: Reference) -> Result(Float, String) {
  let ticker = string.uppercase(ticker)
  use transactions <- result.try(match_trades_for_ticker(ticker, trades_table))
  let buckets = aggregate_tickers(transactions, dict.new())
  case dict.get(buckets, ticker) {
    Ok(bucket) -> Ok(bucket.profit_loss)
    Error(_) -> Ok(0.0)
  }
}

pub fn profit_loss_by_tickers(trades_table: Reference) -> List(TickerAggregate) {
  all_trades(trades_table, 100, dict.new(), fn(list, aggregate) {
    aggregate_tickers(list, aggregate)
  })
  |> dict.values() |> list.sort(fn(a,b) { string.compare(a.ticker, b.ticker) })
}

pub fn all_trades(trades_table: Reference, _limit: Int, context: a, callback: fn(List(trade_types.Trade), a) -> a) -> a {
  let sort = Sort(Asc, "_none")
  let page = Pagination(-1, -1)
  let #(results_list, _) = all_transactions(trades_table, sort, page)
  results_list |> process_matched_trades_result(callback, context)
}
@external(erlang, "query_erlang", "sorter")
pub fn sorter(sort: Sort, page: Pagination) -> List(String)

@external(erlang, "query_erlang", "all_transactions")
pub fn all_transactions(
  trades_table: Reference,
  sort: Sort,
  pagination: Pagination
) -> #(List(trade_types.Trade), Int)

/////
fn process_matched_trades_result(
  result: List(trade_types.Trade),
  callback: fn(List(trade_types.Trade), a) -> a,
  context: a
) -> a {
  case result {
    [] -> context
    _ -> callback(result, context)
  }
}

@external(erlang, "query_erlang", "all_transactions")
fn all_transactions_more(continuation: Dynamic) -> Result(#(List(trade_types.Trade), Dynamic), String)

fn aggregate_tickers(transactions: List(trade_types.Trade), buckets: Dict(String, TickerAggregate)) -> Dict(String, TickerAggregate) {
  case transactions {
    [] -> buckets
    [transaction, ..rest] -> {
      let current = result.unwrap(dict.get(buckets, transaction.symbol), new_ticker_aggregate(transaction.symbol))
      let dollars = float.multiply(transaction.price, transaction.quantity)
      let new_ticker_aggregate = case transaction.action {
        trade_types.Buy -> {
          let buy_qty = float.add(transaction.quantity, current.buy_qty)
          let buy_dollars = float.add(current.buy_dollars, dollars)
          let avg_price = result.unwrap(float.divide(buy_dollars, buy_qty), 0.0)
          TickerAggregate(..current, buy_qty: buy_qty, buy_dollars: buy_dollars, avg_buy_price: avg_price)
        }
        trade_types.Sell -> {
          let sell_qty = float.add(transaction.quantity, current.sell_qty)
          let sell_dollars = float.add(current.sell_dollars, dollars)
          let avg_price = result.unwrap(float.divide(sell_dollars, sell_qty),0.0)
          TickerAggregate(..current, sell_qty: sell_qty, sell_dollars: sell_dollars, avg_sell_price: avg_price)
        }
        trade_types.NA -> current
      }

      let new_ticker_aggregates = dict.insert(
        buckets,
        transaction.symbol,
        TickerAggregate(..new_ticker_aggregate, profit_loss: compute_pl(new_ticker_aggregate))
      )
      aggregate_tickers(rest, new_ticker_aggregates)
    }
  }
}

fn compute_pl(bucket: TickerAggregate) -> Float {
  let zero = 0.0
  case float.loosely_equals(bucket.sell_qty, zero, 0.001) {
    True -> zero
    False -> {
      case float.loosely_equals(bucket.avg_buy_price, zero, 0.001) {
        False -> {
          let avg_buy_dollars = float.multiply(bucket.sell_qty, bucket.avg_buy_price)
          let avg_sell_dollars = float.multiply(bucket.sell_qty, bucket.avg_sell_price)
          float.subtract(avg_sell_dollars, avg_buy_dollars)
        }
        _ -> zero
      }
    }
  }
}
