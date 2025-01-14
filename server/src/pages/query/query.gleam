import gleam/erlang.{type Reference}
import gleam/float
import gleam/string
import shared/utils
import wisp.{type Request, type Response}
import gleam/http
import gleam/io
import lustre/attribute as a
import lustre/element/html.{div,text}
import lustre/element.{type Element}
import layouts/top_across_menu as layout
import pages/render_utils
import pages/query/all_transactions
import pages/query/profit_loss
import pages/site_css
import trades/data_types as trade_types
import gleam/list
import gleam/option.{type Option, Some, None}
import query/query
import lustre/ui/card
import lustre/ui/input
import lustre/ui/button

pub const path_prefix = "query"
const transactions_by_ticker = "transactions-by-ticker"
const pl_by_ticker = "pl-by-ticker"

pub fn accept(r: Request, trades_table: Reference) -> Response {
  case r.method {
    http.Get -> case wisp.path_segments(r) {
      [_,s,..] if s == all_transactions.path_prefix -> all_transactions.accept(r, trades_table, path_prefix)
      [_,s] if s == profit_loss.path_prefix -> profit_loss.accept(r, trades_table)
      _ -> index(r)
    }
    http.Post -> case wisp.path_segments(r) {
      [_,s] if s == transactions_by_ticker -> call_for_ticker(r, trades_table, call_transactions_by_ticker)
      [_,s] if s == pl_by_ticker -> call_for_ticker(r, trades_table, call_pl_by_ticker)
      _ -> wisp.bad_request() |> wisp.string_body("Invalid Post Path ")
    }
    _ -> wisp.bad_request() |> wisp.string_body("Invalid Method ")
  }
}

fn index(r: Request) -> Response {
  use <- wisp.require_method(r, http.Get)
  let page = layout.layout(
    mjs_path: None,
    init_json: None,
    add_lustre_ui: True,
    body: body(),
  )
  render_utils.send_element(page, 200)
}

fn body() -> Element(a) {
  let assert Ok(transactions_ticker_uri) = utils.create_uri(path_prefix, Some(transactions_by_ticker), [])
  let assert Ok(pl_ticker_uri) = utils.create_uri(path_prefix, Some(pl_by_ticker), [])
  let assert Ok(all_transactions_uri) = utils.create_uri(path_prefix, Some(all_transactions.path_prefix), [])
  let assert Ok(pl_summary_uri) = utils.create_uri(path_prefix, Some(profit_loss.path_prefix), [])
  let submit_btn = fn(btn_text) {
    button.button([a.style([#("width", "160px")]), a.type_("submit")], [text(btn_text)])
  }

  let ticker_box = input.input([
    a.name("ticker"),
    a.placeholder("Ticker"),
    a.max("10"),
    a.min("1")
  ])

  let form_params = fn(url) {
    [
      a.class(site_css.horizontally_center),
      a.style([
        #("gap", "10px"),
      ]),
      a.action(url),
      a.method("post")
    ]
  }

  let right_aligned = fn(title, url) {
    div([
      a.style([
        #("display","flex"),
        #("justify-content","flex-end"),
        #("width", "100%"),
      ])
    ], [
      button.button([],[
        html.a([a.href(url)],[text(title)])
      ])
    ])
  }

  div([
    a.class(site_css.center)
  ], [
    div([
      a.class(site_css.vertically_center)
    ],[
      html.a([a.href("/home")],[text("Home")]),
      card.card([],[
        card.content([
          a.style([
            #("align-items","flex-start"),
            #("display","flex"),
            #("gap", "10px"),
            #("flex-direction","column"),
          ])
        ],[
          html.form(form_params(transactions_ticker_uri),[
            ticker_box,
            submit_btn("Find Transactions"),
          ]),
          html.form(form_params(pl_ticker_uri),[
            ticker_box,
            submit_btn("Compute Profit/Loss"),
          ]),
          right_aligned("All Transactions", all_transactions_uri),
          right_aligned("Profit/Loss Summary", pl_summary_uri),
        ])
      ])
    ])
  ])
}

fn call_profit_loss_summary(trades_table: Reference) -> Response {
  let aggregate = list.map(query.profit_loss_by_tickers(trades_table), fn(bucket) {
    html.tr([], [
      html.td([], [text(bucket.ticker)]),
      html.td([], [text(float.to_precision(bucket.buy_qty, 2) |> float.to_string())]),
      html.td([], [text(float.to_precision(bucket.avg_buy_price, 2) |> float.to_string())]),
      html.td([], [text(float.to_precision(bucket.sell_qty, 2) |> float.to_string())]),
      html.td([], [text(float.to_precision(bucket.avg_sell_price, 2) |> float.to_string())]),
      html.td([], [text(float.to_precision(bucket.profit_loss, 2) |> float.to_string())]),
    ])
  })
  
  let page = layout.layout(
    mjs_path: None,
    init_json: None,
    add_lustre_ui: True,
    body: div([],[
      html.h4([],[text("Profit/Loss Summary ")]),
      html.table([a.attribute("border","1")],[
        html.tr([],[
          html.th([],[text("Ticker")]),
          html.th([],[text("Total Buys")]),
          html.th([],[text("Avg Buy Price")]),
          html.th([],[text("Total Sells")]),
          html.th([],[text("Avg Sell Price")]),
          html.th([],[text("P/L")]),
        ]),
        ..aggregate
      ])
    ])
  )
  render_utils.send_element(page, 200)
}

fn call_for_ticker(r: Request, trades_table: Reference, callback: fn(String, Reference) -> Response) -> Response {
  use form <- wisp.require_form(r)
  case form_ticker(form) {
    Some(ticker) -> callback(ticker, trades_table)
    None -> wisp.bad_request() |> wisp.string_body("Missing ticker")
  }
}

fn call_pl_by_ticker(ticker: String, trades_table: Reference) -> Response {
  case query.profit_loss_for_ticker(ticker, trades_table) {
    Ok(pl) -> display_pl_ticker(ticker, pl, None)
    Error(e) -> display_pl_ticker(ticker, 0.0, Some("Error computing PL for ticker " <> ticker <> "(" <> e <> ")"))
  }
}

fn display_pl_ticker(ticker: String, pl: Float, error: Option(String)) -> Response {
  let page = layout.simple_layout(
    body: div([],[
      render_utils.show_error(error),
      html.h4([],[text("Profit/Loss for " <> ticker)]),
      html.h5([],[text(pl |> float.to_precision(2) |> float.to_string())])
    ])
  )
  render_utils.send_element(page, 200)
}

fn call_transactions_by_ticker(
  ticker: String,
  trades_table: Reference
) -> Response {
  case query.match_trades_for_ticker(ticker, trades_table) {
    Ok(list) -> display_transactions(list, None)
    Error(e) -> {
      io.debug(e)
      display_transactions([], Some("Error getting transactions for ticker " <> ticker))
    }
  }
}

fn display_transactions(list: List(trade_types.Trade), error: Option(String)) -> Response {
  let page = layout.layout(
    mjs_path: None,
    init_json: None,
    add_lustre_ui: True,
    body: div([],[
      render_utils.show_error(error),
      html.h4([], [text("Transactions By Ticker")]),
      html.table([a.attribute("border","1")],[
        html.tr([], [
          html.th([], [text("Ticker")]),
          html.th([], [text("Run Date")]),
          html.th([], [text("Action")]),
          html.th([], [text("Quantity")]),
          html.th([], [text("Price")]),
        ]),
        ..list.map(list, fn(trade) {
          html.tr([],[
            html.td([], [text(trade.symbol)]),
            html.td([], [text(string.inspect(trade.run_date))]),
            html.td([], [text(string.inspect(trade.action))]),
            html.td([], [text(float.to_string(trade.quantity))]),
            html.td([], [text(float.to_string(trade.price))]),
          ])
        })
      ])
    ]),
  )
  render_utils.send_element(page, 200)
}

fn form_ticker(form: wisp.FormData) -> Option(String) {
 case list.find(form.values, fn(pair) {
    let #(name, _) = pair
    name == "ticker"
  }) {
    Ok(#(_,value)) -> Some(value)
    _ -> None
  }
}
