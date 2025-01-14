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
import pages/site_css
import trades/data_types as trade_types
import gleam/list
import gleam/option.{type Option, Some, None}
import query/query
import lustre/ui/card
import lustre/ui/input
import lustre/ui/button

pub const path_prefix = "profilt-loss-summary"

pub fn accept(r: Request, trades_table: Reference) -> Response {
  let segments = wisp.path_segments(r)
  let paths = list.drop_while(segments, fn(s) {s != path_prefix}) |> list.drop(1)
  case paths {
    [] -> call_profit_loss_summary(trades_table)
    _ -> wisp.bad_request()
  }
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

  let header = div([
        a.style([
          #("display", "flex"),
          #("justify-content","flex-start"),
          #("width", "100%"),
          #("border-bottom","2px solid green"),
        ])
      ],[
        div([
          a.style([#("flex-grow","0")])
        ],[html.a([a.href("/query")],[text("Back")])]),
        html.em([
          a.class(site_css.horizontally_center),
          a.style([#("flex-grow","1")])
        ],[text("Profit/Loss Summary ")])
    ])

  let page = layout.without_lustre_ui(
    None,
    None,
    div([
      a.class(site_css.vertically_center),
      a.style([
        #("gap", "10px"),
      ])
    ],[
      header,
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
