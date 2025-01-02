import pages/manual_trade/manual_trade
import gleam/http
import wisp.{type Request, type Response}
import gleam/option.{Some}
import lustre/element/html.{text,div}
import layouts/top_across_menu as layout
import pages/render_utils as utils
import lustre/attribute as a
import pages/load_files/load_files
import pages/query/query

pub fn render(r: Request) -> Response {
  use <- wisp.require_method(r, http.Get)
  let page = layout.layout(
    mjs_path: Some(""),
    init_json: option.None,
    body: html.div([],[
      html.a([a.href(manual_trade.path_prefix)], [text("Manual Trade")]),
      html.br([]),
      html.a([a.href(load_files.path_prefix)], [text("Load Trade Files")]),
      html.br([]),
      html.a([a.href(query.path_prefix)], [text("Query Trades")]),
    ])
  )
  utils.send_element(page, 200)
}
