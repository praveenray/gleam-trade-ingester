import pages/site_css
import pages/manual_trade/manual_trade
import gleam/http
import wisp.{type Request, type Response}
import gleam/option.{Some}
import lustre/element/html.{text,div}
import lustre/element.{type Element}
import layouts/top_across_menu as layout
import lustre/ui/card
import pages/render_utils as utils
import lustre/attribute as a
import pages/load_files/load_files
import pages/query/query

pub fn render(r: Request) -> Response {
  use <- wisp.require_method(r, http.Get)
  let page = layout.layout(
    mjs_path: Some(""),
    init_json: option.None,
    add_lustre_ui: True,
    body: div([
      a.class(site_css.center),
      a.style([
        #("border", "1px solid black"),
        #("min-height","800px")
      ])
    ],[
      div([a.style([
        #("display", "flex"),
        #("justify-content", "space-evenly"),
        #("width", "80%"),
        #("flex-wrap", "wrap"),
       ])
      ],[
        with_card(manual_trade.path_prefix, "Manual Trade"),
        with_card(load_files.path_prefix, "Load Trade Files"),
        with_card(query.path_prefix, "Query Trades"),
      ])
    ])
  )
  utils.send_element(page, 200)
}

fn with_card(href: String, content: String) -> Element(a) {
  card.card([
    card.padding("40px","40px"),
  ],[
    html.a([a.href(href)], [text(content)])
  ])
}
