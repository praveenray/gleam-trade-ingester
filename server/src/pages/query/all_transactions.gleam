import gleam/result
import gleam/int
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
import pages/site_css
import trades/data_types as trade_types
import gleam/list
import gleam/option.{type Option, Some, None}
import query/query

pub const path_prefix = "all-transactions"
pub const order_key = "key"
pub const order_direction = "direction"
pub const start_index = "start"
pub const page_size = "page_size"

type SortString {
  SortString(direction: String, key: String)
}

type PaginationString {
  PaginationString(start: String, page_size: String)
}

pub fn accept(r: Request, trades_table: Reference, parent_path: String) -> Response {
  use <- wisp.require_method(r, http.Get)
  let segments = wisp.path_segments(r)
  let paths = list.drop_while(segments, fn(s) {s != path_prefix})
  |> list.drop(1)
  case paths {
    []|["search"] -> call_all_transactions(r, trades_table, parent_path)
    _ -> wisp.bad_request()
  }
}

fn call_all_transactions(r: Request, trades_table: Reference, parent_path: String) -> Response {
  let default_page = query.new_pagination()
  let default_sort = query.new_sort()
  let raw_sort = SortString(
    key: query_param(r, order_key, default_sort.key),
    direction: query_param(r, order_direction, string.inspect(default_sort.direction))
  )
  let raw_page = PaginationString(
    start: query_param(r, start_index, int.to_string(default_page.start)),
    page_size: query_param(r, page_size, int.to_string(default_page.page_size))
  )
  let result = {
    use pagination <- result.try(pagination_params(raw_page))
    use sort <- result.try(sort_params(raw_sort))
    let #(results_list, total_count) = query.all_transactions(trades_table, sort, pagination)
    results_list
    |> display_transactions(raw_sort, raw_page,  None, total_count,  parent_path)
    |> Ok()
  }

  case result {
    Ok(resp) -> resp
    Error(e) -> {
      display_transactions([],
        raw_sort,
        raw_page,
        Some(e),
        0, parent_path
      )
    }
  }
}

fn pagination_params(page: PaginationString) -> Result(query.Pagination, String) {
  use start <- result.try(
    case int.parse(page.start) {
      Ok(i) if i > 0 && i < 1000 -> Ok(i)
      Ok(_) -> Error(start_index <> " must be more than 0 and less than 1000")
      Error(_) -> Error(start_index <> " is not a number")
    })
  use pagesize <- result.try(
    case int.parse(page.page_size) {
      Ok(size) if size > 0 && size <= 100 -> Ok(size)
      Ok(_) -> Error(page_size <> " must be within 1 and 100")
      _ -> Error(page_size <> " is not a number")
    }
  )
  Ok(query.Pagination(start: start, page_size: pagesize))
}

fn query_param(r: Request, param_name: String, default: String) -> String {
  case list.find(wisp.get_query(r), fn(p) {
    let #(name, _) = p
    name == param_name
  }) {
    Ok(#(_, value)) -> value
    _ -> default
  }
}

fn sort_params(sort: SortString) -> Result(query.Sort, String) {
  case string.lowercase(sort.direction) {
    "asc" -> Ok(query.Sort(direction: query.Asc, key: sort.key))
    "desc" -> Ok(query.Sort(direction: query.Desc, key: sort.key))
    _ -> Error(order_direction <> " must be asc or desc")
  }
}

fn display_transactions(
  list: List(trade_types.Trade),
  sort: SortString,
  page: PaginationString,
  error: Option(String),
  total_count: Int,
  parent_path: String
) -> Response {
  let td_left_align = a.attribute("align", "left")
  let th = fn(content: String) {
    html.th([td_left_align], [text(content)])
  }
  let td = fn(content: String) {
    html.td([td_left_align], [text(content)])
  }
  let page = layout.without_lustre_ui(None, None,
    div([
      a.class(site_css.vertically_center),
      a.style([
        #("gap", "20px")
      ])
    ],[
      div([a.class(site_css.horizontally_center)], [render_utils.show_error(error)]),
      div([
        a.style([
          #("display", "flex"),
          #("justify-content", "flex-start"),
          #("width", "100%"),
          #("border-bottom","2px solid green"),
        ])
      ],[
        html.a([
          a.href("/" <> parent_path),
          a.style([
            #("flex-grow", "0")
          ])
        ],[text("Back")]),
        html.em([
          a.class(site_css.horizontally_center),
          a.style([#("flex-grow","1")])
        ], [text("All Transactions")]),
      ]),
      render_controls(sort, page, total_count),
      html.table([a.attribute("border","1")],[
        html.tr([], [
          th("Symbol"),
          th("Run Date"),
          th("Action"),
          th("Quantity"),
          th("Price"),
        ]),
        ..list.map(list, fn(trade) {
          html.tr([],[
            td(trade.symbol),
            td(utils.date_to_string(trade.run_date)),
            td(string.inspect(trade.action)),
            td(float.to_string(trade.quantity)),
            td(float.to_string(trade.price)),
          ])
        })
      ])
    ]),
  )
  render_utils.send_element(page, 200)
}

fn compute_page_count(raw_page_size: String, total_count: Int) -> Int {
  case int.parse(raw_page_size) {
    Ok(num) if num > 0 -> result.unwrap(int.divide(total_count, num), 0) + 1
    _ -> 1
  }
}

fn render_controls(sort: SortString, page: PaginationString, total_count: Int) -> Element(a) {
  let divider =  html.label([],[text("|")])
  let number_of_pages = compute_page_count(page.page_size, total_count)
  html.form([
    a.class(site_css.center),
    a.style([
      #("gap","10px")
    ]),

    a.method("get"),
    a.action("/query/" <> path_prefix <> "/search")
  ],[
    html.em([], [text("Sort By:")]),
    html.select([
      a.name(order_key),
    ], list.map(trade_types.trade_keys(), fn(k) {
      html.option([a.value(k), a.selected(k == sort.key)],string.capitalise(k))
    })),
    html.input([ a.type_("radio"), a.name(order_direction), a.value("Asc"), a.id("dir-up"), a.checked(sort.direction == string.inspect(query.Asc))]),
    html.label([a.for("dir-up")],[text("Increasing")]),
    div([a.style([#("width","5px")])],[]),
    html.input([a.type_("radio"), a.name(order_direction), a.value("Desc"), a.id("dir-down"), a.checked(sort.direction == string.inspect(query.Desc))]),
    html.label([a.for("dir-down")],[text("Decreasing")]),
    divider,
    html.em([], [text("Page Size:")]),
    html.input([a.name(page_size), a.value(page.page_size)]),
    html.em([], [text("Page Number:")]),
    html.select([
      a.name(start_index)
    ],
      list.range(1, number_of_pages)
        |> list.map(int.to_string(_))
        |> list.map(fn(v) {
          html.option([a.value(v), a.selected(v == page.start)],v)
        })
    ),
    div([a.class(site_css.horizontally_right)], [html.em([], [text("Of " <> int.to_string(total_count))])]),
    html.input([a.type_("submit"), a.value("Search")])
  ])
}
