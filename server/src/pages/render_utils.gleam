import pages/site_css
import lustre/element.{type Element}
import lustre/attribute as a
import lustre/element/html.{span, div, text}
import wisp
import gleam/string_tree
import gleam/option.{type Option, Some, None}


pub fn send_element(element: Element(a), status: Int) -> wisp.Response {
  element
  |> element.to_document_string
  |> string_tree.from_string()
  |> wisp.html_response(status)
}

pub fn show_error(error: Option(String)) -> Element(a) {
  case error {
    Some(msg) -> span([a.class(site_css.error_msg)],[text(msg)])
    None -> element.none()
  }
}
