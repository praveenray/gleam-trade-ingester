import lustre/element.{type Element}
import wisp
import gleam/string_tree

pub fn send_element(element: Element(a), status: Int) -> wisp.Response {
  element
  |> element.to_document_string
  |> string_tree.from_string()
  |> wisp.html_response(status)
}
