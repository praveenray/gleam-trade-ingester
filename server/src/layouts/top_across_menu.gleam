import gleam/bool
import lustre/attribute as a
import lustre/element/html
import lustre/element.{type Element}
import gleam/option.{type Option, None}
import lustre/ui/theme

pub fn simple_layout(
  body body: Element(a)
) -> Element(a) {
  layout(None, None, True, body)
}

pub fn without_lustre_ui(
  mjs_path mjs_path: Option(String),
  init_json init_json: Option(String),
  body body: Element(a)
) -> Element(a) {
  layout(mjs_path, init_json, False, body)
}

pub fn layout(
  mjs_path mjs_path: Option(String),
  init_json init_json: Option(String),
  add_lustre_ui add_lustre_ui: Bool,
  body body: Element(a)
) -> Element(a) {    
  html.html([],[
    html.head([],[
      // html.meta([
      //   a.attribute("charset", "UTF-8")
      // ]),
      // html.meta([
      //   a.attribute("name", "viewport"),
      //   a.attribute("content","width=device-width, initial-scale=1.0")
      // ]),
      html.title([],"Trades Management"),
      html.script([a.type_("module"), a.src(option.unwrap(mjs_path,""))],""),
      html.script([a.type_("application/json")], option.unwrap(init_json, "")),

      //html.link([a.href("https://unpkg.com/tailwindcss@^2/dist/tailwind.min.css"), a.rel("stylesheet")]),

      bool.guard(add_lustre_ui, theme.to_style(theme.default()), fn(){element.none()}),
      bool.guard(add_lustre_ui, html.link([a.href("/static/lustre_ui.css"), a.rel("stylesheet")]), fn() {element.none()}),
      html.link([a.href("/static/site.css"), a.rel("stylesheet")]),
    ]),
    html.body([
      a.style([#("margin", "40px 10px 10px 10px")])
    ], [
      body
    ])
  ])
}
