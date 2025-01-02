import lustre/attribute
import lustre/element/html
import lustre/element.{type Element}
import gleam/option.{type Option}

pub fn layout(
  mjs_path mjs_path: Option(String),
  init_json init_json: Option(String),
  body body: Element(a)
) -> Element(a) {
  html.html([],[
    html.head([],[
      html.title([],"hello layout"),
      html.script([attribute.type_("module"), attribute.src(option.unwrap(mjs_path,""))],""),
      html.script([attribute.type_("application/json")], option.unwrap(init_json, "")),
    ]),
    html.body([], [
      header(),
      body
    ])
  ])
}

fn header() -> Element(a) {
  html.div([
    attribute.style(
      [
        #("diplay", "float"),
      ]
    )
  ],[
    html.a([
      attribute.href("/home"),
      attribute.style([
        #("margin-right", "1em")
      ])
    ], [html.text("Home")]),
    html.a([attribute.href("/manual-trade")], [html.text("Manual")]),
  ])
}
