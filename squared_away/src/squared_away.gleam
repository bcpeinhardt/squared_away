import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import lustre
import lustre/attribute.{class}
import lustre/effect
import lustre/element
import lustre/element/html
import lustre/event
import renderable_error
import squared_away_lang as lang
import squared_away_lang/error
import squared_away_lang/grid
import squared_away_lang/interpreter/value
import squared_away_lang/typechecker/typ
import squared_away_lang/typechecker/type_error

const initial_grid_width = 5

const initial_grid_height = 5

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}

@external(javascript, "./squared_away_ffi.js", "focus")
fn focus(id: String) -> Nil

/// To start, our model will be a 5x5 grid of Strings
type Model {
  Model(
    grid_width: Int,
    grid_height: Int,
    display_mode: DisplayMode,
    active_cell: Option(grid.GridKey),
    src_grid: grid.Grid(String),
    value_grid: grid.Grid(Result(value.Value, error.CompileError)),
    errors_to_display: List(#(grid.GridKey, error.CompileError)),
  )
}

type DisplayMode {
  DisplayValues
  DisplayFormulas
  DisplayGridCoords
}

fn init(_flags) -> #(Model, effect.Effect(Msg)) {
  let src_grid = grid.new(initial_grid_width, initial_grid_height, "")
  let value_grid =
    grid.new(initial_grid_width, initial_grid_height, Ok(value.Empty))

  let model =
    Model(
      grid_width: initial_grid_width,
      grid_height: initial_grid_height,
      display_mode: DisplayValues,
      active_cell: None,
      src_grid:,
      value_grid:,
      errors_to_display: [],
    )
    |> update_grid

  #(model, effect.none())
}

type Msg {
  Noop
  UserToggledDisplayMode(to: DisplayMode)
  UserSetCellValue(key: grid.GridKey, val: String)
  UserFocusedOnCell(key: grid.GridKey)
  UserFocusedOffCell
  UserHitKeyInCell(key: grid.GridKey, keyboard_key: String)
}

fn update_grid(model: Model) -> Model {
  let scanned = lang.scan_grid(model.src_grid)
  let parsed = lang.parse_grid(scanned)
  let typechecked = lang.typecheck_grid(parsed)
  let value_grid = lang.interpret_grid(typechecked)

  // Loop over the grid to see if there's any errors to display
  let errors_to_display =
    grid.fold(value_grid, [], fn(acc, key, val) {
      case val {
        Error(err) -> [#(key, err), ..acc]
        Ok(_) -> acc
      }
    })

  Model(..model, value_grid:, errors_to_display:)
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    Noop -> #(model, effect.none())
    UserSetCellValue(key, val) -> {
      let model =
        Model(..model, src_grid: grid.insert(model.src_grid, key, val))
      #(update_grid(model), effect.none())
    }
    UserToggledDisplayMode(display_mode) -> {
      #(Model(..model, display_mode:), effect.none())
    }
    UserFocusedOnCell(key) -> {
      #(Model(..model, active_cell: Some(key)), effect.none())
    }
    UserFocusedOffCell -> {
      #(Model(..model, active_cell: None), effect.none())
    }
    UserHitKeyInCell(key, keyboard_key) -> {
      case keyboard_key {
        "Enter" -> {
          let new_active_cell = grid.cell_underneath(model.src_grid, key)
          case new_active_cell {
            Error(_) -> #(Model(..model, active_cell: None), effect.none())
            Ok(new) -> {
              focus(grid.to_string(new))
              #(Model(..model, active_cell: Some(new)), effect.none())
            }
          }
        }
        _ -> #(model, effect.none())
      }
    }
  }
}

fn view(model: Model) -> element.Element(Msg) {
  let error_to_display =
    list.find_map(model.errors_to_display, fn(e) {
      case Some(e.0) == model.active_cell {
        False -> Error(Nil)
        True -> Ok(error_view(e.1 |> error.to_renderable_error))
      }
    })
    |> result.unwrap(or: html.div([], []))

  let rows =
    model.src_grid.cells
    |> list.group(grid.row)
    |> dict.map_values(fn(_, keys) {
      let cells =
        list.map(keys, fn(key) {
          let on_enter = event.on_keydown(UserHitKeyInCell(key, _))
          let on_input = event.on_input(UserSetCellValue(key:, val: _))
          let out_of_focus = event.on_blur(UserFocusedOffCell)
          let on_focus = event.on_focus(UserFocusedOnCell(key))
          let id = attribute.id(grid.to_string(key))
          let value = case model.display_mode {
            DisplayFormulas -> grid.get(model.src_grid, key)
            DisplayGridCoords -> string.inspect(key)
            DisplayValues -> case model.active_cell == Some(key) {
              False -> case grid.get(model.value_grid, key) {
                  Error(e) -> error.error_type_string(e)
                  Ok(v) -> value.value_to_string(v)
                }
              True -> grid.get(model.src_grid, key)
            }
          } |> attribute.value

          let styles = case
            list.find(model.errors_to_display, fn(i) { i.0 == key })
          {
            Error(Nil) -> []
            Ok(_) -> [attribute.style([#("background-color", "red")])]
          }

          let input = html.input([
                on_input,
                on_focus,
                out_of_focus,
                value,
                on_enter,
                id
              ])

          html.td(styles, {
            [
              input
            ]
          })
        })

      html.tr([], cells)
    })
    |> dict.to_list
    |> list.sort(fn(r1, r2) { int.compare(r1.0, r2.0) })
    |> list.map(fn(e) { e.1 })

  let grid =
    html.div([class("table-container")], [
      html.table([attribute.class("tg")], [html.tbody([], rows)]),
    ])

  let formula_mode_toggle =
    html.input([
      attribute.type_("radio"),
      attribute.name("display_mode"),
      attribute.id("formula_mode"),
      event.on_check(fn(_) { UserToggledDisplayMode(DisplayFormulas) }),
    ])

  let formula_mode_toggle_label =
    html.label([attribute.for("formula_mode")], t("Show formulas"))

  let grid_mode_toggle =
    html.input([
      attribute.type_("radio"),
      attribute.name("display_mode"),
      attribute.id("grid_mode"),
      event.on_check(fn(_) { UserToggledDisplayMode(DisplayGridCoords) }),
    ])

  let grid_mode_toggle_label =
    html.label([attribute.for("grid_mode")], t("Show grid coordinates"))

  let value_mode_toggle =
    html.input([
      attribute.type_("radio"),
      attribute.name("display_mode"),
      attribute.id("value_mode"),
      event.on_check(fn(_) { UserToggledDisplayMode(DisplayValues) }),
    ])

  let value_mode_toggle_label =
    html.label([attribute.for("value_mode")], t("Show evaluated values"))

  html.div([], [
    value_mode_toggle,
    value_mode_toggle_label,
    formula_mode_toggle,
    formula_mode_toggle_label,
    grid_mode_toggle,
    grid_mode_toggle_label,
    grid,
    error_to_display,
  ])
}

fn error_view(re: renderable_error.RenderableError) {
  html.div([], [
    html.h4([], t(re.title)),
    html.p([], t(re.info)),
    ..case re.hint {
      None -> []
      Some(hint) -> [html.p([], t(hint))]
    }
  ])
}

fn t(input: String) {
  [html.text(input)]
}
