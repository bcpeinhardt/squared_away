import renderable_error
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import lustre
import lustre/attribute.{class}
import lustre/effect
import lustre/element
import lustre/element/html
import lustre/event
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

/// To start, our model will be a 5x5 grid of Strings
type Model {
  Model(
    grid_width: Int,
    grid_height: Int,
    formula_mode: Bool,
    active_cell: Option(grid.GridKey),
    src_grid: grid.Grid(String),
    value_grid: grid.Grid(Result(value.Value, error.CompileError)),
    errors_to_display: List(#(grid.GridKey, error.CompileError)),
  )
}

fn init(_flags) -> #(Model, effect.Effect(Msg)) {
  let src_grid = grid.new(initial_grid_width, initial_grid_height, "")
  let value_grid =
    grid.new(initial_grid_width, initial_grid_height, Ok(value.Empty))

  let model =
    Model(
      grid_width: initial_grid_width,
      grid_height: initial_grid_height,
      formula_mode: False,
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
  UserToggledFormulaMode(to: Bool)
  UserSetCellValue(key: grid.GridKey, val: String)
  UserFocusedOnCell(key: grid.GridKey)
  UserFocusedOffCell
  UserResizedGrid(new_width: Int, new_height: Int)
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
    UserToggledFormulaMode(formula_mode) -> #(
      Model(..model, formula_mode:),
      effect.none(),
    )
    UserFocusedOnCell(key) -> {
      #(Model(..model, active_cell: Some(key)), effect.none())
    }
    UserFocusedOffCell -> {
      #(Model(..model, active_cell: None), effect.none())
    }
    UserResizedGrid(new_width, new_height) -> {
      #(
        Model(
          ..model,
          grid_width: new_width,
          grid_height: new_height,
          src_grid: grid.resize(model.src_grid, new_width, new_height, ""),
          value_grid: grid.resize(
            model.value_grid,
            new_width,
            new_height,
            Ok(value.Empty),
          ),
        ),
        effect.none(),
      )
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

  let resize_menu =
    html.div([], [
      html.label([], t("Width")),
      html.input([
        event.on_input(fn(txt) {
          case int.parse(txt) {
            Error(_) -> Noop
            Ok(n) -> UserResizedGrid(n, model.grid_height)
          }
        }),
        attribute.value(model.grid_width |> int.to_string),
      ]),
      html.label([], t("Height")),
      html.input([
        event.on_input(fn(txt) {
          case int.parse(txt) {
            Error(_) -> Noop
            Ok(n) -> UserResizedGrid(model.grid_width, n)
          }
        }),
        attribute.value(model.grid_height |> int.to_string),
      ]),
    ])

  let rows =
    model.src_grid.cells
    |> list.group(grid.row)
    |> dict.map_values(fn(_, keys) {
      let cells =
        list.map(keys, fn(key) {
          let on_input = event.on_input(UserSetCellValue(key:, val: _))
          let out_of_focus = event.on_blur(UserFocusedOffCell)
          let on_focus = event.on_focus(UserFocusedOnCell(key))
          let show_formula =
            model.active_cell == Some(key) || model.formula_mode
          let value =
            case show_formula {
              True -> grid.get(model.src_grid, key)
              False ->
                case grid.get(model.value_grid, key) {
                  Error(e) -> error.error_type_string(e)
                  Ok(v) -> value.value_to_string(v)
                }
            }
            |> attribute.value

          let styles = case
            list.find(model.errors_to_display, fn(i) { i.0 == key })
          {
            Error(Nil) -> []
            Ok(_) -> [attribute.style([#("background-color", "red")])]
          }

          html.td(styles, {
            [html.input([on_input, on_focus, out_of_focus, value])]
          })
        })

      html.tr([], cells)
    })
    |> dict.values

  let grid =
    html.div([class("table-container")], [
      html.table([], [html.tbody([], rows)]),
    ])

  let formula_mode_toggle =
    html.input([
      attribute.type_("checkbox"),
      attribute.id("formula_mode"),
      event.on_check(UserToggledFormulaMode),
    ])

  let formula_mode_toggle_label =
    html.label([attribute.for("formula_mode")], t("toggle formula mode"))

  html.div([], [
    formula_mode_toggle,
    formula_mode_toggle_label,
    resize_menu,
    grid,
    error_to_display,
  ])
}

fn error_view(re: renderable_error.RenderableError) {
  html.div([], [
    html.h4([], t(re.title)),
    html.p([], t(re.info)),
    .. case re.hint {
      None -> []
      Some(hint) -> [html.p([], t(hint))]
    }
  ])
}

fn t(input: String) {
  [html.text(input)]
}
