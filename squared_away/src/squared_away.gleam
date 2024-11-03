import gleam/bool
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
import renderable_error
import squared_away_lang as lang
import squared_away_lang/error
import squared_away_lang/grid
import squared_away_lang/interpreter/value
import squared_away_lang/typechecker/typed_expr

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
    holding_shift: Bool,
    grid_width: Int,
    grid_height: Int,
    display_mode: DisplayMode,
    display_coords: Bool,
    active_cell: Option(grid.GridKey),
    src_grid: grid.Grid(String),
    value_grid: grid.Grid(Result(value.Value, error.CompileError)),
    errors_to_display: List(#(grid.GridKey, error.CompileError)),
  )
}

type DisplayMode {
  DisplayValues
  DisplayFormulas
}

fn init(_flags) -> #(Model, effect.Effect(Msg)) {
  let src_grid = grid.new(initial_grid_width, initial_grid_height, "")
  let value_grid =
    grid.new(initial_grid_width, initial_grid_height, Ok(value.Empty))

  let model =
    Model(
      holding_shift: False,
      grid_width: initial_grid_width,
      grid_height: initial_grid_height,
      display_mode: DisplayValues,
      display_coords: False,
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
  UserToggledDisplayCoords(to: Bool)
  UserSetCellValue(key: grid.GridKey, val: String)
  UserFocusedOnCell(key: grid.GridKey)
  UserFocusedOffCell
  UserHitKeyInCell(key: grid.GridKey, keyboard_key: String)
  UserReleasedKeyInCell(keyboard_key: String)
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
    UserToggledDisplayCoords(display_coords) -> {
      #(Model(..model, display_coords:), effect.none())
    }
    UserFocusedOnCell(key) -> {
      #(Model(..model, active_cell: Some(key)), effect.none())
    }
    UserFocusedOffCell -> {
      #(Model(..model, active_cell: None), effect.none())
    }
    UserHitKeyInCell(key, keyboard_key) -> {
      case keyboard_key, model.holding_shift {
        "Shift", False -> #(Model(..model, holding_shift: True), effect.none())
        "ArrowUp", _ ->
          set_active_cell_to(model, grid.cell_above(model.src_grid, key))
        "ArrowLeft", _ ->
          set_active_cell_to(model, grid.cell_to_the_left(model.src_grid, key))
        "Enter", _ | "ArrowDown", False ->
          set_active_cell_to(model, grid.cell_underneath(model.src_grid, key))
        "ArrowRight", False ->
          set_active_cell_to(model, grid.cell_to_the_right(model.src_grid, key))
        "ArrowRight", True -> {
          let maybe_cell_to_right = grid.cell_to_the_right(model.src_grid, key)
          case maybe_cell_to_right {
            Error(Nil) -> #(model, effect.none())
            Ok(cell_to_right) -> {
              // Alright, this might be a slightly complex operation
              // If the current formula is a cross label, we want to 
              // produce the equivalent cross label but updated to the new
              // column value.

              let scanned = lang.scan_grid(model.src_grid)
              let parsed = lang.parse_grid(scanned)
              let typechecked = lang.typecheck_grid(parsed)
              let maybe_expr = grid.get(typechecked, key)

              // if it doesn't typecheck, don't copy it over
              use <- bool.guard(maybe_expr |> result.is_error, #(
                model,
                effect.none(),
              ))
              let assert Ok(expr) = maybe_expr

              let expr_with_labels_updated =
                typed_expr.visit_cross_labels(
                  expr,
                  fn(key, row_label, col_label) {
                    // For this case, we want to get the label directly to the right of the col label
                    let assert Ok(key_for_col) =
                      grid.find(model.src_grid, col_label)
                    let assert Ok(key_for_new_col) =
                      grid.cell_to_the_right(model.src_grid, key_for_col)
                    let new_label = grid.get(model.src_grid, key_for_new_col)
                    let assert Ok(new_key) =
                      grid.cell_to_the_right(model.src_grid, key)
                    typed_expr.CrossLabel(
                      expr.type_,
                      new_key,
                      row_label,
                      new_label,
                    )
                  },
                )
              let formula =
                "=" <> typed_expr.to_string(expr_with_labels_updated)
              let src_grid = grid.insert(model.src_grid, cell_to_right, formula)
              let id = grid.to_string(cell_to_right)
              focus(id)
              let new_model =
                Model(..model, src_grid:, active_cell: Some(cell_to_right))
              #(update_grid(new_model), effect.none())
            }
          }
        }
        "ArrowDown", True -> {
          let maybe_cell_below = grid.cell_underneath(model.src_grid, key)
          case maybe_cell_below {
            Error(Nil) -> #(model, effect.none())
            Ok(cell_below) -> {

              let scanned = lang.scan_grid(model.src_grid)
              let parsed = lang.parse_grid(scanned)
              let typechecked = lang.typecheck_grid(parsed)
              let maybe_expr = grid.get(typechecked, key)

              // if it doesn't typecheck, don't copy it over
              use <- bool.guard(maybe_expr |> result.is_error, #(
                model,
                effect.none(),
              ))
              let assert Ok(expr) = maybe_expr

              let expr_with_labels_updated =
                typed_expr.visit_cross_labels(
                  expr,
                  fn(key, row_label, col_label) {
                    // For this case, we want to get the label directly below the row label
                    let assert Ok(key_for_row) =
                      grid.find(model.src_grid, row_label)
                    let assert Ok(key_for_new_row) =
                      grid.cell_underneath(model.src_grid, key_for_row)
                    let new_label = grid.get(model.src_grid, key_for_new_row)
                    let assert Ok(new_key) =
                      grid.cell_underneath(model.src_grid, key)
                    typed_expr.CrossLabel(
                      expr.type_,
                      new_key,
                      new_label,
                      col_label,
                    )
                  },
                )
              let formula =
                "=" <> typed_expr.to_string(expr_with_labels_updated)

              let src_grid = grid.insert(model.src_grid, cell_below, formula)
              let id = grid.to_string(cell_below)
              focus(id)
              let new_model =
                Model(..model, src_grid:, active_cell: Some(cell_below))
              #(update_grid(new_model), effect.none())
            }
          }
        }
        _, _ -> #(model, effect.none())
      }
    }
    UserReleasedKeyInCell(keyboard_key) -> {
      case keyboard_key {
        "Shift" -> #(Model(..model, holding_shift: False), effect.none())
        _ -> #(model, effect.none())
      }
    }
  }
}

fn set_active_cell_to(model, key: Result(grid.GridKey, Nil)) {
  case key {
    Error(_) -> #(model, effect.none())
    Ok(key) -> {
      let id = grid.to_string(key)
      focus(id)
      #(Model(..model, active_cell: Some(key)), effect.none())
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
          let on_keydown = event.on_keydown(UserHitKeyInCell(key, _))
          let on_keyup = event.on_keyup(UserReleasedKeyInCell)
          let on_input = event.on_input(UserSetCellValue(key:, val: _))
          let out_of_focus = event.on_blur(UserFocusedOffCell)
          let on_focus = event.on_focus(UserFocusedOnCell(key))
          let id = attribute.id(grid.to_string(key))
          let value =
            case model.display_mode, model.active_cell == Some(key) {
              DisplayFormulas, _ | DisplayValues, True ->
                grid.get(model.src_grid, key)
              DisplayValues, _ ->
                case grid.get(model.value_grid, key) {
                  Error(e) -> error.error_type_string(e)
                  Ok(v) -> value.value_to_string(v)
                }
            }
            |> attribute.value

          let cell_is_errored =
            list.any(model.errors_to_display, fn(i) { i.0 == key })
          let error_class = case cell_is_errored {
            False -> attribute.none()
            True -> attribute.class("errorcell")
          }

          let input =
            html.input([
              on_input,
              on_focus,
              out_of_focus,
              value,
              on_keydown,
              on_keyup,
              id,
              attribute.type_("text"),
              error_class,
            ])

          case model.display_coords {
            False -> html.td([], [input])
            True ->
              html.td([], [
                html.label([], t(grid.to_string(key) <> ": ")),
                input,
              ])
          }
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
      attribute.type_("checkbox"),
      attribute.id("grid_mode"),
      event.on_check(UserToggledDisplayCoords),
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
