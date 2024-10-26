import * as $dict from "../gleam_stdlib/gleam/dict.mjs";
import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $lustre from "../lustre/lustre.mjs";
import * as $attribute from "../lustre/lustre/attribute.mjs";
import * as $effect from "../lustre/lustre/effect.mjs";
import * as $element from "../lustre/lustre/element.mjs";
import * as $html from "../lustre/lustre/element/html.mjs";
import * as $event from "../lustre/lustre/event.mjs";
import { toList, prepend as listPrepend, CustomType as $CustomType, makeError } from "./gleam.mjs";
import * as $lang from "./squared_away/lang.mjs";
import * as $error from "./squared_away/lang/error.mjs";
import * as $value from "./squared_away/lang/interpreter/value.mjs";

class Model extends $CustomType {
  constructor(active_cell, src_grid, value_grid) {
    super();
    this.active_cell = active_cell;
    this.src_grid = src_grid;
    this.value_grid = value_grid;
  }
}

class UserClickedCell extends $CustomType {
  constructor(key) {
    super();
    this.key = key;
  }
}

class UserSetCellValue extends $CustomType {
  constructor(key, val) {
    super();
    this.key = key;
    this.val = val;
  }
}

function update_grid(model) {
  let scanned = $lang.scan_grid(model.src_grid);
  let parsed = $lang.parse_grid(scanned);
  let typechecked = $lang.typecheck_grid(parsed);
  let value_grid = $lang.interpret_grid(typechecked);
  return model.withFields({ value_grid: value_grid });
}

function init(_) {
  let cols = (() => {
    let _pipe = "ABCDE";
    return $string.to_graphemes(_pipe);
  })();
  let rows = toList([1, 2, 3, 4, 5]);
  let src_grid = $list.fold(
    cols,
    $dict.new$(),
    (grid, c) => {
      let _pipe = $list.fold(
        rows,
        $dict.new$(),
        (partial_grid, r) => {
          let key = c + $int.to_string(r);
          let _pipe = partial_grid;
          return $dict.insert(_pipe, key, "");
        },
      );
      return $dict.merge(_pipe, grid);
    },
  );
  let model = new Model("A1", src_grid, $dict.new$());
  let model$1 = update_grid(model);
  return [model$1, $effect.none()];
}

function update(model, msg) {
  if (msg instanceof UserSetCellValue) {
    let key = msg.key;
    let val = msg.val;
    let model$1 = model.withFields({
      src_grid: $dict.insert(model.src_grid, key, val)
    });
    return [update_grid(model$1), $effect.none()];
  } else {
    let key = msg.key;
    return [model.withFields({ active_cell: key }), $effect.none()];
  }
}

function view(model) {
  let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  let columns = listPrepend(
    $html.th(
      toList([$attribute.class$("sticky-header")]),
      toList([$html.text("")]),
    ),
    $list.map(
      $string.to_graphemes(alphabet),
      (l) => {
        return $html.th(
          toList([$attribute.class$("sticky-header")]),
          toList([$html.text(l)]),
        );
      },
    ),
  );
  let grid = $html.div(
    toList([$attribute.class$("table-container")]),
    toList([
      $html.table(
        toList([]),
        toList([
          $html.thead(toList([]), toList([$html.tr(toList([]), columns)])),
          $html.tbody(
            toList([]),
            (() => {
              let _pipe = $list.repeat(undefined, 100);
              return $list.index_map(
                _pipe,
                (_, i) => {
                  return $html.tr(
                    toList([]),
                    listPrepend(
                      $html.th(
                        toList([$attribute.class$("sticky-column")]),
                        toList([$html.text($int.to_string(i + 1))]),
                      ),
                      $list.map(
                        $string.to_graphemes(alphabet),
                        (l) => {
                          return $html.td(
                            toList([]),
                            toList([
                              $html.input(
                                toList([
                                  $event.on_input(
                                    (_capture) => {
                                      return new UserSetCellValue(
                                        l + $int.to_string(i + 1),
                                        _capture,
                                      );
                                    },
                                  ),
                                  $event.on_click(
                                    new UserClickedCell(
                                      l + $int.to_string(i + 1),
                                    ),
                                  ),
                                ]),
                              ),
                            ]),
                          );
                        },
                      ),
                    ),
                  );
                },
              );
            })(),
          ),
        ]),
      ),
    ]),
  );
  let $ = $dict.get(model.value_grid, model.active_cell);
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "squared_away",
      120,
      "view",
      "Assignment pattern did not match",
      { value: $ }
    )
  }
  let active_cell_value = $[0];
  return $html.div(
    toList([]),
    toList([
      $html.div(toList([]), toList([grid])),
      $html.p(
        toList([]),
        toList([
          $html.text(
            (model.active_cell + ": ") + $string.inspect(active_cell_value),
          ),
        ]),
      ),
    ]),
  );
}

export function main() {
  let app = $lustre.application(init, update, view);
  let $ = $lustre.start(app, "#app", undefined);
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "squared_away",
      17,
      "main",
      "Assignment pattern did not match",
      { value: $ }
    )
  }
  return undefined;
}
