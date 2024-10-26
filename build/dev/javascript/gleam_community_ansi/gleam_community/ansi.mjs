import * as $gc_colour from "../../gleam_community_colour/gleam_community/colour.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $regex from "../../gleam_stdlib/gleam/regex.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import { toList, CustomType as $CustomType, makeError } from "../gleam.mjs";

class Code extends $CustomType {
  constructor(open, close, regexp) {
    super();
    this.open = open;
    this.close = close;
    this.regexp = regexp;
  }
}

function run(text, code) {
  return (code.open + $string.replace(text, code.regexp, code.open)) + code.close;
}

export function strip(text) {
  let regex_options = new $regex.Options(false, true);
  let $ = $regex.compile("(?:\\[(?:\\d+;?)+m)+", regex_options);
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "gleam_community/ansi",
      2346,
      "strip",
      "Assignment pattern did not match",
      { value: $ }
    )
  }
  let r = $[0];
  let _pipe = r;
  let _pipe$1 = $regex.split(_pipe, text);
  return $string.join(_pipe$1, "");
}

const asci_escape_character = "\u{001b}";

function code(open, close) {
  let close_str = $int.to_string(close);
  let open_strs = $list.map(open, $int.to_string);
  return new Code(
    ((asci_escape_character + "[") + $string.join(open_strs, ";")) + "m",
    ((asci_escape_character + "[") + close_str) + "m",
    ((asci_escape_character + "[") + close_str) + "m",
  );
}

export function reset(text) {
  return run(text, code(toList([0]), 0));
}

export function bold(text) {
  return run(text, code(toList([1]), 22));
}

export function dim(text) {
  return run(text, code(toList([2]), 22));
}

export function italic(text) {
  return run(text, code(toList([3]), 23));
}

export function underline(text) {
  return run(text, code(toList([4]), 24));
}

export function inverse(text) {
  return run(text, code(toList([7]), 27));
}

export function hidden(text) {
  return run(text, code(toList([8]), 28));
}

export function strikethrough(text) {
  return run(text, code(toList([9]), 29));
}

export function black(text) {
  return run(text, code(toList([30]), 39));
}

export function red(text) {
  return run(text, code(toList([31]), 39));
}

export function green(text) {
  return run(text, code(toList([32]), 39));
}

export function yellow(text) {
  return run(text, code(toList([33]), 39));
}

export function blue(text) {
  return run(text, code(toList([34]), 39));
}

export function magenta(text) {
  return run(text, code(toList([35]), 39));
}

export function cyan(text) {
  return run(text, code(toList([36]), 39));
}

export function white(text) {
  return run(text, code(toList([37]), 39));
}

export function bright_black(text) {
  return run(text, code(toList([90]), 39));
}

export function grey(text) {
  return bright_black(text);
}

export function gray(text) {
  return bright_black(text);
}

export function bright_red(text) {
  return run(text, code(toList([91]), 39));
}

export function bright_green(text) {
  return run(text, code(toList([92]), 39));
}

export function bright_yellow(text) {
  return run(text, code(toList([93]), 39));
}

export function bright_blue(text) {
  return run(text, code(toList([94]), 39));
}

export function bright_magenta(text) {
  return run(text, code(toList([95]), 39));
}

export function bright_cyan(text) {
  return run(text, code(toList([96]), 39));
}

export function bright_white(text) {
  return run(text, code(toList([97]), 39));
}

export function pink(text) {
  return run(text, code(toList([38, 5, 219]), 39));
}

export function hex(text, colour) {
  let colour$1 = $int.clamp(colour, 0x0, 0xffffff);
  return run(
    text,
    code(
      toList([
        38,
        2,
        (() => {
          let _pipe = $int.bitwise_shift_right(colour$1, 16);
          return $int.bitwise_and(_pipe, 0xff);
        })(),
        (() => {
          let _pipe = $int.bitwise_shift_right(colour$1, 8);
          return $int.bitwise_and(_pipe, 0xff);
        })(),
        $int.bitwise_and(colour$1, 0xff),
      ]),
      39,
    ),
  );
}

export function colour(text, colour) {
  let hex_colour = $gc_colour.to_rgb_hex(colour);
  return hex(text, hex_colour);
}

export function color(text, color) {
  return colour(text, color);
}

export function bg_black(text) {
  return run(text, code(toList([40]), 49));
}

export function bg_red(text) {
  return run(text, code(toList([41]), 49));
}

export function bg_green(text) {
  return run(text, code(toList([42]), 49));
}

export function bg_yellow(text) {
  return run(text, code(toList([43]), 49));
}

export function bg_blue(text) {
  return run(text, code(toList([44]), 49));
}

export function bg_magenta(text) {
  return run(text, code(toList([45]), 49));
}

export function bg_cyan(text) {
  return run(text, code(toList([46]), 49));
}

export function bg_white(text) {
  return run(text, code(toList([47]), 49));
}

export function bg_bright_black(text) {
  return run(text, code(toList([100]), 49));
}

export function bg_bright_red(text) {
  return run(text, code(toList([101]), 49));
}

export function bg_bright_green(text) {
  return run(text, code(toList([102]), 49));
}

export function bg_bright_yellow(text) {
  return run(text, code(toList([103]), 49));
}

export function bg_bright_blue(text) {
  return run(text, code(toList([104]), 49));
}

export function bg_bright_magenta(text) {
  return run(text, code(toList([105]), 49));
}

export function bg_bright_cyan(text) {
  return run(text, code(toList([106]), 49));
}

export function bg_bright_white(text) {
  return run(text, code(toList([107]), 49));
}

export function bg_pink(text) {
  return run(text, code(toList([48, 5, 219]), 49));
}

export function bg_hex(text, colour) {
  return run(
    text,
    code(
      toList([
        48,
        2,
        (() => {
          let _pipe = $int.bitwise_shift_right(colour, 16);
          return $int.bitwise_and(_pipe, 0xff);
        })(),
        (() => {
          let _pipe = $int.bitwise_shift_right(colour, 8);
          return $int.bitwise_and(_pipe, 0xff);
        })(),
        $int.bitwise_and(colour, 0xff),
      ]),
      49,
    ),
  );
}

export function bg_colour(text, colour) {
  let hex_colour = $gc_colour.to_rgb_hex(colour);
  return bg_hex(text, hex_colour);
}

export function bg_color(text, colour) {
  return bg_colour(text, colour);
}
