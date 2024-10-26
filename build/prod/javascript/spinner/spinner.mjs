import * as $ansi from "../gleam_community_ansi/gleam_community/ansi.mjs";
import * as $io from "../gleam_stdlib/gleam/io.mjs";
import * as $glearray from "../glearray/glearray.mjs";
import * as $repeatedly from "../repeatedly/repeatedly.mjs";
import { toList, CustomType as $CustomType, makeError, remainderInt } from "./gleam.mjs";

class Spinner extends $CustomType {
  constructor(repeater, frames) {
    super();
    this.repeater = repeater;
    this.frames = frames;
  }
}

class State extends $CustomType {
  constructor(text, colour) {
    super();
    this.text = text;
    this.colour = colour;
  }
}

class Builder extends $CustomType {
  constructor(frames, text, colour) {
    super();
    this.frames = frames;
    this.text = text;
    this.colour = colour;
  }
}

export function with_frames(builder, frames) {
  return builder.withFields({ frames: frames });
}

export function with_colour(builder, colour) {
  return builder.withFields({ colour: colour });
}

export function set_text(spinner, text) {
  return $repeatedly.update_state(
    spinner.repeater,
    (state) => { return state.withFields({ text: text }); },
  );
}

export function set_colour(spinner, colour) {
  return $repeatedly.update_state(
    spinner.repeater,
    (state) => { return state.withFields({ colour: colour }); },
  );
}

function frame(frames, index) {
  let $ = $glearray.get(frames, remainderInt(index, $glearray.length(frames)));
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "spinner",
      107,
      "frame",
      "Assignment pattern did not match",
      { value: $ }
    )
  }
  let frame$1 = $[0];
  return frame$1;
}

const clear_line_code = "\u{001b}[2K";

const go_to_start_code = "\r";

export function stop(spinner) {
  $repeatedly.stop(spinner.repeater);
  let show_cursor = "\u{001b}[?25h";
  return $io.print((clear_line_code + go_to_start_code) + show_cursor);
}

function print(frames, state, index) {
  let hide_cursor = "\u{001b}[?25l";
  return $io.print(
    ((((hide_cursor + clear_line_code) + go_to_start_code) + state.colour(
      frame(frames, index),
    )) + " ") + state.text,
  );
}

export function start(builder) {
  let frames = $glearray.from_list(builder.frames);
  let repeater = $repeatedly.call(
    80,
    new State(builder.text, builder.colour),
    (state, i) => {
      print(frames, state, i);
      return state;
    },
  );
  return new Spinner(repeater, frames);
}

export const clock_frames = /* @__PURE__ */ toList([
  "🕛",
  "🕐",
  "🕑",
  "🕒",
  "🕓",
  "🕔",
  "🕕",
  "🕖",
  "🕗",
  "🕘",
  "🕙",
  "🕚",
]);

export const half_circle_frames = /* @__PURE__ */ toList([
  "◐",
  "◓",
  "◑",
  "◒",
]);

export const moon_frames = /* @__PURE__ */ toList([
  "🌑",
  "🌒",
  "🌓",
  "🌔",
  "🌕",
  "🌖",
  "🌗",
  "🌘",
]);

export const negative_dots_frames = /* @__PURE__ */ toList([
  "⣾",
  "⣽",
  "⣻",
  "⢿",
  "⡿",
  "⣟",
  "⣯",
  "⣷",
]);

export const snake_frames = /* @__PURE__ */ toList([
  "⠋",
  "⠙",
  "⠹",
  "⠸",
  "⠼",
  "⠴",
  "⠦",
  "⠧",
  "⠇",
  "⠏",
]);

export function new$(text) {
  return new Builder(snake_frames, text, $ansi.pink);
}

export const triangle_frames = /* @__PURE__ */ toList([
  "◢",
  "◣",
  "◤",
  "◥",
]);

export const walking_frames = /* @__PURE__ */ toList([
  "⢄",
  "⢂",
  "⢁",
  "⡁",
  "⡈",
  "⡐",
  "⡠",
]);
