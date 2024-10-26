import * as $float from "../../../gleam_stdlib/gleam/float.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import { makeError, divideFloat } from "../../gleam.mjs";
import * as $colour from "../../gleam_community/colour.mjs";

function intensity(colour_value) {
  let $ = true;
  if (colour_value <= 0.03928) {
    return divideFloat(colour_value, 12.92);
  } else {
    let $1 = $float.power(divideFloat((colour_value + 0.055), 1.055), 2.4);
    if (!$1.isOk()) {
      throw makeError(
        "assignment_no_match",
        "gleam_community/colour/accessibility",
        62,
        "intensity",
        "Assignment pattern did not match",
        { value: $1 }
      )
    }
    let i = $1[0];
    return i;
  }
}

export function luminance(colour) {
  let $ = $colour.to_rgba(colour);
  let r = $[0];
  let g = $[1];
  let b = $[2];
  let r_intensity = intensity(r);
  let g_intensity = intensity(g);
  let b_intensity = intensity(b);
  return ((0.2126 * r_intensity) + (0.7152 * g_intensity)) + (0.0722 * b_intensity);
}

export function contrast_ratio(colour_a, colour_b) {
  let luminance_a = luminance(colour_a) + 0.05;
  let luminance_b = luminance(colour_b) + 0.05;
  let $ = luminance_a > luminance_b;
  if ($) {
    return divideFloat(luminance_a, luminance_b);
  } else {
    return divideFloat(luminance_b, luminance_a);
  }
}

export function maximum_contrast(base, colours) {
  let _pipe = colours;
  let _pipe$1 = $list.sort(
    _pipe,
    (colour_a, colour_b) => {
      let contrast_a = contrast_ratio(base, colour_a);
      let contrast_b = contrast_ratio(base, colour_b);
      return $float.compare(contrast_b, contrast_a);
    },
  );
  return $list.first(_pipe$1);
}
