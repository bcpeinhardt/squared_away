import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";

export function cell_to_the_right(input) {
  if (input.startsWith("A")) {
    let rest = input.slice(1);
    return new Some("B" + rest);
  } else if (input.startsWith("B")) {
    let rest = input.slice(1);
    return new Some("C" + rest);
  } else if (input.startsWith("C")) {
    let rest = input.slice(1);
    return new Some("D" + rest);
  } else if (input.startsWith("D")) {
    let rest = input.slice(1);
    return new Some("E" + rest);
  } else if (input.startsWith("E")) {
    let rest = input.slice(1);
    return new Some("F" + rest);
  } else if (input.startsWith("F")) {
    let rest = input.slice(1);
    return new Some("G" + rest);
  } else if (input.startsWith("G")) {
    let rest = input.slice(1);
    return new Some("H" + rest);
  } else if (input.startsWith("H")) {
    let rest = input.slice(1);
    return new Some("I" + rest);
  } else if (input.startsWith("I")) {
    let rest = input.slice(1);
    return new Some("J" + rest);
  } else if (input.startsWith("J")) {
    let rest = input.slice(1);
    return new Some("K" + rest);
  } else if (input.startsWith("K")) {
    let rest = input.slice(1);
    return new Some("L" + rest);
  } else if (input.startsWith("L")) {
    let rest = input.slice(1);
    return new Some("M" + rest);
  } else if (input.startsWith("M")) {
    let rest = input.slice(1);
    return new Some("N" + rest);
  } else if (input.startsWith("N")) {
    let rest = input.slice(1);
    return new Some("O" + rest);
  } else if (input.startsWith("O")) {
    let rest = input.slice(1);
    return new Some("P" + rest);
  } else if (input.startsWith("P")) {
    let rest = input.slice(1);
    return new Some("Q" + rest);
  } else if (input.startsWith("Q")) {
    let rest = input.slice(1);
    return new Some("R" + rest);
  } else if (input.startsWith("R")) {
    let rest = input.slice(1);
    return new Some("S" + rest);
  } else if (input.startsWith("S")) {
    let rest = input.slice(1);
    return new Some("T" + rest);
  } else if (input.startsWith("T")) {
    let rest = input.slice(1);
    return new Some("U" + rest);
  } else if (input.startsWith("U")) {
    let rest = input.slice(1);
    return new Some("V" + rest);
  } else if (input.startsWith("V")) {
    let rest = input.slice(1);
    return new Some("W" + rest);
  } else if (input.startsWith("W")) {
    let rest = input.slice(1);
    return new Some("X" + rest);
  } else if (input.startsWith("X")) {
    let rest = input.slice(1);
    return new Some("Y" + rest);
  } else if (input.startsWith("Y")) {
    let rest = input.slice(1);
    return new Some("Z" + rest);
  } else {
    return new None();
  }
}
