import * as $gleam from './gleam.mjs';

export function terminal_size() {
  try {
    let rows;
    let cols;

    if (typeof Deno === 'undefined') {
      rows = process?.stdout?.rows;
      cols = process?.stdout?.columns;
    } else {
      const size = Deno.consoleSize();
      rows = size.rows;
      cols = size.columns;
    }

    if (rows === undefined || cols === undefined) return new $gleam.Error();

    return new $gleam.Ok([rows, cols]);
  } catch (e) {
    return new $gleam.Error();
  }
}
