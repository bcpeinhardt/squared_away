import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import * as $order from "../../gleam_stdlib/gleam/order.mjs";
import * as $regex from "../../gleam_stdlib/gleam/regex.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
  remainderInt,
  divideInt,
} from "../gleam.mjs";

export class Duration extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class MicroSecond extends $CustomType {}

export class MilliSecond extends $CustomType {}

export class Second extends $CustomType {}

export class Minute extends $CustomType {}

export class Hour extends $CustomType {}

export class Day extends $CustomType {}

export class Week extends $CustomType {}

export class Month extends $CustomType {}

export class Year extends $CustomType {}

export function add(a, b) {
  let a$1 = a[0];
  let b$1 = b[0];
  return new Duration(a$1 + b$1);
}

export function subtract(a, b) {
  let a$1 = a[0];
  let b$1 = b[0];
  return new Duration(a$1 - b$1);
}

export function scale_up(value, factor) {
  let value$1 = value[0];
  return new Duration(value$1 * factor);
}

export function scale_down(value, factor) {
  let value$1 = value[0];
  return new Duration(divideInt(value$1, factor));
}

export function micro_seconds(value) {
  return new Duration(value);
}

export function compare(a, b) {
  let dta = a[0];
  let dtb = b[0];
  let $ = dta === dtb;
  let $1 = dta < dtb;
  if ($) {
    return new $order.Eq();
  } else if ($1) {
    return new $order.Lt();
  } else {
    return new $order.Gt();
  }
}

function extract(duration, unit_value) {
  return [divideInt(duration, unit_value), remainderInt(duration, unit_value)];
}

const milli_second = 1000;

export function milli_seconds(value) {
  return new Duration(value * milli_second);
}

const second = 1_000_000;

export function seconds(value) {
  return new Duration(value * second);
}

const minute = 60_000_000;

export function minutes(value) {
  return new Duration(value * minute);
}

const hour = 3_600_000_000;

export function hours(value) {
  return new Duration(value * hour);
}

const day = 86_400_000_000;

export function days(value) {
  return new Duration(value * day);
}

const week = 604_800_000_000;

export function weeks(value) {
  return new Duration(value * week);
}

const month = 2_592_000_000_000;

export function months(value) {
  return new Duration(value * month);
}

const year = 31_536_000_000_000;

export function years(value) {
  return new Duration(value * year);
}

export function new$(values) {
  let _pipe = values;
  let _pipe$1 = $list.fold(
    _pipe,
    0,
    (total, current) => {
      if (current[1] instanceof MicroSecond) {
        let amount = current[0];
        return total + amount;
      } else if (current[1] instanceof MilliSecond) {
        let amount = current[0];
        return total + amount * milli_second;
      } else if (current[1] instanceof Second) {
        let amount = current[0];
        return total + amount * second;
      } else if (current[1] instanceof Minute) {
        let amount = current[0];
        return total + amount * minute;
      } else if (current[1] instanceof Hour) {
        let amount = current[0];
        return total + amount * hour;
      } else if (current[1] instanceof Day) {
        let amount = current[0];
        return total + amount * day;
      } else if (current[1] instanceof Week) {
        let amount = current[0];
        return total + amount * week;
      } else if (current[1] instanceof Month) {
        let amount = current[0];
        return total + amount * month;
      } else {
        let amount = current[0];
        return total + amount * year;
      }
    },
  );
  return new Duration(_pipe$1);
}

export function decompose(duration) {
  let value = duration[0];
  let absolute_value = $int.absolute_value(value);
  let $ = extract(absolute_value, year);
  let years$1 = $[0];
  let remaining = $[1];
  let $1 = extract(remaining, month);
  let months$1 = $1[0];
  let remaining$1 = $1[1];
  let $2 = extract(remaining$1, week);
  let weeks$1 = $2[0];
  let remaining$2 = $2[1];
  let $3 = extract(remaining$2, day);
  let days$1 = $3[0];
  let remaining$3 = $3[1];
  let $4 = extract(remaining$3, hour);
  let hours$1 = $4[0];
  let remaining$4 = $4[1];
  let $5 = extract(remaining$4, minute);
  let minutes$1 = $5[0];
  let remaining$5 = $5[1];
  let $6 = extract(remaining$5, second);
  let seconds$1 = $6[0];
  let remaining$6 = $6[1];
  let $7 = extract(remaining$6, milli_second);
  let milli_seconds$1 = $7[0];
  let remaining$7 = $7[1];
  let _pipe = toList([
    [years$1, new Year()],
    [months$1, new Month()],
    [weeks$1, new Week()],
    [days$1, new Day()],
    [hours$1, new Hour()],
    [minutes$1, new Minute()],
    [seconds$1, new Second()],
    [milli_seconds$1, new MilliSecond()],
    [remaining$7, new MicroSecond()],
  ]);
  let _pipe$1 = $list.filter(_pipe, (item) => { return item[0] > 0; });
  return $list.map(
    _pipe$1,
    (item) => {
      let $8 = value < 0;
      if ($8) {
        return [-1 * item[0], item[1]];
      } else {
        return item;
      }
    },
  );
}

const accurate_month = 2_629_746_000_000;

const accurate_year = 31_556_952_000_000;

export function accurate_new(values) {
  let _pipe = values;
  let _pipe$1 = $list.fold(
    _pipe,
    0,
    (total, current) => {
      if (current[1] instanceof MicroSecond) {
        let amount = current[0];
        return total + amount;
      } else if (current[1] instanceof MilliSecond) {
        let amount = current[0];
        return total + amount * milli_second;
      } else if (current[1] instanceof Second) {
        let amount = current[0];
        return total + amount * second;
      } else if (current[1] instanceof Minute) {
        let amount = current[0];
        return total + amount * minute;
      } else if (current[1] instanceof Hour) {
        let amount = current[0];
        return total + amount * hour;
      } else if (current[1] instanceof Day) {
        let amount = current[0];
        return total + amount * day;
      } else if (current[1] instanceof Week) {
        let amount = current[0];
        return total + amount * week;
      } else if (current[1] instanceof Month) {
        let amount = current[0];
        return total + amount * accurate_month;
      } else {
        let amount = current[0];
        return total + amount * accurate_year;
      }
    },
  );
  return new Duration(_pipe$1);
}

export function accurate_decompose(duration) {
  let value = duration[0];
  let absolute_value = $int.absolute_value(value);
  let $ = extract(absolute_value, accurate_year);
  let years$1 = $[0];
  let remaining = $[1];
  let $1 = extract(remaining, accurate_month);
  let months$1 = $1[0];
  let remaining$1 = $1[1];
  let $2 = extract(remaining$1, week);
  let weeks$1 = $2[0];
  let remaining$2 = $2[1];
  let $3 = extract(remaining$2, day);
  let days$1 = $3[0];
  let remaining$3 = $3[1];
  let $4 = extract(remaining$3, hour);
  let hours$1 = $4[0];
  let remaining$4 = $4[1];
  let $5 = extract(remaining$4, minute);
  let minutes$1 = $5[0];
  let remaining$5 = $5[1];
  let $6 = extract(remaining$5, second);
  let seconds$1 = $6[0];
  let remaining$6 = $6[1];
  let $7 = extract(remaining$6, milli_second);
  let milli_seconds$1 = $7[0];
  let remaining$7 = $7[1];
  let _pipe = toList([
    [years$1, new Year()],
    [months$1, new Month()],
    [weeks$1, new Week()],
    [days$1, new Day()],
    [hours$1, new Hour()],
    [minutes$1, new Minute()],
    [seconds$1, new Second()],
    [milli_seconds$1, new MilliSecond()],
    [remaining$7, new MicroSecond()],
  ]);
  let _pipe$1 = $list.filter(_pipe, (item) => { return item[0] > 0; });
  return $list.map(
    _pipe$1,
    (item) => {
      let $8 = value < 0;
      if ($8) {
        return [-1 * item[0], item[1]];
      } else {
        return item;
      }
    },
  );
}

const unit_values = /* @__PURE__ */ toList([
  [/* @__PURE__ */ new Year(), year],
  [/* @__PURE__ */ new Month(), month],
  [/* @__PURE__ */ new Week(), week],
  [/* @__PURE__ */ new Day(), day],
  [/* @__PURE__ */ new Hour(), hour],
  [/* @__PURE__ */ new Minute(), minute],
  [/* @__PURE__ */ new Second(), second],
  [/* @__PURE__ */ new MilliSecond(), milli_second],
  [/* @__PURE__ */ new MicroSecond(), 1],
]);

export function blur_to(duration, unit) {
  let $ = $list.key_find(unit_values, unit);
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "birl/duration",
      207,
      "blur_to",
      "Assignment pattern did not match",
      { value: $ }
    )
  }
  let unit_value = $[0];
  let value = duration[0];
  let $1 = extract(value, unit_value);
  let unit_counts = $1[0];
  let remaining = $1[1];
  let $2 = remaining >= (divideInt(unit_value * 2, 3));
  if ($2) {
    return unit_counts + 1;
  } else {
    return unit_counts;
  }
}

function inner_blur(loop$values) {
  while (true) {
    let values = loop$values;
    if (!values.atLeastLength(2)) {
      throw makeError(
        "assignment_no_match",
        "birl/duration",
        254,
        "inner_blur",
        "Assignment pattern did not match",
        { value: values }
      )
    }
    let second$1 = values.head;
    let leading = values.tail.head;
    let $ = $list.key_find(unit_values, leading[1]);
    if (!$.isOk()) {
      throw makeError(
        "assignment_no_match",
        "birl/duration",
        255,
        "inner_blur",
        "Assignment pattern did not match",
        { value: $ }
      )
    }
    let leading_unit = $[0];
    let $1 = $list.key_find(unit_values, second$1[1]);
    if (!$1.isOk()) {
      throw makeError(
        "assignment_no_match",
        "birl/duration",
        256,
        "inner_blur",
        "Assignment pattern did not match",
        { value: $1 }
      )
    }
    let second_unit = $1[0];
    let leading$1 = (() => {
      let $2 = second$1[0] * second_unit < (divideInt(leading_unit * 2, 3));
      if ($2) {
        return leading;
      } else {
        return [leading[0] + 1, leading[1]];
      }
    })();
    let $2 = $list.drop(values, 2);
    if ($2.hasLength(0)) {
      return leading$1;
    } else {
      let chopped = $2;
      loop$values = listPrepend(leading$1, chopped);
    }
  }
}

export function blur(duration) {
  let $ = decompose(duration);
  if ($.hasLength(0)) {
    return [0, new MicroSecond()];
  } else {
    let decomposed = $;
    let _pipe = decomposed;
    let _pipe$1 = $list.reverse(_pipe);
    return inner_blur(_pipe$1);
  }
}

const year_units = /* @__PURE__ */ toList(["y", "year", "years"]);

const month_units = /* @__PURE__ */ toList(["mon", "month", "months"]);

const week_units = /* @__PURE__ */ toList(["w", "week", "weeks"]);

const day_units = /* @__PURE__ */ toList(["d", "day", "days"]);

const hour_units = /* @__PURE__ */ toList(["h", "hour", "hours"]);

const minute_units = /* @__PURE__ */ toList(["m", "min", "minute", "minutes"]);

const second_units = /* @__PURE__ */ toList([
  "s",
  "sec",
  "secs",
  "second",
  "seconds",
]);

const milli_second_units = /* @__PURE__ */ toList([
  "ms",
  "msec",
  "msecs",
  "millisecond",
  "milliseconds",
  "milli-second",
  "milli-seconds",
  "milli_second",
  "milli_seconds",
]);

const units = /* @__PURE__ */ toList([
  [/* @__PURE__ */ new Year(), year_units],
  [/* @__PURE__ */ new Month(), month_units],
  [/* @__PURE__ */ new Week(), week_units],
  [/* @__PURE__ */ new Day(), day_units],
  [/* @__PURE__ */ new Hour(), hour_units],
  [/* @__PURE__ */ new Minute(), minute_units],
  [/* @__PURE__ */ new Second(), second_units],
  [/* @__PURE__ */ new MilliSecond(), milli_second_units],
]);

export function parse(expression) {
  let $ = $regex.from_string("([+|\\-])?\\s*(\\d+)\\s*(\\w+)?");
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "birl/duration",
      319,
      "parse",
      "Assignment pattern did not match",
      { value: $ }
    )
  }
  let re = $[0];
  let $1 = (() => {
    let $2 = $string.starts_with(expression, "accurate:");
    if ($2) {
      let $3 = $string.split(expression, ":");
      if (!$3.hasLength(2)) {
        throw makeError(
          "assignment_no_match",
          "birl/duration",
          325,
          "parse",
          "Assignment pattern did not match",
          { value: $3 }
        )
      }
      let expression$1 = $3.tail.head;
      return [accurate_new, expression$1];
    } else {
      return [new$, expression];
    }
  })();
  let constructor = $1[0];
  let expression$1 = $1[1];
  let $2 = (() => {
    let _pipe = expression$1;
    let _pipe$1 = $string.lowercase(_pipe);
    let _pipe$2 = ((_capture) => { return $regex.scan(re, _capture); })(_pipe$1);
    return $list.try_map(
      _pipe$2,
      (item) => {
        if (item instanceof $regex.Match &&
        item.submatches.hasLength(2) &&
        item.submatches.tail.head instanceof $option.Some) {
          let sign_option = item.submatches.head;
          let amount_string = item.submatches.tail.head[0];
          return $result.then$(
            $int.parse(amount_string),
            (amount) => {
              if (sign_option instanceof $option.Some && sign_option[0] === "-") {
                return new Ok([-1 * amount, new MicroSecond()]);
              } else if (sign_option instanceof $option.None) {
                return new Ok([amount, new MicroSecond()]);
              } else if (sign_option instanceof $option.Some &&
              sign_option[0] === "+") {
                return new Ok([amount, new MicroSecond()]);
              } else {
                return new Error(undefined);
              }
            },
          );
        } else if (item instanceof $regex.Match &&
        item.submatches.hasLength(3) &&
        item.submatches.tail.head instanceof $option.Some &&
        item.submatches.tail.tail.head instanceof $option.Some) {
          let sign_option = item.submatches.head;
          let amount_string = item.submatches.tail.head[0];
          let unit = item.submatches.tail.tail.head[0];
          return $result.then$(
            $int.parse(amount_string),
            (amount) => {
              return $result.then$(
                $list.find(
                  units,
                  (item) => { return $list.contains(item[1], unit); },
                ),
                (_use0) => {
                  let unit$1 = _use0[0];
                  if (sign_option instanceof $option.Some &&
                  sign_option[0] === "-") {
                    return new Ok([-1 * amount, unit$1]);
                  } else if (sign_option instanceof $option.None) {
                    return new Ok([amount, unit$1]);
                  } else if (sign_option instanceof $option.Some &&
                  sign_option[0] === "+") {
                    return new Ok([amount, unit$1]);
                  } else {
                    return new Error(undefined);
                  }
                },
              );
            },
          );
        } else {
          return new Error(undefined);
        }
      },
    );
  })();
  if ($2.isOk()) {
    let values = $2[0];
    let _pipe = values;
    let _pipe$1 = constructor(_pipe);
    return new Ok(_pipe$1);
  } else {
    return new Error(undefined);
  }
}
