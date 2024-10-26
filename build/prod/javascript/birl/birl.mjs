import * as $bool from "../gleam_stdlib/gleam/bool.mjs";
import * as $function from "../gleam_stdlib/gleam/function.mjs";
import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $iterator from "../gleam_stdlib/gleam/iterator.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import * as $order from "../gleam_stdlib/gleam/order.mjs";
import * as $regex from "../gleam_stdlib/gleam/regex.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $ranger from "../ranger/ranger.mjs";
import * as $duration from "./birl/duration.mjs";
import * as $zones from "./birl/zones.mjs";
import {
  now as ffi_now,
  local_offset as ffi_local_offset,
  monotonic_now as ffi_monotonic_now,
  to_parts as ffi_to_parts,
  from_parts as ffi_from_parts,
  weekday as ffi_weekday,
  local_timezone,
} from "./birl_ffi.mjs";
import {
  Ok,
  Error,
  toList,
  CustomType as $CustomType,
  makeError,
  remainderInt,
  divideInt,
} from "./gleam.mjs";

class Time extends $CustomType {
  constructor(wall_time, offset, timezone, monotonic_time) {
    super();
    this.wall_time = wall_time;
    this.offset = offset;
    this.timezone = timezone;
    this.monotonic_time = monotonic_time;
  }
}

export class Day extends $CustomType {
  constructor(year, month, date) {
    super();
    this.year = year;
    this.month = month;
    this.date = date;
  }
}

export class TimeOfDay extends $CustomType {
  constructor(hour, minute, second, milli_second) {
    super();
    this.hour = hour;
    this.minute = minute;
    this.second = second;
    this.milli_second = milli_second;
  }
}

export class Mon extends $CustomType {}

export class Tue extends $CustomType {}

export class Wed extends $CustomType {}

export class Thu extends $CustomType {}

export class Fri extends $CustomType {}

export class Sat extends $CustomType {}

export class Sun extends $CustomType {}

export class Jan extends $CustomType {}

export class Feb extends $CustomType {}

export class Mar extends $CustomType {}

export class Apr extends $CustomType {}

export class May extends $CustomType {}

export class Jun extends $CustomType {}

export class Jul extends $CustomType {}

export class Aug extends $CustomType {}

export class Sep extends $CustomType {}

export class Oct extends $CustomType {}

export class Nov extends $CustomType {}

export class Dec extends $CustomType {}

export function time_of_day_to_string(value) {
  return ((((($int.to_string(value.hour) + ":") + (() => {
    let _pipe = $int.to_string(value.minute);
    return $string.pad_left(_pipe, 2, "0");
  })()) + ":") + (() => {
    let _pipe = $int.to_string(value.second);
    return $string.pad_left(_pipe, 2, "0");
  })()) + ".") + (() => {
    let _pipe = $int.to_string(value.milli_second);
    return $string.pad_left(_pipe, 3, "0");
  })();
}

export function time_of_day_to_short_string(value) {
  return ($int.to_string(value.hour) + ":") + (() => {
    let _pipe = $int.to_string(value.minute);
    return $string.pad_left(_pipe, 2, "0");
  })();
}

export function to_unix(value) {
  {
    let t = value.wall_time;
    return divideInt(t, 1_000_000);
  }
}

export function from_unix(value) {
  return new Time(value * 1_000_000, 0, new $option.None(), new $option.None());
}

export function to_unix_milli(value) {
  {
    let t = value.wall_time;
    return divideInt(t, 1000);
  }
}

export function from_unix_milli(value) {
  return new Time(value * 1000, 0, new $option.None(), new $option.None());
}

export function to_unix_micro(value) {
  {
    let t = value.wall_time;
    return t;
  }
}

export function from_unix_micro(value) {
  return new Time(value, 0, new $option.None(), new $option.None());
}

export function compare(a, b) {
  let wta = a.wall_time;
  let mta = a.monotonic_time;
  let wtb = b.wall_time;
  let mtb = b.monotonic_time;
  let $ = (() => {
    if (mta instanceof $option.Some && mtb instanceof $option.Some) {
      let ta = mta[0];
      let tb = mtb[0];
      return [ta, tb];
    } else {
      return [wta, wtb];
    }
  })();
  let ta = $[0];
  let tb = $[1];
  let $1 = ta === tb;
  let $2 = ta < tb;
  if ($1) {
    return new $order.Eq();
  } else if ($2) {
    return new $order.Lt();
  } else {
    return new $order.Gt();
  }
}

export function difference(a, b) {
  let wta = a.wall_time;
  let mta = a.monotonic_time;
  let wtb = b.wall_time;
  let mtb = b.monotonic_time;
  let $ = (() => {
    if (mta instanceof $option.Some && mtb instanceof $option.Some) {
      let ta = mta[0];
      let tb = mtb[0];
      return [ta, tb];
    } else {
      return [wta, wtb];
    }
  })();
  let ta = $[0];
  let tb = $[1];
  return new $duration.Duration(ta - tb);
}

export function add(value, duration) {
  let wt = value.wall_time;
  let o = value.offset;
  let timezone = value.timezone;
  let mt = value.monotonic_time;
  let duration$1 = duration[0];
  if (mt instanceof $option.Some) {
    let mt$1 = mt[0];
    return new Time(
      wt + duration$1,
      o,
      timezone,
      new $option.Some(mt$1 + duration$1),
    );
  } else {
    return new Time(wt + duration$1, o, timezone, new $option.None());
  }
}

export function subtract(value, duration) {
  let wt = value.wall_time;
  let o = value.offset;
  let timezone = value.timezone;
  let mt = value.monotonic_time;
  let duration$1 = duration[0];
  if (mt instanceof $option.Some) {
    let mt$1 = mt[0];
    return new Time(
      wt - duration$1,
      o,
      timezone,
      new $option.Some(mt$1 - duration$1),
    );
  } else {
    return new Time(wt - duration$1, o, timezone, new $option.None());
  }
}

export function weekday_to_string(value) {
  if (value instanceof Mon) {
    return "Monday";
  } else if (value instanceof Tue) {
    return "Tuesday";
  } else if (value instanceof Wed) {
    return "Wednesday";
  } else if (value instanceof Thu) {
    return "Thursday";
  } else if (value instanceof Fri) {
    return "Friday";
  } else if (value instanceof Sat) {
    return "Saturday";
  } else {
    return "Sunday";
  }
}

export function weekday_to_short_string(value) {
  if (value instanceof Mon) {
    return "Mon";
  } else if (value instanceof Tue) {
    return "Tue";
  } else if (value instanceof Wed) {
    return "Wed";
  } else if (value instanceof Thu) {
    return "Thu";
  } else if (value instanceof Fri) {
    return "Fri";
  } else if (value instanceof Sat) {
    return "Sat";
  } else {
    return "Sun";
  }
}

export function range(a, b, s) {
  let $ = (() => {
    if (b instanceof $option.Some) {
      let b$1 = b[0];
      return $ranger.create(
        (_) => { return true; },
        (duration) => {
          let value = duration[0];
          return new $duration.Duration(-1 * value);
        },
        add,
        compare,
      )(a, b$1, s);
    } else {
      return $ranger.create_infinite((_) => { return true; }, add, compare)(
        a,
        s,
      );
    }
  })();
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "birl",
      1145,
      "range",
      "Assignment pattern did not match",
      { value: $ }
    )
  }
  let range$1 = $[0];
  return range$1;
}

export function set_timezone(value, new_timezone) {
  let $ = $list.key_find($zones.list, new_timezone);
  if ($.isOk()) {
    let new_offset_number = $[0];
    {
      let t = value.wall_time;
      let mt = value.monotonic_time;
      let _pipe = new Time(
        t,
        new_offset_number * 1_000_000,
        new $option.Some(new_timezone),
        mt,
      );
      return new Ok(_pipe);
    }
  } else {
    return new Error(undefined);
  }
}

export function get_timezone(value) {
  let timezone = value.timezone;
  return timezone;
}

function parse_offset(offset) {
  return $bool.guard(
    $list.contains(toList(["Z", "z"]), offset),
    new Ok(0),
    () => {
      let $ = $regex.from_string("([+-])");
      if (!$.isOk()) {
        throw makeError(
          "assignment_no_match",
          "birl",
          1326,
          "",
          "Assignment pattern did not match",
          { value: $ }
        )
      }
      let re = $[0];
      return $result.then$(
        (() => {
          let $1 = $regex.split(re, offset);
          if ($1.hasLength(3) && $1.head === "" && $1.tail.head === "+") {
            let offset$1 = $1.tail.tail.head;
            return new Ok([1, offset$1]);
          } else if ($1.hasLength(3) && $1.head === "" && $1.tail.head === "-") {
            let offset$1 = $1.tail.tail.head;
            return new Ok([-1, offset$1]);
          } else if ($1.hasLength(1)) {
            return new Ok([1, offset]);
          } else {
            return new Error(undefined);
          }
        })(),
        (_use0) => {
          let sign = _use0[0];
          let offset$1 = _use0[1];
          let $1 = $string.split(offset$1, ":");
          if ($1.hasLength(2)) {
            let hour_str = $1.head;
            let minute_str = $1.tail.head;
            return $result.then$(
              $int.parse(hour_str),
              (hour) => {
                return $result.then$(
                  $int.parse(minute_str),
                  (minute) => {
                    return new Ok(sign * (hour * 60 + minute) * 60 * 1_000_000);
                  },
                );
              },
            );
          } else if ($1.hasLength(1)) {
            let offset$2 = $1.head;
            let $2 = $string.length(offset$2);
            if ($2 === 1) {
              return $result.then$(
                $int.parse(offset$2),
                (hour) => { return new Ok(sign * hour * 3600 * 1_000_000); },
              );
            } else if ($2 === 2) {
              return $result.then$(
                $int.parse(offset$2),
                (number) => {
                  let $3 = number < 14;
                  if ($3) {
                    return new Ok(sign * number * 3600 * 1_000_000);
                  } else {
                    return new Ok(
                      sign * ((divideInt(number, 10)) * 60 + (remainderInt(
                        number,
                        10
                      ))) * 60 * 1_000_000,
                    );
                  }
                },
              );
            } else if ($2 === 3) {
              let $3 = $string.first(offset$2);
              if (!$3.isOk()) {
                throw makeError(
                  "assignment_no_match",
                  "birl",
                  1356,
                  "",
                  "Assignment pattern did not match",
                  { value: $3 }
                )
              }
              let hour_str = $3[0];
              let minute_str = $string.slice(offset$2, 1, 2);
              return $result.then$(
                $int.parse(hour_str),
                (hour) => {
                  return $result.then$(
                    $int.parse(minute_str),
                    (minute) => {
                      return new Ok(
                        sign * (hour * 60 + minute) * 60 * 1_000_000,
                      );
                    },
                  );
                },
              );
            } else if ($2 === 4) {
              let hour_str = $string.slice(offset$2, 0, 2);
              let minute_str = $string.slice(offset$2, 2, 2);
              return $result.then$(
                $int.parse(hour_str),
                (hour) => {
                  return $result.then$(
                    $int.parse(minute_str),
                    (minute) => {
                      return new Ok(
                        sign * (hour * 60 + minute) * 60 * 1_000_000,
                      );
                    },
                  );
                },
              );
            } else {
              return new Error(undefined);
            }
          } else {
            return new Error(undefined);
          }
        },
      );
    },
  );
}

export function set_offset(value, new_offset) {
  return $result.then$(
    parse_offset(new_offset),
    (new_offset_number) => {
      {
        let t = value.wall_time;
        let timezone = value.timezone;
        let mt = value.monotonic_time;
        let _pipe = new Time(t, new_offset_number, timezone, mt);
        return new Ok(_pipe);
      }
    },
  );
}

function generate_offset(offset) {
  return $bool.guard(
    offset === 0,
    new Ok("Z"),
    () => {
      let $ = (() => {
        let _pipe = toList([[offset, new $duration.MicroSecond()]]);
        let _pipe$1 = $duration.new$(_pipe);
        return $duration.decompose(_pipe$1);
      })();
      if ($.hasLength(2) &&
      $.head[1] instanceof $duration.Hour &&
      $.tail.head[1] instanceof $duration.Minute) {
        let hour = $.head[0];
        let minute = $.tail.head[0];
        let _pipe = toList([
          (() => {
            let $1 = hour > 0;
            if ($1) {
              return $string.concat(
                toList([
                  "+",
                  (() => {
                    let _pipe = hour;
                    let _pipe$1 = $int.to_string(_pipe);
                    return $string.pad_left(_pipe$1, 2, "0");
                  })(),
                ]),
              );
            } else {
              return $string.concat(
                toList([
                  "-",
                  (() => {
                    let _pipe = hour;
                    let _pipe$1 = $int.absolute_value(_pipe);
                    let _pipe$2 = $int.to_string(_pipe$1);
                    return $string.pad_left(_pipe$2, 2, "0");
                  })(),
                ]),
              );
            }
          })(),
          (() => {
            let _pipe = minute;
            let _pipe$1 = $int.absolute_value(_pipe);
            let _pipe$2 = $int.to_string(_pipe$1);
            return $string.pad_left(_pipe$2, 2, "0");
          })(),
        ]);
        let _pipe$1 = $string.join(_pipe, ":");
        return new Ok(_pipe$1);
      } else if ($.hasLength(1) && $.head[1] instanceof $duration.Hour) {
        let hour = $.head[0];
        let _pipe = toList([
          (() => {
            let $1 = hour > 0;
            if ($1) {
              return $string.concat(
                toList([
                  "+",
                  (() => {
                    let _pipe = hour;
                    let _pipe$1 = $int.to_string(_pipe);
                    return $string.pad_left(_pipe$1, 2, "0");
                  })(),
                ]),
              );
            } else {
              return $string.concat(
                toList([
                  "-",
                  (() => {
                    let _pipe = hour;
                    let _pipe$1 = $int.absolute_value(_pipe);
                    let _pipe$2 = $int.to_string(_pipe$1);
                    return $string.pad_left(_pipe$2, 2, "0");
                  })(),
                ]),
              );
            }
          })(),
          "00",
        ]);
        let _pipe$1 = $string.join(_pipe, ":");
        return new Ok(_pipe$1);
      } else {
        return new Error(undefined);
      }
    },
  );
}

export function get_offset(value) {
  let offset = value.offset;
  let $ = generate_offset(offset);
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "birl",
      1202,
      "get_offset",
      "Assignment pattern did not match",
      { value: $ }
    )
  }
  let offset$1 = $[0];
  return offset$1;
}

function is_invalid_date(date) {
  let _pipe = date;
  let _pipe$1 = $string.to_utf_codepoints(_pipe);
  let _pipe$2 = $list.map(_pipe$1, $string.utf_codepoint_to_int);
  return $list.any(
    _pipe$2,
    (code) => {
      if (code === 45) {
        return false;
      } else if ((code >= 48) && (code <= 57)) {
        return false;
      } else {
        return true;
      }
    },
  );
}

function is_invalid_time(time) {
  let _pipe = time;
  let _pipe$1 = $string.to_utf_codepoints(_pipe);
  let _pipe$2 = $list.map(_pipe$1, $string.utf_codepoint_to_int);
  return $list.any(
    _pipe$2,
    (code) => {
      if ((code >= 48) && (code <= 58)) {
        return false;
      } else {
        return true;
      }
    },
  );
}

function parse_section(section, pattern_string, default$) {
  let $ = $regex.from_string(pattern_string);
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "birl",
      1521,
      "parse_section",
      "Assignment pattern did not match",
      { value: $ }
    )
  }
  let pattern = $[0];
  let $1 = $regex.scan(pattern, section);
  if ($1.hasLength(1) &&
  $1.head instanceof $regex.Match &&
  $1.head.submatches.hasLength(1) &&
  $1.head.submatches.head instanceof $option.Some) {
    let major = $1.head.submatches.head[0];
    return toList([$int.parse(major), new Ok(default$), new Ok(default$)]);
  } else if ($1.hasLength(1) &&
  $1.head instanceof $regex.Match &&
  $1.head.submatches.hasLength(2) &&
  $1.head.submatches.head instanceof $option.Some &&
  $1.head.submatches.tail.head instanceof $option.Some) {
    let major = $1.head.submatches.head[0];
    let middle = $1.head.submatches.tail.head[0];
    return toList([$int.parse(major), $int.parse(middle), new Ok(default$)]);
  } else if ($1.hasLength(1) &&
  $1.head instanceof $regex.Match &&
  $1.head.submatches.hasLength(3) &&
  $1.head.submatches.head instanceof $option.Some &&
  $1.head.submatches.tail.head instanceof $option.Some &&
  $1.head.submatches.tail.tail.head instanceof $option.Some) {
    let major = $1.head.submatches.head[0];
    let middle = $1.head.submatches.tail.head[0];
    let minor = $1.head.submatches.tail.tail.head[0];
    return toList([$int.parse(major), $int.parse(middle), $int.parse(minor)]);
  } else {
    return toList([new Error(undefined)]);
  }
}

function parse_date_section(date) {
  return $bool.guard(
    is_invalid_date(date),
    new Error(undefined),
    () => {
      let _pipe = (() => {
        let $ = $string.contains(date, "-");
        if ($) {
          let $1 = $regex.from_string(
            "(\\d{4})(?:-(1[0-2]|0?[0-9]))?(?:-(3[0-1]|[1-2][0-9]|0?[0-9]))?",
          );
          if (!$1.isOk()) {
            throw makeError(
              "assignment_no_match",
              "birl",
              1441,
              "",
              "Assignment pattern did not match",
              { value: $1 }
            )
          }
          let dash_pattern = $1[0];
          let $2 = $regex.scan(dash_pattern, date);
          if ($2.hasLength(1) &&
          $2.head instanceof $regex.Match &&
          $2.head.submatches.hasLength(1) &&
          $2.head.submatches.head instanceof $option.Some) {
            let major = $2.head.submatches.head[0];
            return toList([$int.parse(major), new Ok(1), new Ok(1)]);
          } else if ($2.hasLength(1) &&
          $2.head instanceof $regex.Match &&
          $2.head.submatches.hasLength(2) &&
          $2.head.submatches.head instanceof $option.Some &&
          $2.head.submatches.tail.head instanceof $option.Some) {
            let major = $2.head.submatches.head[0];
            let middle = $2.head.submatches.tail.head[0];
            return toList([$int.parse(major), $int.parse(middle), new Ok(1)]);
          } else if ($2.hasLength(1) &&
          $2.head instanceof $regex.Match &&
          $2.head.submatches.hasLength(3) &&
          $2.head.submatches.head instanceof $option.Some &&
          $2.head.submatches.tail.head instanceof $option.Some &&
          $2.head.submatches.tail.tail.head instanceof $option.Some) {
            let major = $2.head.submatches.head[0];
            let middle = $2.head.submatches.tail.head[0];
            let minor = $2.head.submatches.tail.tail.head[0];
            return toList([
              $int.parse(major),
              $int.parse(middle),
              $int.parse(minor),
            ]);
          } else {
            return toList([new Error(undefined)]);
          }
        } else {
          return parse_section(
            date,
            "(\\d{4})(1[0-2]|0?[0-9])?(3[0-1]|[1-2][0-9]|0?[0-9])?",
            1,
          );
        }
      })();
      return $list.try_map(_pipe, $function.identity);
    },
  );
}

function parse_time_section(time) {
  return $bool.guard(
    is_invalid_time(time),
    new Error(undefined),
    () => {
      let _pipe = parse_section(
        time,
        "(2[0-3]|1[0-9]|0?[0-9])([1-5][0-9]|0?[0-9])?([1-5][0-9]|0?[0-9])?",
        0,
      );
      return $list.try_map(_pipe, $function.identity);
    },
  );
}

export function parse_time_of_day(value) {
  let $ = $regex.from_string("(.*)([+|\\-].*)");
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "birl",
      398,
      "parse_time_of_day",
      "Assignment pattern did not match",
      { value: $ }
    )
  }
  let offset_pattern = $[0];
  let time_string = (() => {
    let $1 = $string.starts_with(value, "T");
    let $2 = $string.starts_with(value, "t");
    if ($1) {
      return $string.drop_left(value, 1);
    } else if ($2) {
      return $string.drop_left(value, 1);
    } else {
      return value;
    }
  })();
  return $result.then$(
    (() => {
      let $1 = $string.ends_with(time_string, "Z") || $string.ends_with(
        time_string,
        "z",
      );
      if ($1) {
        return new Ok([$string.drop_right(value, 1), "+00:00"]);
      } else {
        let $2 = $regex.scan(offset_pattern, value);
        if ($2.hasLength(1) &&
        $2.head instanceof $regex.Match &&
        $2.head.submatches.hasLength(2) &&
        $2.head.submatches.head instanceof $option.Some &&
        $2.head.submatches.tail.head instanceof $option.Some) {
          let time_string$1 = $2.head.submatches.head[0];
          let offset_string = $2.head.submatches.tail.head[0];
          return new Ok([time_string$1, offset_string]);
        } else {
          return new Error(undefined);
        }
      }
    })(),
    (_use0) => {
      let time_string$1 = _use0[0];
      let offset_string = _use0[1];
      let time_string$2 = $string.replace(time_string$1, ":", "");
      return $result.then$(
        (() => {
          let $1 = $string.split(time_string$2, ".");
          let $2 = $string.split(time_string$2, ",");
          if ($1.hasLength(1) && $2.hasLength(1)) {
            return new Ok([time_string$2, new Ok(0)]);
          } else if ($1.hasLength(2) && $2.hasLength(1)) {
            let time_string$3 = $1.head;
            let milli_seconds_string = $1.tail.head;
            return new Ok(
              [
                time_string$3,
                (() => {
                  let _pipe = milli_seconds_string;
                  let _pipe$1 = $string.slice(_pipe, 0, 3);
                  let _pipe$2 = $string.pad_right(_pipe$1, 3, "0");
                  return $int.parse(_pipe$2);
                })(),
              ],
            );
          } else if ($1.hasLength(1) && $2.hasLength(2)) {
            let time_string$3 = $2.head;
            let milli_seconds_string = $2.tail.head;
            return new Ok(
              [
                time_string$3,
                (() => {
                  let _pipe = milli_seconds_string;
                  let _pipe$1 = $string.slice(_pipe, 0, 3);
                  let _pipe$2 = $string.pad_right(_pipe$1, 3, "0");
                  return $int.parse(_pipe$2);
                })(),
              ],
            );
          } else {
            return new Error(undefined);
          }
        })(),
        (_use0) => {
          let time_string$3 = _use0[0];
          let milli_seconds_result = _use0[1];
          if (milli_seconds_result.isOk()) {
            let milli_seconds = milli_seconds_result[0];
            return $result.then$(
              parse_time_section(time_string$3),
              (time_of_day) => {
                if (!time_of_day.hasLength(3)) {
                  throw makeError(
                    "assignment_no_match",
                    "birl",
                    447,
                    "",
                    "Assignment pattern did not match",
                    { value: time_of_day }
                  )
                }
                let hour = time_of_day.head;
                let minute = time_of_day.tail.head;
                let second = time_of_day.tail.tail.head;
                return $result.then$(
                  parse_offset(offset_string),
                  (offset) => {
                    return $result.then$(
                      generate_offset(offset),
                      (offset_string) => {
                        return new Ok(
                          [
                            new TimeOfDay(hour, minute, second, milli_seconds),
                            offset_string,
                          ],
                        );
                      },
                    );
                  },
                );
              },
            );
          } else {
            return new Error(undefined);
          }
        },
      );
    },
  );
}

export function parse_naive_time_of_day(value) {
  let time_string = (() => {
    let $ = $string.starts_with(value, "T");
    let $1 = $string.starts_with(value, "t");
    if ($) {
      return $string.drop_left(value, 1);
    } else if ($1) {
      return $string.drop_left(value, 1);
    } else {
      return value;
    }
  })();
  let time_string$1 = $string.replace(time_string, ":", "");
  return $result.then$(
    (() => {
      let $ = $string.split(time_string$1, ".");
      let $1 = $string.split(time_string$1, ",");
      if ($.hasLength(1) && $1.hasLength(1)) {
        return new Ok([time_string$1, new Ok(0)]);
      } else if ($.hasLength(2) && $1.hasLength(1)) {
        let time_string$2 = $.head;
        let milli_seconds_string = $.tail.head;
        return new Ok(
          [
            time_string$2,
            (() => {
              let _pipe = milli_seconds_string;
              let _pipe$1 = $string.slice(_pipe, 0, 3);
              let _pipe$2 = $string.pad_right(_pipe$1, 3, "0");
              return $int.parse(_pipe$2);
            })(),
          ],
        );
      } else if ($.hasLength(1) && $1.hasLength(2)) {
        let time_string$2 = $1.head;
        let milli_seconds_string = $1.tail.head;
        return new Ok(
          [
            time_string$2,
            (() => {
              let _pipe = milli_seconds_string;
              let _pipe$1 = $string.slice(_pipe, 0, 3);
              let _pipe$2 = $string.pad_right(_pipe$1, 3, "0");
              return $int.parse(_pipe$2);
            })(),
          ],
        );
      } else {
        return new Error(undefined);
      }
    })(),
    (_use0) => {
      let time_string$2 = _use0[0];
      let milli_seconds_result = _use0[1];
      if (milli_seconds_result.isOk()) {
        let milli_seconds = milli_seconds_result[0];
        return $result.then$(
          parse_time_section(time_string$2),
          (time_of_day) => {
            if (!time_of_day.hasLength(3)) {
              throw makeError(
                "assignment_no_match",
                "birl",
                496,
                "",
                "Assignment pattern did not match",
                { value: time_of_day }
              )
            }
            let hour = time_of_day.head;
            let minute = time_of_day.tail.head;
            let second = time_of_day.tail.tail.head;
            return new Ok(
              [new TimeOfDay(hour, minute, second, milli_seconds), "Z"],
            );
          },
        );
      } else {
        return new Error(undefined);
      }
    },
  );
}

function weekday_from_int(weekday) {
  if (weekday === 0) {
    return new Ok(new Sun());
  } else if (weekday === 1) {
    return new Ok(new Mon());
  } else if (weekday === 2) {
    return new Ok(new Tue());
  } else if (weekday === 3) {
    return new Ok(new Wed());
  } else if (weekday === 4) {
    return new Ok(new Thu());
  } else if (weekday === 5) {
    return new Ok(new Fri());
  } else if (weekday === 6) {
    return new Ok(new Sat());
  } else {
    return new Error(undefined);
  }
}

function month_from_int(month) {
  if (month === 1) {
    return new Ok(new Jan());
  } else if (month === 2) {
    return new Ok(new Feb());
  } else if (month === 3) {
    return new Ok(new Mar());
  } else if (month === 4) {
    return new Ok(new Apr());
  } else if (month === 5) {
    return new Ok(new May());
  } else if (month === 6) {
    return new Ok(new Jun());
  } else if (month === 7) {
    return new Ok(new Jul());
  } else if (month === 8) {
    return new Ok(new Aug());
  } else if (month === 9) {
    return new Ok(new Sep());
  } else if (month === 10) {
    return new Ok(new Oct());
  } else if (month === 11) {
    return new Ok(new Nov());
  } else if (month === 12) {
    return new Ok(new Dec());
  } else {
    return new Error(undefined);
  }
}

export function utc_now() {
  let now$1 = ffi_now();
  let monotonic_now$1 = ffi_monotonic_now();
  return new Time(
    now$1,
    0,
    new $option.Some("Etc/UTC"),
    new $option.Some(monotonic_now$1),
  );
}

export function now_with_offset(offset) {
  return $result.then$(
    parse_offset(offset),
    (offset) => {
      let now$1 = ffi_now();
      let monotonic_now$1 = ffi_monotonic_now();
      let _pipe = new Time(
        now$1,
        offset,
        new $option.None(),
        new $option.Some(monotonic_now$1),
      );
      return new Ok(_pipe);
    },
  );
}

export function now_with_timezone(timezone) {
  let $ = $list.key_find($zones.list, timezone);
  if ($.isOk()) {
    let offset = $[0];
    let now$1 = ffi_now();
    let monotonic_now$1 = ffi_monotonic_now();
    let _pipe = new Time(
      now$1,
      offset * 1_000_000,
      new $option.Some(timezone),
      new $option.Some(monotonic_now$1),
    );
    return new Ok(_pipe);
  } else {
    return new Error(undefined);
  }
}

export function monotonic_now() {
  return ffi_monotonic_now();
}

function to_parts(value) {
  {
    let t = value.wall_time;
    let o = value.offset;
    let $ = ffi_to_parts(t, o);
    let date = $[0];
    let time = $[1];
    let $1 = generate_offset(o);
    if (!$1.isOk()) {
      throw makeError(
        "assignment_no_match",
        "birl",
        1318,
        "to_parts",
        "Assignment pattern did not match",
        { value: $1 }
      )
    }
    let offset = $1[0];
    return [date, time, offset];
  }
}

export function to_date_string(value) {
  let $ = to_parts(value);
  let year = $[0][0];
  let month$1 = $[0][1];
  let day = $[0][2];
  let offset = $[2];
  return (((($int.to_string(year) + "-") + (() => {
    let _pipe = month$1;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + "-") + (() => {
    let _pipe = day;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + offset;
}

export function to_naive_date_string(value) {
  let $ = to_parts(value);
  let year = $[0][0];
  let month$1 = $[0][1];
  let day = $[0][2];
  return ((($int.to_string(year) + "-") + (() => {
    let _pipe = month$1;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + "-") + (() => {
    let _pipe = day;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })();
}

export function to_time_string(value) {
  let $ = to_parts(value);
  let hour = $[1][0];
  let minute = $[1][1];
  let second = $[1][2];
  let milli_second = $[1][3];
  let offset = $[2];
  return (((((((() => {
    let _pipe = hour;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })() + ":") + (() => {
    let _pipe = minute;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + ":") + (() => {
    let _pipe = second;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + ".") + (() => {
    let _pipe = milli_second;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 3, "0");
  })()) + offset;
}

export function to_naive_time_string(value) {
  let $ = to_parts(value);
  let hour = $[1][0];
  let minute = $[1][1];
  let second = $[1][2];
  let milli_second = $[1][3];
  return ((((((() => {
    let _pipe = hour;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })() + ":") + (() => {
    let _pipe = minute;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + ":") + (() => {
    let _pipe = second;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + ".") + (() => {
    let _pipe = milli_second;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 3, "0");
  })();
}

export function to_iso8601(value) {
  let $ = to_parts(value);
  let year = $[0][0];
  let month$1 = $[0][1];
  let day = $[0][2];
  let hour = $[1][0];
  let minute = $[1][1];
  let second = $[1][2];
  let milli_second = $[1][3];
  let offset = $[2];
  return (((((((((((($int.to_string(year) + "-") + (() => {
    let _pipe = month$1;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + "-") + (() => {
    let _pipe = day;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + "T") + (() => {
    let _pipe = hour;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + ":") + (() => {
    let _pipe = minute;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + ":") + (() => {
    let _pipe = second;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + ".") + (() => {
    let _pipe = milli_second;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 3, "0");
  })()) + offset;
}

export function to_naive(value) {
  let $ = to_parts(value);
  let year = $[0][0];
  let month$1 = $[0][1];
  let day = $[0][2];
  let hour = $[1][0];
  let minute = $[1][1];
  let second = $[1][2];
  let milli_second = $[1][3];
  return ((((((((((($int.to_string(year) + "-") + (() => {
    let _pipe = month$1;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + "-") + (() => {
    let _pipe = day;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + "T") + (() => {
    let _pipe = hour;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + ":") + (() => {
    let _pipe = minute;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + ":") + (() => {
    let _pipe = second;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + ".") + (() => {
    let _pipe = milli_second;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 3, "0");
  })();
}

export function month(value) {
  let $ = to_parts(value);
  let month$1 = $[0][1];
  let $1 = month_from_int(month$1);
  if (!$1.isOk()) {
    throw makeError(
      "assignment_no_match",
      "birl",
      1099,
      "month",
      "Assignment pattern did not match",
      { value: $1 }
    )
  }
  let month$2 = $1[0];
  return month$2;
}

export function string_month(value) {
  let $ = month(value);
  if ($ instanceof Jan) {
    return "January";
  } else if ($ instanceof Feb) {
    return "February";
  } else if ($ instanceof Mar) {
    return "March";
  } else if ($ instanceof Apr) {
    return "April";
  } else if ($ instanceof May) {
    return "May";
  } else if ($ instanceof Jun) {
    return "June";
  } else if ($ instanceof Jul) {
    return "July";
  } else if ($ instanceof Aug) {
    return "August";
  } else if ($ instanceof Sep) {
    return "September";
  } else if ($ instanceof Oct) {
    return "October";
  } else if ($ instanceof Nov) {
    return "November";
  } else {
    return "December";
  }
}

export function short_string_month(value) {
  let $ = month(value);
  if ($ instanceof Jan) {
    return "Jan";
  } else if ($ instanceof Feb) {
    return "Feb";
  } else if ($ instanceof Mar) {
    return "Mar";
  } else if ($ instanceof Apr) {
    return "Apr";
  } else if ($ instanceof May) {
    return "May";
  } else if ($ instanceof Jun) {
    return "Jun";
  } else if ($ instanceof Jul) {
    return "Jul";
  } else if ($ instanceof Aug) {
    return "Aug";
  } else if ($ instanceof Sep) {
    return "Sep";
  } else if ($ instanceof Oct) {
    return "Oct";
  } else if ($ instanceof Nov) {
    return "Nov";
  } else {
    return "Dec";
  }
}

export function get_day(value) {
  let $ = to_parts(value);
  let year = $[0][0];
  let month$1 = $[0][1];
  let day = $[0][2];
  return new Day(year, month$1, day);
}

export function get_time_of_day(value) {
  let $ = to_parts(value);
  let hour = $[1][0];
  let minute = $[1][1];
  let second = $[1][2];
  let milli_second = $[1][3];
  return new TimeOfDay(hour, minute, second, milli_second);
}

function from_parts(date, time, offset) {
  return $result.then$(
    parse_offset(offset),
    (offset_number) => {
      let _pipe = ffi_from_parts([date, time], offset_number);
      let _pipe$1 = new Time(
        _pipe,
        offset_number,
        new $option.None(),
        new $option.None(),
      );
      return new Ok(_pipe$1);
    },
  );
}

export function parse(value) {
  let $ = $regex.from_string("(.*)([+|\\-].*)");
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "birl",
      295,
      "parse",
      "Assignment pattern did not match",
      { value: $ }
    )
  }
  let offset_pattern = $[0];
  let value$1 = $string.trim(value);
  return $result.then$(
    (() => {
      let $1 = $string.split(value$1, "T");
      let $2 = $string.split(value$1, "t");
      let $3 = $string.split(value$1, " ");
      if ($1.hasLength(2)) {
        let day_string = $1.head;
        let time_string = $1.tail.head;
        return new Ok([day_string, time_string]);
      } else if ($2.hasLength(2)) {
        let day_string = $2.head;
        let time_string = $2.tail.head;
        return new Ok([day_string, time_string]);
      } else if ($3.hasLength(2)) {
        let day_string = $3.head;
        let time_string = $3.tail.head;
        return new Ok([day_string, time_string]);
      } else if ($1.hasLength(1) && $2.hasLength(1) && $3.hasLength(1)) {
        return new Ok([value$1, "00"]);
      } else {
        return new Error(undefined);
      }
    })(),
    (_use0) => {
      let day_string = _use0[0];
      let offsetted_time_string = _use0[1];
      let day_string$1 = $string.trim(day_string);
      let offsetted_time_string$1 = $string.trim(offsetted_time_string);
      return $result.then$(
        (() => {
          let $1 = $string.ends_with(offsetted_time_string$1, "Z") || $string.ends_with(
            offsetted_time_string$1,
            "z",
          );
          if ($1) {
            return new Ok(
              [
                day_string$1,
                $string.drop_right(offsetted_time_string$1, 1),
                "+00:00",
              ],
            );
          } else {
            let $2 = $regex.scan(offset_pattern, offsetted_time_string$1);
            if ($2.hasLength(1) &&
            $2.head instanceof $regex.Match &&
            $2.head.submatches.hasLength(2) &&
            $2.head.submatches.head instanceof $option.Some &&
            $2.head.submatches.tail.head instanceof $option.Some) {
              let time_string = $2.head.submatches.head[0];
              let offset_string = $2.head.submatches.tail.head[0];
              return new Ok([day_string$1, time_string, offset_string]);
            } else {
              let $3 = $regex.scan(offset_pattern, day_string$1);
              if ($3.hasLength(1) &&
              $3.head instanceof $regex.Match &&
              $3.head.submatches.hasLength(2) &&
              $3.head.submatches.head instanceof $option.Some &&
              $3.head.submatches.tail.head instanceof $option.Some) {
                let day_string$2 = $3.head.submatches.head[0];
                let offset_string = $3.head.submatches.tail.head[0];
                return new Ok([day_string$2, "00", offset_string]);
              } else {
                return new Error(undefined);
              }
            }
          }
        })(),
        (_use0) => {
          let day_string$2 = _use0[0];
          let time_string = _use0[1];
          let offset_string = _use0[2];
          let time_string$1 = $string.replace(time_string, ":", "");
          return $result.then$(
            (() => {
              let $1 = $string.split(time_string$1, ".");
              let $2 = $string.split(time_string$1, ",");
              if ($1.hasLength(1) && $2.hasLength(1)) {
                return new Ok([time_string$1, new Ok(0)]);
              } else if ($1.hasLength(2) && $2.hasLength(1)) {
                let time_string$2 = $1.head;
                let milli_seconds_string = $1.tail.head;
                return new Ok(
                  [
                    time_string$2,
                    (() => {
                      let _pipe = milli_seconds_string;
                      let _pipe$1 = $string.slice(_pipe, 0, 3);
                      let _pipe$2 = $string.pad_right(_pipe$1, 3, "0");
                      return $int.parse(_pipe$2);
                    })(),
                  ],
                );
              } else if ($1.hasLength(1) && $2.hasLength(2)) {
                let time_string$2 = $2.head;
                let milli_seconds_string = $2.tail.head;
                return new Ok(
                  [
                    time_string$2,
                    (() => {
                      let _pipe = milli_seconds_string;
                      let _pipe$1 = $string.slice(_pipe, 0, 3);
                      let _pipe$2 = $string.pad_right(_pipe$1, 3, "0");
                      return $int.parse(_pipe$2);
                    })(),
                  ],
                );
              } else {
                return new Error(undefined);
              }
            })(),
            (_use0) => {
              let time_string$2 = _use0[0];
              let milli_seconds_result = _use0[1];
              if (milli_seconds_result.isOk()) {
                let milli_seconds = milli_seconds_result[0];
                return $result.then$(
                  parse_date_section(day_string$2),
                  (day) => {
                    if (!day.hasLength(3)) {
                      throw makeError(
                        "assignment_no_match",
                        "birl",
                        363,
                        "",
                        "Assignment pattern did not match",
                        { value: day }
                      )
                    }
                    let year = day.head;
                    let month$1 = day.tail.head;
                    let date = day.tail.tail.head;
                    return $result.then$(
                      parse_time_section(time_string$2),
                      (time_of_day) => {
                        if (!time_of_day.hasLength(3)) {
                          throw makeError(
                            "assignment_no_match",
                            "birl",
                            366,
                            "",
                            "Assignment pattern did not match",
                            { value: time_of_day }
                          )
                        }
                        let hour = time_of_day.head;
                        let minute = time_of_day.tail.head;
                        let second = time_of_day.tail.tail.head;
                        return from_parts(
                          [year, month$1, date],
                          [hour, minute, second, milli_seconds],
                          offset_string,
                        );
                      },
                    );
                  },
                );
              } else {
                return new Error(undefined);
              }
            },
          );
        },
      );
    },
  );
}

export function from_naive(value) {
  let value$1 = $string.trim(value);
  return $result.then$(
    (() => {
      let $ = $string.split(value$1, "T");
      let $1 = $string.split(value$1, "t");
      let $2 = $string.split(value$1, " ");
      if ($.hasLength(2)) {
        let day_string = $.head;
        let time_string = $.tail.head;
        return new Ok([day_string, time_string]);
      } else if ($1.hasLength(2)) {
        let day_string = $1.head;
        let time_string = $1.tail.head;
        return new Ok([day_string, time_string]);
      } else if ($2.hasLength(2)) {
        let day_string = $2.head;
        let time_string = $2.tail.head;
        return new Ok([day_string, time_string]);
      } else if ($.hasLength(1) && $1.hasLength(1) && $2.hasLength(1)) {
        return new Ok([value$1, "00"]);
      } else {
        return new Error(undefined);
      }
    })(),
    (_use0) => {
      let day_string = _use0[0];
      let time_string = _use0[1];
      let day_string$1 = $string.trim(day_string);
      let time_string$1 = $string.trim(time_string);
      let time_string$2 = $string.replace(time_string$1, ":", "");
      return $result.then$(
        (() => {
          let $ = $string.split(time_string$2, ".");
          let $1 = $string.split(time_string$2, ",");
          if ($.hasLength(1) && $1.hasLength(1)) {
            return new Ok([time_string$2, new Ok(0)]);
          } else if ($.hasLength(2) && $1.hasLength(1)) {
            let time_string$3 = $.head;
            let milli_seconds_string = $.tail.head;
            return new Ok(
              [
                time_string$3,
                (() => {
                  let _pipe = milli_seconds_string;
                  let _pipe$1 = $string.slice(_pipe, 0, 3);
                  let _pipe$2 = $string.pad_right(_pipe$1, 3, "0");
                  return $int.parse(_pipe$2);
                })(),
              ],
            );
          } else if ($.hasLength(1) && $1.hasLength(2)) {
            let time_string$3 = $1.head;
            let milli_seconds_string = $1.tail.head;
            return new Ok(
              [
                time_string$3,
                (() => {
                  let _pipe = milli_seconds_string;
                  let _pipe$1 = $string.slice(_pipe, 0, 3);
                  let _pipe$2 = $string.pad_right(_pipe$1, 3, "0");
                  return $int.parse(_pipe$2);
                })(),
              ],
            );
          } else {
            return new Error(undefined);
          }
        })(),
        (_use0) => {
          let time_string$3 = _use0[0];
          let milli_seconds_result = _use0[1];
          if (milli_seconds_result.isOk()) {
            let milli_seconds = milli_seconds_result[0];
            return $result.then$(
              parse_date_section(day_string$1),
              (day) => {
                if (!day.hasLength(3)) {
                  throw makeError(
                    "assignment_no_match",
                    "birl",
                    612,
                    "",
                    "Assignment pattern did not match",
                    { value: day }
                  )
                }
                let year = day.head;
                let month$1 = day.tail.head;
                let date = day.tail.tail.head;
                return $result.then$(
                  parse_time_section(time_string$3),
                  (time_of_day) => {
                    if (!time_of_day.hasLength(3)) {
                      throw makeError(
                        "assignment_no_match",
                        "birl",
                        615,
                        "",
                        "Assignment pattern did not match",
                        { value: time_of_day }
                      )
                    }
                    let hour = time_of_day.head;
                    let minute = time_of_day.tail.head;
                    let second = time_of_day.tail.tail.head;
                    return from_parts(
                      [year, month$1, date],
                      [hour, minute, second, milli_seconds],
                      "Z",
                    );
                  },
                );
              },
            );
          } else {
            return new Error(undefined);
          }
        },
      );
    },
  );
}

export function set_day(value, day) {
  let $ = to_parts(value);
  let time = $[1];
  let offset = $[2];
  let year = day.year;
  let month$1 = day.month;
  let date = day.date;
  let $1 = from_parts([year, month$1, date], time, offset);
  if (!$1.isOk()) {
    throw makeError(
      "assignment_no_match",
      "birl",
      1209,
      "set_day",
      "Assignment pattern did not match",
      { value: $1 }
    )
  }
  let new_value = $1[0];
  return new Time(
    new_value.wall_time,
    new_value.offset,
    value.timezone,
    value.monotonic_time,
  );
}

export function set_time_of_day(value, time) {
  let $ = to_parts(value);
  let date = $[0];
  let offset = $[2];
  let hour = time.hour;
  let minute = time.minute;
  let second = time.second;
  let milli_second = time.milli_second;
  let $1 = from_parts(date, [hour, minute, second, milli_second], offset);
  if (!$1.isOk()) {
    throw makeError(
      "assignment_no_match",
      "birl",
      1227,
      "set_time_of_day",
      "Assignment pattern did not match",
      { value: $1 }
    )
  }
  let new_value = $1[0];
  return new Time(
    new_value.wall_time,
    new_value.offset,
    value.timezone,
    value.monotonic_time,
  );
}

export function weekday(value) {
  {
    let t = value.wall_time;
    let o = value.offset;
    let $ = weekday_from_int(ffi_weekday(t, o));
    if (!$.isOk()) {
      throw makeError(
        "assignment_no_match",
        "birl",
        1033,
        "weekday",
        "Assignment pattern did not match",
        { value: $ }
      )
    }
    let weekday$1 = $[0];
    return weekday$1;
  }
}

export function string_weekday(value) {
  let _pipe = weekday(value);
  return weekday_to_string(_pipe);
}

export function short_string_weekday(value) {
  let _pipe = weekday(value);
  return weekday_to_short_string(_pipe);
}

export function to_http(value) {
  let $ = set_offset(value, "Z");
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "birl",
      630,
      "to_http",
      "Assignment pattern did not match",
      { value: $ }
    )
  }
  let value$1 = $[0];
  let $1 = to_parts(value$1);
  let year = $1[0][0];
  let day = $1[0][2];
  let hour = $1[1][0];
  let minute = $1[1][1];
  let second = $1[1][2];
  let short_weekday = short_string_weekday(value$1);
  let short_month = short_string_month(value$1);
  return ((((((((((((short_weekday + ", ") + (() => {
    let _pipe = day;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + " ") + short_month) + " ") + $int.to_string(year)) + " ") + (() => {
    let _pipe = hour;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + ":") + (() => {
    let _pipe = minute;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + ":") + (() => {
    let _pipe = second;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + " GMT";
}

export function to_http_with_offset(value) {
  let $ = to_parts(value);
  let year = $[0][0];
  let day = $[0][2];
  let hour = $[1][0];
  let minute = $[1][1];
  let second = $[1][2];
  let offset = $[2];
  let short_weekday = short_string_weekday(value);
  let short_month = short_string_month(value);
  let offset$1 = (() => {
    if (offset === "Z") {
      return "GMT";
    } else {
      return offset;
    }
  })();
  return (((((((((((((short_weekday + ", ") + (() => {
    let _pipe = day;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + " ") + short_month) + " ") + $int.to_string(year)) + " ") + (() => {
    let _pipe = hour;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + ":") + (() => {
    let _pipe = minute;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + ":") + (() => {
    let _pipe = second;
    let _pipe$1 = $int.to_string(_pipe);
    return $string.pad_left(_pipe$1, 2, "0");
  })()) + " ") + offset$1;
}

export function now() {
  let now$1 = ffi_now();
  let offset_in_minutes = ffi_local_offset();
  let monotonic_now$1 = ffi_monotonic_now();
  let timezone = local_timezone();
  return new Time(
    now$1,
    offset_in_minutes * 60_000_000,
    (() => {
      let _pipe = $option.map(
        timezone,
        (tz) => {
          let $ = $list.any($zones.list, (item) => { return item[0] === tz; });
          if ($) {
            return new $option.Some(tz);
          } else {
            return new $option.None();
          }
        },
      );
      return $option.flatten(_pipe);
    })(),
    new $option.Some(monotonic_now$1),
  );
}

export const unix_epoch = /* @__PURE__ */ new Time(
  0,
  0,
  /* @__PURE__ */ new $option.None(),
  /* @__PURE__ */ new $option.None(),
);

const string_to_units = /* @__PURE__ */ toList([
  ["year", /* @__PURE__ */ new $duration.Year()],
  ["month", /* @__PURE__ */ new $duration.Month()],
  ["week", /* @__PURE__ */ new $duration.Week()],
  ["day", /* @__PURE__ */ new $duration.Day()],
  ["hour", /* @__PURE__ */ new $duration.Hour()],
  ["minute", /* @__PURE__ */ new $duration.Minute()],
  ["second", /* @__PURE__ */ new $duration.Second()],
]);

export function parse_relative(origin, legible_difference) {
  let $ = $string.split(legible_difference, " ");
  if ($.hasLength(3) && $.head === "in") {
    let amount_string = $.tail.head;
    let unit = $.tail.tail.head;
    let unit$1 = (() => {
      let $1 = $string.ends_with(unit, "s");
      if (!$1) {
        return unit;
      } else {
        return $string.drop_right(unit, 1);
      }
    })();
    return $result.then$(
      $int.parse(amount_string),
      (amount) => {
        return $result.then$(
          $list.key_find(string_to_units, unit$1),
          (unit) => {
            return new Ok(add(origin, $duration.new$(toList([[amount, unit]]))));
          },
        );
      },
    );
  } else if ($.hasLength(3) && $.tail.tail.head === "from now") {
    let amount_string = $.head;
    let unit = $.tail.head;
    let unit$1 = (() => {
      let $1 = $string.ends_with(unit, "s");
      if (!$1) {
        return unit;
      } else {
        return $string.drop_right(unit, 1);
      }
    })();
    return $result.then$(
      $int.parse(amount_string),
      (amount) => {
        return $result.then$(
          $list.key_find(string_to_units, unit$1),
          (unit) => {
            return new Ok(add(origin, $duration.new$(toList([[amount, unit]]))));
          },
        );
      },
    );
  } else if ($.hasLength(3) && $.tail.tail.head === "later") {
    let amount_string = $.head;
    let unit = $.tail.head;
    let unit$1 = (() => {
      let $1 = $string.ends_with(unit, "s");
      if (!$1) {
        return unit;
      } else {
        return $string.drop_right(unit, 1);
      }
    })();
    return $result.then$(
      $int.parse(amount_string),
      (amount) => {
        return $result.then$(
          $list.key_find(string_to_units, unit$1),
          (unit) => {
            return new Ok(add(origin, $duration.new$(toList([[amount, unit]]))));
          },
        );
      },
    );
  } else if ($.hasLength(3) && $.tail.tail.head === "ahead") {
    let amount_string = $.head;
    let unit = $.tail.head;
    let unit$1 = (() => {
      let $1 = $string.ends_with(unit, "s");
      if (!$1) {
        return unit;
      } else {
        return $string.drop_right(unit, 1);
      }
    })();
    return $result.then$(
      $int.parse(amount_string),
      (amount) => {
        return $result.then$(
          $list.key_find(string_to_units, unit$1),
          (unit) => {
            return new Ok(add(origin, $duration.new$(toList([[amount, unit]]))));
          },
        );
      },
    );
  } else if ($.hasLength(3) && $.tail.tail.head === "in the future") {
    let amount_string = $.head;
    let unit = $.tail.head;
    let unit$1 = (() => {
      let $1 = $string.ends_with(unit, "s");
      if (!$1) {
        return unit;
      } else {
        return $string.drop_right(unit, 1);
      }
    })();
    return $result.then$(
      $int.parse(amount_string),
      (amount) => {
        return $result.then$(
          $list.key_find(string_to_units, unit$1),
          (unit) => {
            return new Ok(add(origin, $duration.new$(toList([[amount, unit]]))));
          },
        );
      },
    );
  } else if ($.hasLength(3) && $.tail.tail.head === "hence") {
    let amount_string = $.head;
    let unit = $.tail.head;
    let unit$1 = (() => {
      let $1 = $string.ends_with(unit, "s");
      if (!$1) {
        return unit;
      } else {
        return $string.drop_right(unit, 1);
      }
    })();
    return $result.then$(
      $int.parse(amount_string),
      (amount) => {
        return $result.then$(
          $list.key_find(string_to_units, unit$1),
          (unit) => {
            return new Ok(add(origin, $duration.new$(toList([[amount, unit]]))));
          },
        );
      },
    );
  } else if ($.hasLength(3) && $.tail.tail.head === "ago") {
    let amount_string = $.head;
    let unit = $.tail.head;
    let unit$1 = (() => {
      let $1 = $string.ends_with(unit, "s");
      if (!$1) {
        return unit;
      } else {
        return $string.drop_right(unit, 1);
      }
    })();
    return $result.then$(
      $int.parse(amount_string),
      (amount) => {
        return $result.then$(
          $list.key_find(string_to_units, unit$1),
          (unit) => {
            return new Ok(
              subtract(origin, $duration.new$(toList([[amount, unit]]))),
            );
          },
        );
      },
    );
  } else if ($.hasLength(3) && $.tail.tail.head === "before") {
    let amount_string = $.head;
    let unit = $.tail.head;
    let unit$1 = (() => {
      let $1 = $string.ends_with(unit, "s");
      if (!$1) {
        return unit;
      } else {
        return $string.drop_right(unit, 1);
      }
    })();
    return $result.then$(
      $int.parse(amount_string),
      (amount) => {
        return $result.then$(
          $list.key_find(string_to_units, unit$1),
          (unit) => {
            return new Ok(
              subtract(origin, $duration.new$(toList([[amount, unit]]))),
            );
          },
        );
      },
    );
  } else if ($.hasLength(3) && $.tail.tail.head === "earlier") {
    let amount_string = $.head;
    let unit = $.tail.head;
    let unit$1 = (() => {
      let $1 = $string.ends_with(unit, "s");
      if (!$1) {
        return unit;
      } else {
        return $string.drop_right(unit, 1);
      }
    })();
    return $result.then$(
      $int.parse(amount_string),
      (amount) => {
        return $result.then$(
          $list.key_find(string_to_units, unit$1),
          (unit) => {
            return new Ok(
              subtract(origin, $duration.new$(toList([[amount, unit]]))),
            );
          },
        );
      },
    );
  } else if ($.hasLength(3) && $.tail.tail.head === "since") {
    let amount_string = $.head;
    let unit = $.tail.head;
    let unit$1 = (() => {
      let $1 = $string.ends_with(unit, "s");
      if (!$1) {
        return unit;
      } else {
        return $string.drop_right(unit, 1);
      }
    })();
    return $result.then$(
      $int.parse(amount_string),
      (amount) => {
        return $result.then$(
          $list.key_find(string_to_units, unit$1),
          (unit) => {
            return new Ok(
              subtract(origin, $duration.new$(toList([[amount, unit]]))),
            );
          },
        );
      },
    );
  } else if ($.hasLength(3) && $.tail.tail.head === "in the past") {
    let amount_string = $.head;
    let unit = $.tail.head;
    let unit$1 = (() => {
      let $1 = $string.ends_with(unit, "s");
      if (!$1) {
        return unit;
      } else {
        return $string.drop_right(unit, 1);
      }
    })();
    return $result.then$(
      $int.parse(amount_string),
      (amount) => {
        return $result.then$(
          $list.key_find(string_to_units, unit$1),
          (unit) => {
            return new Ok(
              subtract(origin, $duration.new$(toList([[amount, unit]]))),
            );
          },
        );
      },
    );
  } else {
    return new Error(undefined);
  }
}

const units_to_string = /* @__PURE__ */ toList([
  [/* @__PURE__ */ new $duration.Year(), "year"],
  [/* @__PURE__ */ new $duration.Month(), "month"],
  [/* @__PURE__ */ new $duration.Week(), "week"],
  [/* @__PURE__ */ new $duration.Day(), "day"],
  [/* @__PURE__ */ new $duration.Hour(), "hour"],
  [/* @__PURE__ */ new $duration.Minute(), "minute"],
  [/* @__PURE__ */ new $duration.Second(), "second"],
]);

export function legible_difference(a, b) {
  let $ = (() => {
    let _pipe = difference(a, b);
    return $duration.blur(_pipe);
  })();
  if ($[1] instanceof $duration.MicroSecond) {
    return "just now";
  } else if ($[1] instanceof $duration.MilliSecond) {
    return "just now";
  } else {
    let amount = $[0];
    let unit = $[1];
    let $1 = $list.key_find(units_to_string, unit);
    if (!$1.isOk()) {
      throw makeError(
        "assignment_no_match",
        "birl",
        963,
        "legible_difference",
        "Assignment pattern did not match",
        { value: $1 }
      )
    }
    let unit$1 = $1[0];
    let is_negative = amount < 0;
    let amount$1 = $int.absolute_value(amount);
    let unit$2 = (() => {
      if (amount$1 === 1) {
        return unit$1;
      } else {
        return unit$1 + "s";
      }
    })();
    if (is_negative) {
      return (("in " + $int.to_string(amount$1)) + " ") + unit$2;
    } else {
      return (((() => {
        let _pipe = amount$1;
        let _pipe$1 = $int.absolute_value(_pipe);
        return $int.to_string(_pipe$1);
      })() + " ") + unit$2) + " ago";
    }
  }
}

const weekday_strings = /* @__PURE__ */ toList([
  [/* @__PURE__ */ new Mon(), ["Monday", "Mon"]],
  [/* @__PURE__ */ new Tue(), ["Tuesday", "Tue"]],
  [/* @__PURE__ */ new Wed(), ["Wednesday", "Wed"]],
  [/* @__PURE__ */ new Thu(), ["Thursday", "Thu"]],
  [/* @__PURE__ */ new Fri(), ["Friday", "Fri"]],
  [/* @__PURE__ */ new Sat(), ["Saturday", "Sat"]],
  [/* @__PURE__ */ new Sun(), ["Sunday", "Sun"]],
]);

export function parse_weekday(value) {
  let lowercase = $string.lowercase(value);
  let weekday$1 = $list.find(
    weekday_strings,
    (weekday_string) => {
      let long = weekday_string[1][0];
      let short = weekday_string[1][1];
      return (lowercase === $string.lowercase(short)) || (lowercase === $string.lowercase(
        long,
      ));
    },
  );
  let _pipe = weekday$1;
  return $result.map(_pipe, (weekday) => { return weekday[0]; });
}

const month_strings = /* @__PURE__ */ toList([
  [/* @__PURE__ */ new Jan(), ["January", "Jan"]],
  [/* @__PURE__ */ new Feb(), ["February", "Feb"]],
  [/* @__PURE__ */ new Mar(), ["March", "Mar"]],
  [/* @__PURE__ */ new Apr(), ["April", "Apr"]],
  [/* @__PURE__ */ new May(), ["May", "May"]],
  [/* @__PURE__ */ new Jun(), ["June", "Jun"]],
  [/* @__PURE__ */ new Jul(), ["July", "Jul"]],
  [/* @__PURE__ */ new Aug(), ["August", "Aug"]],
  [/* @__PURE__ */ new Sep(), ["September", "Sep"]],
  [/* @__PURE__ */ new Oct(), ["October", "Oct"]],
  [/* @__PURE__ */ new Nov(), ["November", "Nov"]],
  [/* @__PURE__ */ new Dec(), ["December", "Dec"]],
]);

export function from_http(value) {
  let value$1 = $string.trim(value);
  return $result.then$(
    $string.split_once(value$1, ","),
    (_use0) => {
      let weekday$1 = _use0[0];
      let rest = _use0[1];
      return $bool.guard(
        !$list.any(
          weekday_strings,
          (weekday_item) => {
            let strings = weekday_item[1];
            return (strings[0] === weekday$1) || (strings[1] === weekday$1);
          },
        ),
        new Error(undefined),
        () => {
          let rest$1 = $string.trim(rest);
          let $ = $regex.from_string("\\s+");
          if (!$.isOk()) {
            throw makeError(
              "assignment_no_match",
              "birl",
              737,
              "",
              "Assignment pattern did not match",
              { value: $ }
            )
          }
          let whitespace_pattern = $[0];
          let $1 = $regex.split(whitespace_pattern, rest$1);
          if ($1.hasLength(5)) {
            let day_string = $1.head;
            let month_string = $1.tail.head;
            let year_string = $1.tail.tail.head;
            let time_string = $1.tail.tail.tail.head;
            let offset_string = $1.tail.tail.tail.tail.head;
            let time_string$1 = $string.replace(time_string, ":", "");
            let $2 = $int.parse(day_string);
            let $3 = (() => {
              let _pipe = $list.index_map(
                month_strings,
                (month, index) => {
                  let strings = month[1];
                  return [index, strings[0], strings[1]];
                },
              );
              return $list.find(
                _pipe,
                (month) => {
                  return (month[1] === month_string) || (month[2] === month_string);
                },
              );
            })();
            let $4 = $int.parse(year_string);
            let $5 = parse_time_section(time_string$1);
            if ($2.isOk() &&
            $3.isOk() &&
            $4.isOk() &&
            $5.isOk() &&
            $5[0].hasLength(3)) {
              let day = $2[0];
              let month_index = $3[0][0];
              let year = $4[0];
              let hour = $5[0].head;
              let minute = $5[0].tail.head;
              let second = $5[0].tail.tail.head;
              let $6 = from_parts(
                [year, month_index + 1, day],
                [hour, minute, second, 0],
                (() => {
                  if (offset_string === "GMT") {
                    return "Z";
                  } else {
                    return offset_string;
                  }
                })(),
              );
              if ($6.isOk()) {
                let value$2 = $6[0];
                let correct_weekday = string_weekday(value$2);
                let correct_short_weekday = short_string_weekday(value$2);
                let $7 = $list.contains(
                  toList([correct_weekday, correct_short_weekday]),
                  weekday$1,
                );
                if ($7) {
                  return new Ok(value$2);
                } else {
                  return new Error(undefined);
                }
              } else {
                return new Error(undefined);
              }
            } else {
              return new Error(undefined);
            }
          } else if ($1.hasLength(3)) {
            let day_string = $1.head;
            let time_string = $1.tail.head;
            let offset_string = $1.tail.tail.head;
            let $2 = $string.split(day_string, "-");
            if ($2.hasLength(3)) {
              let day_string$1 = $2.head;
              let month_string = $2.tail.head;
              let year_string = $2.tail.tail.head;
              let time_string$1 = $string.replace(time_string, ":", "");
              let $3 = $int.parse(day_string$1);
              let $4 = (() => {
                let _pipe = $list.index_map(
                  month_strings,
                  (month, index) => {
                    let strings = month[1];
                    return [index, strings[0], strings[1]];
                  },
                );
                return $list.find(
                  _pipe,
                  (month) => {
                    return (month[1] === month_string) || (month[2] === month_string);
                  },
                );
              })();
              let $5 = $int.parse(year_string);
              let $6 = parse_time_section(time_string$1);
              if ($3.isOk() &&
              $4.isOk() &&
              $5.isOk() &&
              $6.isOk() &&
              $6[0].hasLength(3)) {
                let day = $3[0];
                let month_index = $4[0][0];
                let year = $5[0];
                let hour = $6[0].head;
                let minute = $6[0].tail.head;
                let second = $6[0].tail.tail.head;
                let $7 = from_parts(
                  [year, month_index + 1, day],
                  [hour, minute, second, 0],
                  (() => {
                    if (offset_string === "GMT") {
                      return "Z";
                    } else {
                      return offset_string;
                    }
                  })(),
                );
                if ($7.isOk()) {
                  let value$2 = $7[0];
                  let correct_weekday = string_weekday(value$2);
                  let correct_short_weekday = short_string_weekday(value$2);
                  let $8 = $list.contains(
                    toList([correct_weekday, correct_short_weekday]),
                    weekday$1,
                  );
                  if ($8) {
                    return new Ok(value$2);
                  } else {
                    return new Error(undefined);
                  }
                } else {
                  return new Error(undefined);
                }
              } else {
                return new Error(undefined);
              }
            } else {
              return new Error(undefined);
            }
          } else {
            return new Error(undefined);
          }
        },
      );
    },
  );
}

export function parse_month(value) {
  let lowercase = $string.lowercase(value);
  let month$1 = $list.find(
    month_strings,
    (month_string) => {
      let long = month_string[1][0];
      let short = month_string[1][1];
      return (lowercase === $string.lowercase(short)) || (lowercase === $string.lowercase(
        long,
      ));
    },
  );
  let _pipe = month$1;
  return $result.map(_pipe, (month) => { return month[0]; });
}
