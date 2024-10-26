import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import * as $order from "../../gleam_stdlib/gleam/order.mjs";
import * as $birl from "../birl.mjs";
import * as $duration from "../birl/duration.mjs";
import { Ok, Error, toList, CustomType as $CustomType, makeError } from "../gleam.mjs";

class Interval extends $CustomType {
  constructor(start, end) {
    super();
    this.start = start;
    this.end = end;
  }
}

export function from_start_and_end(start, end) {
  let $ = $birl.compare(start, end);
  if ($ instanceof $order.Eq) {
    return new Error(undefined);
  } else if ($ instanceof $order.Lt) {
    return new Ok(new Interval(start, end));
  } else {
    return new Ok(new Interval(end, start));
  }
}

export function from_start_and_duration(start, duration) {
  return from_start_and_end(start, $birl.add(start, duration));
}

export function shift(interval, duration) {
  {
    let start = interval.start;
    let end = interval.end;
    return new Interval($birl.add(start, duration), $birl.add(end, duration));
  }
}

export function scale_up(interval, factor) {
  {
    let start = interval.start;
    let end = interval.end;
    let $ = (() => {
      let _pipe = $birl.difference(end, start);
      let _pipe$1 = $duration.scale_up(_pipe, factor);
      return ((_capture) => { return from_start_and_duration(start, _capture); })(
        _pipe$1,
      );
    })();
    if (!$.isOk()) {
      throw makeError(
        "assignment_no_match",
        "birl/interval",
        34,
        "scale_up",
        "Assignment pattern did not match",
        { value: $ }
      )
    }
    let interval$1 = $[0];
    return interval$1;
  }
}

export function scale_down(interval, factor) {
  {
    let start = interval.start;
    let end = interval.end;
    let $ = (() => {
      let _pipe = $birl.difference(end, start);
      let _pipe$1 = $duration.scale_down(_pipe, factor);
      return ((_capture) => { return from_start_and_duration(start, _capture); })(
        _pipe$1,
      );
    })();
    if (!$.isOk()) {
      throw makeError(
        "assignment_no_match",
        "birl/interval",
        46,
        "scale_down",
        "Assignment pattern did not match",
        { value: $ }
      )
    }
    let interval$1 = $[0];
    return interval$1;
  }
}

export function includes(interval, time) {
  {
    let start = interval.start;
    let end = interval.end;
    return $list.contains(
      toList([new $order.Eq(), new $order.Lt()]),
      $birl.compare(start, time),
    ) && $list.contains(
      toList([new $order.Eq(), new $order.Gt()]),
      $birl.compare(end, time),
    );
  }
}

export function contains(a, b) {
  {
    let start = b.start;
    let end = b.end;
    return includes(a, start) && includes(a, end);
  }
}

export function intersection(a, b) {
  let $ = contains(a, b);
  let $1 = contains(b, a);
  if ($ && !$1) {
    return new $option.Some(b);
  } else if (!$ && $1) {
    return new $option.Some(a);
  } else {
    let a_start = a.start;
    let a_end = a.end;
    let b_start = b.start;
    let b_end = b.end;
    let $2 = includes(a, b_start);
    let $3 = includes(b, a_start);
    if ($2 && !$3) {
      return new $option.Some(new Interval(b_start, a_end));
    } else if (!$2 && $3) {
      return new $option.Some(new Interval(a_start, b_end));
    } else {
      return new $option.None();
    }
  }
}

export function get_bounds(interval) {
  {
    let start = interval.start;
    let end = interval.end;
    return [start, end];
  }
}
