import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import { toList, prepend as listPrepend, CustomType as $CustomType } from "../../gleam.mjs";

class IntensityTracker extends $CustomType {
  constructor(limit, period, events) {
    super();
    this.limit = limit;
    this.period = period;
    this.events = events;
  }
}

export class TooIntense extends $CustomType {}

export function new$(limit, period) {
  return new IntensityTracker(limit, period, toList([]));
}

export function trim_window(events, now, period) {
  if (events.hasLength(0)) {
    return toList([]);
  } else {
    let event = events.head;
    let events$1 = events.tail;
    let $ = now < (event + period);
    if ($) {
      return listPrepend(event, trim_window(events$1, now, period));
    } else {
      return toList([]);
    }
  }
}
