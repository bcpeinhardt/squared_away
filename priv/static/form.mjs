// build/dev/javascript/prelude.mjs
var CustomType = class {
  withFields(fields) {
    let properties = Object.keys(this).map(
      (label) => label in fields ? fields[label] : this[label]
    );
    return new this.constructor(...properties);
  }
};
var List = class {
  static fromArray(array3, tail) {
    let t = tail || new Empty();
    for (let i = array3.length - 1; i >= 0; --i) {
      t = new NonEmpty(array3[i], t);
    }
    return t;
  }
  [Symbol.iterator]() {
    return new ListIterator(this);
  }
  toArray() {
    return [...this];
  }
  // @internal
  atLeastLength(desired) {
    for (let _ of this) {
      if (desired <= 0)
        return true;
      desired--;
    }
    return desired <= 0;
  }
  // @internal
  hasLength(desired) {
    for (let _ of this) {
      if (desired <= 0)
        return false;
      desired--;
    }
    return desired === 0;
  }
  countLength() {
    let length2 = 0;
    for (let _ of this)
      length2++;
    return length2;
  }
};
function prepend(element2, tail) {
  return new NonEmpty(element2, tail);
}
function toList(elements, tail) {
  return List.fromArray(elements, tail);
}
var ListIterator = class {
  #current;
  constructor(current) {
    this.#current = current;
  }
  next() {
    if (this.#current instanceof Empty) {
      return { done: true };
    } else {
      let { head, tail } = this.#current;
      this.#current = tail;
      return { value: head, done: false };
    }
  }
};
var Empty = class extends List {
};
var NonEmpty = class extends List {
  constructor(head, tail) {
    super();
    this.head = head;
    this.tail = tail;
  }
};
var BitArray = class _BitArray {
  constructor(buffer) {
    if (!(buffer instanceof Uint8Array)) {
      throw "BitArray can only be constructed from a Uint8Array";
    }
    this.buffer = buffer;
  }
  // @internal
  get length() {
    return this.buffer.length;
  }
  // @internal
  byteAt(index2) {
    return this.buffer[index2];
  }
  // @internal
  floatFromSlice(start4, end, isBigEndian) {
    return byteArrayToFloat(this.buffer, start4, end, isBigEndian);
  }
  // @internal
  intFromSlice(start4, end, isBigEndian, isSigned) {
    return byteArrayToInt(this.buffer, start4, end, isBigEndian, isSigned);
  }
  // @internal
  binaryFromSlice(start4, end) {
    return new _BitArray(this.buffer.slice(start4, end));
  }
  // @internal
  sliceAfter(index2) {
    return new _BitArray(this.buffer.slice(index2));
  }
};
var UtfCodepoint = class {
  constructor(value2) {
    this.value = value2;
  }
};
function byteArrayToInt(byteArray, start4, end, isBigEndian, isSigned) {
  let value2 = 0;
  if (isBigEndian) {
    for (let i = start4; i < end; i++) {
      value2 = value2 * 256 + byteArray[i];
    }
  } else {
    for (let i = end - 1; i >= start4; i--) {
      value2 = value2 * 256 + byteArray[i];
    }
  }
  if (isSigned) {
    const byteSize = end - start4;
    const highBit = 2 ** (byteSize * 8 - 1);
    if (value2 >= highBit) {
      value2 -= highBit * 2;
    }
  }
  return value2;
}
function byteArrayToFloat(byteArray, start4, end, isBigEndian) {
  const view2 = new DataView(byteArray.buffer);
  const byteSize = end - start4;
  if (byteSize === 8) {
    return view2.getFloat64(start4, !isBigEndian);
  } else if (byteSize === 4) {
    return view2.getFloat32(start4, !isBigEndian);
  } else {
    const msg = `Sized floats must be 32-bit or 64-bit on JavaScript, got size of ${byteSize * 8} bits`;
    throw new globalThis.Error(msg);
  }
}
var Result = class _Result extends CustomType {
  // @internal
  static isResult(data) {
    return data instanceof _Result;
  }
};
var Ok = class extends Result {
  constructor(value2) {
    super();
    this[0] = value2;
  }
  // @internal
  isOk() {
    return true;
  }
};
var Error = class extends Result {
  constructor(detail) {
    super();
    this[0] = detail;
  }
  // @internal
  isOk() {
    return false;
  }
};
function isEqual(x, y) {
  let values = [x, y];
  while (values.length) {
    let a = values.pop();
    let b = values.pop();
    if (a === b)
      continue;
    if (!isObject(a) || !isObject(b))
      return false;
    let unequal = !structurallyCompatibleObjects(a, b) || unequalDates(a, b) || unequalBuffers(a, b) || unequalArrays(a, b) || unequalMaps(a, b) || unequalSets(a, b) || unequalRegExps(a, b);
    if (unequal)
      return false;
    const proto = Object.getPrototypeOf(a);
    if (proto !== null && typeof proto.equals === "function") {
      try {
        if (a.equals(b))
          continue;
        else
          return false;
      } catch {
      }
    }
    let [keys2, get2] = getters(a);
    for (let k of keys2(a)) {
      values.push(get2(a, k), get2(b, k));
    }
  }
  return true;
}
function getters(object3) {
  if (object3 instanceof Map) {
    return [(x) => x.keys(), (x, y) => x.get(y)];
  } else {
    let extra = object3 instanceof globalThis.Error ? ["message"] : [];
    return [(x) => [...extra, ...Object.keys(x)], (x, y) => x[y]];
  }
}
function unequalDates(a, b) {
  return a instanceof Date && (a > b || a < b);
}
function unequalBuffers(a, b) {
  return a.buffer instanceof ArrayBuffer && a.BYTES_PER_ELEMENT && !(a.byteLength === b.byteLength && a.every((n, i) => n === b[i]));
}
function unequalArrays(a, b) {
  return Array.isArray(a) && a.length !== b.length;
}
function unequalMaps(a, b) {
  return a instanceof Map && a.size !== b.size;
}
function unequalSets(a, b) {
  return a instanceof Set && (a.size != b.size || [...a].some((e) => !b.has(e)));
}
function unequalRegExps(a, b) {
  return a instanceof RegExp && (a.source !== b.source || a.flags !== b.flags);
}
function isObject(a) {
  return typeof a === "object" && a !== null;
}
function structurallyCompatibleObjects(a, b) {
  if (typeof a !== "object" && typeof b !== "object" && (!a || !b))
    return false;
  let nonstructural = [Promise, WeakSet, WeakMap, Function];
  if (nonstructural.some((c) => a instanceof c))
    return false;
  return a.constructor === b.constructor;
}
function divideInt(a, b) {
  return Math.trunc(divideFloat(a, b));
}
function divideFloat(a, b) {
  if (b === 0) {
    return 0;
  } else {
    return a / b;
  }
}
function makeError(variant, module, line, fn, message, extra) {
  let error = new globalThis.Error(message);
  error.gleam_error = variant;
  error.module = module;
  error.line = line;
  error.fn = fn;
  for (let k in extra)
    error[k] = extra[k];
  return error;
}

// build/dev/javascript/gleam_stdlib/gleam/option.mjs
var Some = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var None = class extends CustomType {
};
function to_result(option, e) {
  if (option instanceof Some) {
    let a = option[0];
    return new Ok(a);
  } else {
    return new Error(e);
  }
}

// build/dev/javascript/gleam_stdlib/gleam/order.mjs
var Lt = class extends CustomType {
};
var Eq = class extends CustomType {
};
var Gt = class extends CustomType {
};

// build/dev/javascript/gleam_stdlib/gleam/float.mjs
function parse(string3) {
  return parse_float(string3);
}
function ceiling2(x) {
  return ceiling(x);
}
function power2(base, exponent) {
  let fractional = ceiling2(exponent) - exponent > 0;
  let $ = base < 0 && fractional || base === 0 && exponent < 0;
  if ($) {
    return new Error(void 0);
  } else {
    return new Ok(power(base, exponent));
  }
}

// build/dev/javascript/gleam_stdlib/gleam/int.mjs
function parse2(string3) {
  return parse_int(string3);
}
function to_string2(x) {
  return to_string(x);
}
function to_float(x) {
  return identity(x);
}
function power3(base, exponent) {
  let _pipe = base;
  let _pipe$1 = to_float(_pipe);
  return power2(_pipe$1, exponent);
}

// build/dev/javascript/gleam_stdlib/gleam/list.mjs
var Ascending = class extends CustomType {
};
var Descending = class extends CustomType {
};
function do_reverse(loop$remaining, loop$accumulator) {
  while (true) {
    let remaining = loop$remaining;
    let accumulator = loop$accumulator;
    if (remaining.hasLength(0)) {
      return accumulator;
    } else {
      let item = remaining.head;
      let rest$1 = remaining.tail;
      loop$remaining = rest$1;
      loop$accumulator = prepend(item, accumulator);
    }
  }
}
function reverse(xs) {
  return do_reverse(xs, toList([]));
}
function do_map(loop$list, loop$fun, loop$acc) {
  while (true) {
    let list = loop$list;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse(acc);
    } else {
      let x = list.head;
      let xs = list.tail;
      loop$list = xs;
      loop$fun = fun;
      loop$acc = prepend(fun(x), acc);
    }
  }
}
function map(list, fun) {
  return do_map(list, fun, toList([]));
}
function fold(loop$list, loop$initial, loop$fun) {
  while (true) {
    let list = loop$list;
    let initial = loop$initial;
    let fun = loop$fun;
    if (list.hasLength(0)) {
      return initial;
    } else {
      let x = list.head;
      let rest$1 = list.tail;
      loop$list = rest$1;
      loop$initial = fun(initial, x);
      loop$fun = fun;
    }
  }
}
function sequences(loop$list, loop$compare, loop$growing, loop$direction, loop$prev, loop$acc) {
  while (true) {
    let list = loop$list;
    let compare4 = loop$compare;
    let growing = loop$growing;
    let direction = loop$direction;
    let prev = loop$prev;
    let acc = loop$acc;
    let growing$1 = prepend(prev, growing);
    if (list.hasLength(0)) {
      if (direction instanceof Ascending) {
        return prepend(do_reverse(growing$1, toList([])), acc);
      } else {
        return prepend(growing$1, acc);
      }
    } else {
      let new$1 = list.head;
      let rest$1 = list.tail;
      let $ = compare4(prev, new$1);
      if ($ instanceof Gt && direction instanceof Descending) {
        loop$list = rest$1;
        loop$compare = compare4;
        loop$growing = growing$1;
        loop$direction = direction;
        loop$prev = new$1;
        loop$acc = acc;
      } else if ($ instanceof Lt && direction instanceof Ascending) {
        loop$list = rest$1;
        loop$compare = compare4;
        loop$growing = growing$1;
        loop$direction = direction;
        loop$prev = new$1;
        loop$acc = acc;
      } else if ($ instanceof Eq && direction instanceof Ascending) {
        loop$list = rest$1;
        loop$compare = compare4;
        loop$growing = growing$1;
        loop$direction = direction;
        loop$prev = new$1;
        loop$acc = acc;
      } else if ($ instanceof Gt && direction instanceof Ascending) {
        let acc$1 = (() => {
          if (direction instanceof Ascending) {
            return prepend(do_reverse(growing$1, toList([])), acc);
          } else {
            return prepend(growing$1, acc);
          }
        })();
        if (rest$1.hasLength(0)) {
          return prepend(toList([new$1]), acc$1);
        } else {
          let next = rest$1.head;
          let rest$2 = rest$1.tail;
          let direction$1 = (() => {
            let $1 = compare4(new$1, next);
            if ($1 instanceof Lt) {
              return new Ascending();
            } else if ($1 instanceof Eq) {
              return new Ascending();
            } else {
              return new Descending();
            }
          })();
          loop$list = rest$2;
          loop$compare = compare4;
          loop$growing = toList([new$1]);
          loop$direction = direction$1;
          loop$prev = next;
          loop$acc = acc$1;
        }
      } else if ($ instanceof Lt && direction instanceof Descending) {
        let acc$1 = (() => {
          if (direction instanceof Ascending) {
            return prepend(do_reverse(growing$1, toList([])), acc);
          } else {
            return prepend(growing$1, acc);
          }
        })();
        if (rest$1.hasLength(0)) {
          return prepend(toList([new$1]), acc$1);
        } else {
          let next = rest$1.head;
          let rest$2 = rest$1.tail;
          let direction$1 = (() => {
            let $1 = compare4(new$1, next);
            if ($1 instanceof Lt) {
              return new Ascending();
            } else if ($1 instanceof Eq) {
              return new Ascending();
            } else {
              return new Descending();
            }
          })();
          loop$list = rest$2;
          loop$compare = compare4;
          loop$growing = toList([new$1]);
          loop$direction = direction$1;
          loop$prev = next;
          loop$acc = acc$1;
        }
      } else {
        let acc$1 = (() => {
          if (direction instanceof Ascending) {
            return prepend(do_reverse(growing$1, toList([])), acc);
          } else {
            return prepend(growing$1, acc);
          }
        })();
        if (rest$1.hasLength(0)) {
          return prepend(toList([new$1]), acc$1);
        } else {
          let next = rest$1.head;
          let rest$2 = rest$1.tail;
          let direction$1 = (() => {
            let $1 = compare4(new$1, next);
            if ($1 instanceof Lt) {
              return new Ascending();
            } else if ($1 instanceof Eq) {
              return new Ascending();
            } else {
              return new Descending();
            }
          })();
          loop$list = rest$2;
          loop$compare = compare4;
          loop$growing = toList([new$1]);
          loop$direction = direction$1;
          loop$prev = next;
          loop$acc = acc$1;
        }
      }
    }
  }
}
function merge_ascendings(loop$list1, loop$list2, loop$compare, loop$acc) {
  while (true) {
    let list1 = loop$list1;
    let list2 = loop$list2;
    let compare4 = loop$compare;
    let acc = loop$acc;
    if (list1.hasLength(0)) {
      let list = list2;
      return do_reverse(list, acc);
    } else if (list2.hasLength(0)) {
      let list = list1;
      return do_reverse(list, acc);
    } else {
      let first1 = list1.head;
      let rest1 = list1.tail;
      let first2 = list2.head;
      let rest2 = list2.tail;
      let $ = compare4(first1, first2);
      if ($ instanceof Lt) {
        loop$list1 = rest1;
        loop$list2 = list2;
        loop$compare = compare4;
        loop$acc = prepend(first1, acc);
      } else if ($ instanceof Gt) {
        loop$list1 = list1;
        loop$list2 = rest2;
        loop$compare = compare4;
        loop$acc = prepend(first2, acc);
      } else {
        loop$list1 = list1;
        loop$list2 = rest2;
        loop$compare = compare4;
        loop$acc = prepend(first2, acc);
      }
    }
  }
}
function merge_ascending_pairs(loop$sequences, loop$compare, loop$acc) {
  while (true) {
    let sequences2 = loop$sequences;
    let compare4 = loop$compare;
    let acc = loop$acc;
    if (sequences2.hasLength(0)) {
      return do_reverse(acc, toList([]));
    } else if (sequences2.hasLength(1)) {
      let sequence = sequences2.head;
      return do_reverse(
        prepend(do_reverse(sequence, toList([])), acc),
        toList([])
      );
    } else {
      let ascending1 = sequences2.head;
      let ascending2 = sequences2.tail.head;
      let rest$1 = sequences2.tail.tail;
      let descending = merge_ascendings(
        ascending1,
        ascending2,
        compare4,
        toList([])
      );
      loop$sequences = rest$1;
      loop$compare = compare4;
      loop$acc = prepend(descending, acc);
    }
  }
}
function merge_descendings(loop$list1, loop$list2, loop$compare, loop$acc) {
  while (true) {
    let list1 = loop$list1;
    let list2 = loop$list2;
    let compare4 = loop$compare;
    let acc = loop$acc;
    if (list1.hasLength(0)) {
      let list = list2;
      return do_reverse(list, acc);
    } else if (list2.hasLength(0)) {
      let list = list1;
      return do_reverse(list, acc);
    } else {
      let first1 = list1.head;
      let rest1 = list1.tail;
      let first2 = list2.head;
      let rest2 = list2.tail;
      let $ = compare4(first1, first2);
      if ($ instanceof Lt) {
        loop$list1 = list1;
        loop$list2 = rest2;
        loop$compare = compare4;
        loop$acc = prepend(first2, acc);
      } else if ($ instanceof Gt) {
        loop$list1 = rest1;
        loop$list2 = list2;
        loop$compare = compare4;
        loop$acc = prepend(first1, acc);
      } else {
        loop$list1 = rest1;
        loop$list2 = list2;
        loop$compare = compare4;
        loop$acc = prepend(first1, acc);
      }
    }
  }
}
function merge_descending_pairs(loop$sequences, loop$compare, loop$acc) {
  while (true) {
    let sequences2 = loop$sequences;
    let compare4 = loop$compare;
    let acc = loop$acc;
    if (sequences2.hasLength(0)) {
      return do_reverse(acc, toList([]));
    } else if (sequences2.hasLength(1)) {
      let sequence = sequences2.head;
      return do_reverse(
        prepend(do_reverse(sequence, toList([])), acc),
        toList([])
      );
    } else {
      let descending1 = sequences2.head;
      let descending2 = sequences2.tail.head;
      let rest$1 = sequences2.tail.tail;
      let ascending = merge_descendings(
        descending1,
        descending2,
        compare4,
        toList([])
      );
      loop$sequences = rest$1;
      loop$compare = compare4;
      loop$acc = prepend(ascending, acc);
    }
  }
}
function merge_all(loop$sequences, loop$direction, loop$compare) {
  while (true) {
    let sequences2 = loop$sequences;
    let direction = loop$direction;
    let compare4 = loop$compare;
    if (sequences2.hasLength(0)) {
      return toList([]);
    } else if (sequences2.hasLength(1) && direction instanceof Ascending) {
      let sequence = sequences2.head;
      return sequence;
    } else if (sequences2.hasLength(1) && direction instanceof Descending) {
      let sequence = sequences2.head;
      return do_reverse(sequence, toList([]));
    } else if (direction instanceof Ascending) {
      let sequences$1 = merge_ascending_pairs(sequences2, compare4, toList([]));
      loop$sequences = sequences$1;
      loop$direction = new Descending();
      loop$compare = compare4;
    } else {
      let sequences$1 = merge_descending_pairs(sequences2, compare4, toList([]));
      loop$sequences = sequences$1;
      loop$direction = new Ascending();
      loop$compare = compare4;
    }
  }
}
function sort(list, compare4) {
  if (list.hasLength(0)) {
    return toList([]);
  } else if (list.hasLength(1)) {
    let x = list.head;
    return toList([x]);
  } else {
    let x = list.head;
    let y = list.tail.head;
    let rest$1 = list.tail.tail;
    let direction = (() => {
      let $ = compare4(x, y);
      if ($ instanceof Lt) {
        return new Ascending();
      } else if ($ instanceof Eq) {
        return new Ascending();
      } else {
        return new Descending();
      }
    })();
    let sequences$1 = sequences(
      rest$1,
      compare4,
      toList([x]),
      direction,
      y,
      toList([])
    );
    return merge_all(sequences$1, new Ascending(), compare4);
  }
}

// build/dev/javascript/gleam_stdlib/gleam/result.mjs
function map2(result, fun) {
  if (result.isOk()) {
    let x = result[0];
    return new Ok(fun(x));
  } else {
    let e = result[0];
    return new Error(e);
  }
}
function map_error(result, fun) {
  if (result.isOk()) {
    let x = result[0];
    return new Ok(x);
  } else {
    let error = result[0];
    return new Error(fun(error));
  }
}
function try$(result, fun) {
  if (result.isOk()) {
    let x = result[0];
    return fun(x);
  } else {
    let e = result[0];
    return new Error(e);
  }
}
function replace_error(result, error) {
  if (result.isOk()) {
    let x = result[0];
    return new Ok(x);
  } else {
    return new Error(error);
  }
}

// build/dev/javascript/gleam_stdlib/gleam/string_builder.mjs
function from_strings(strings) {
  return concat(strings);
}
function to_string3(builder) {
  return identity(builder);
}

// build/dev/javascript/gleam_stdlib/gleam/string.mjs
function compare3(a, b) {
  let $ = a === b;
  if ($) {
    return new Eq();
  } else {
    let $1 = less_than(a, b);
    if ($1) {
      return new Lt();
    } else {
      return new Gt();
    }
  }
}
function trim2(string3) {
  return trim(string3);
}
function trim_left2(string3) {
  return trim_left(string3);
}
function inspect2(term) {
  let _pipe = inspect(term);
  return to_string3(_pipe);
}

// build/dev/javascript/gleam_stdlib/gleam/dynamic.mjs
var DecodeError = class extends CustomType {
  constructor(expected, found, path) {
    super();
    this.expected = expected;
    this.found = found;
    this.path = path;
  }
};
function from(a) {
  return identity(a);
}
function classify(data) {
  return classify_dynamic(data);
}
function int(data) {
  return decode_int(data);
}
function any(decoders) {
  return (data) => {
    if (decoders.hasLength(0)) {
      return new Error(
        toList([new DecodeError("another type", classify(data), toList([]))])
      );
    } else {
      let decoder = decoders.head;
      let decoders$1 = decoders.tail;
      let $ = decoder(data);
      if ($.isOk()) {
        let decoded = $[0];
        return new Ok(decoded);
      } else {
        return any(decoders$1)(data);
      }
    }
  };
}
function push_path(error, name) {
  let name$1 = from(name);
  let decoder = any(
    toList([string, (x) => {
      return map2(int(x), to_string2);
    }])
  );
  let name$2 = (() => {
    let $ = decoder(name$1);
    if ($.isOk()) {
      let name$22 = $[0];
      return name$22;
    } else {
      let _pipe = toList(["<", classify(name$1), ">"]);
      let _pipe$1 = from_strings(_pipe);
      return to_string3(_pipe$1);
    }
  })();
  return error.withFields({ path: prepend(name$2, error.path) });
}
function map_errors(result, f) {
  return map_error(
    result,
    (_capture) => {
      return map(_capture, f);
    }
  );
}
function string(data) {
  return decode_string(data);
}
function field(name, inner_type) {
  return (value2) => {
    let missing_field_error = new DecodeError("field", "nothing", toList([]));
    return try$(
      decode_field(value2, name),
      (maybe_inner) => {
        let _pipe = maybe_inner;
        let _pipe$1 = to_result(_pipe, toList([missing_field_error]));
        let _pipe$2 = try$(_pipe$1, inner_type);
        return map_errors(
          _pipe$2,
          (_capture) => {
            return push_path(_capture, name);
          }
        );
      }
    );
  };
}

// build/dev/javascript/gleam_stdlib/dict.mjs
var referenceMap = /* @__PURE__ */ new WeakMap();
var tempDataView = new DataView(new ArrayBuffer(8));
var referenceUID = 0;
function hashByReference(o) {
  const known = referenceMap.get(o);
  if (known !== void 0) {
    return known;
  }
  const hash = referenceUID++;
  if (referenceUID === 2147483647) {
    referenceUID = 0;
  }
  referenceMap.set(o, hash);
  return hash;
}
function hashMerge(a, b) {
  return a ^ b + 2654435769 + (a << 6) + (a >> 2) | 0;
}
function hashString(s) {
  let hash = 0;
  const len = s.length;
  for (let i = 0; i < len; i++) {
    hash = Math.imul(31, hash) + s.charCodeAt(i) | 0;
  }
  return hash;
}
function hashNumber(n) {
  tempDataView.setFloat64(0, n);
  const i = tempDataView.getInt32(0);
  const j = tempDataView.getInt32(4);
  return Math.imul(73244475, i >> 16 ^ i) ^ j;
}
function hashBigInt(n) {
  return hashString(n.toString());
}
function hashObject(o) {
  const proto = Object.getPrototypeOf(o);
  if (proto !== null && typeof proto.hashCode === "function") {
    try {
      const code = o.hashCode(o);
      if (typeof code === "number") {
        return code;
      }
    } catch {
    }
  }
  if (o instanceof Promise || o instanceof WeakSet || o instanceof WeakMap) {
    return hashByReference(o);
  }
  if (o instanceof Date) {
    return hashNumber(o.getTime());
  }
  let h = 0;
  if (o instanceof ArrayBuffer) {
    o = new Uint8Array(o);
  }
  if (Array.isArray(o) || o instanceof Uint8Array) {
    for (let i = 0; i < o.length; i++) {
      h = Math.imul(31, h) + getHash(o[i]) | 0;
    }
  } else if (o instanceof Set) {
    o.forEach((v) => {
      h = h + getHash(v) | 0;
    });
  } else if (o instanceof Map) {
    o.forEach((v, k) => {
      h = h + hashMerge(getHash(v), getHash(k)) | 0;
    });
  } else {
    const keys2 = Object.keys(o);
    for (let i = 0; i < keys2.length; i++) {
      const k = keys2[i];
      const v = o[k];
      h = h + hashMerge(getHash(v), hashString(k)) | 0;
    }
  }
  return h;
}
function getHash(u) {
  if (u === null)
    return 1108378658;
  if (u === void 0)
    return 1108378659;
  if (u === true)
    return 1108378657;
  if (u === false)
    return 1108378656;
  switch (typeof u) {
    case "number":
      return hashNumber(u);
    case "string":
      return hashString(u);
    case "bigint":
      return hashBigInt(u);
    case "object":
      return hashObject(u);
    case "symbol":
      return hashByReference(u);
    case "function":
      return hashByReference(u);
    default:
      return 0;
  }
}
var SHIFT = 5;
var BUCKET_SIZE = Math.pow(2, SHIFT);
var MASK = BUCKET_SIZE - 1;
var MAX_INDEX_NODE = BUCKET_SIZE / 2;
var MIN_ARRAY_NODE = BUCKET_SIZE / 4;
var ENTRY = 0;
var ARRAY_NODE = 1;
var INDEX_NODE = 2;
var COLLISION_NODE = 3;
var EMPTY = {
  type: INDEX_NODE,
  bitmap: 0,
  array: []
};
function mask(hash, shift) {
  return hash >>> shift & MASK;
}
function bitpos(hash, shift) {
  return 1 << mask(hash, shift);
}
function bitcount(x) {
  x -= x >> 1 & 1431655765;
  x = (x & 858993459) + (x >> 2 & 858993459);
  x = x + (x >> 4) & 252645135;
  x += x >> 8;
  x += x >> 16;
  return x & 127;
}
function index(bitmap, bit) {
  return bitcount(bitmap & bit - 1);
}
function cloneAndSet(arr, at, val) {
  const len = arr.length;
  const out = new Array(len);
  for (let i = 0; i < len; ++i) {
    out[i] = arr[i];
  }
  out[at] = val;
  return out;
}
function spliceIn(arr, at, val) {
  const len = arr.length;
  const out = new Array(len + 1);
  let i = 0;
  let g = 0;
  while (i < at) {
    out[g++] = arr[i++];
  }
  out[g++] = val;
  while (i < len) {
    out[g++] = arr[i++];
  }
  return out;
}
function spliceOut(arr, at) {
  const len = arr.length;
  const out = new Array(len - 1);
  let i = 0;
  let g = 0;
  while (i < at) {
    out[g++] = arr[i++];
  }
  ++i;
  while (i < len) {
    out[g++] = arr[i++];
  }
  return out;
}
function createNode(shift, key1, val1, key2hash, key2, val2) {
  const key1hash = getHash(key1);
  if (key1hash === key2hash) {
    return {
      type: COLLISION_NODE,
      hash: key1hash,
      array: [
        { type: ENTRY, k: key1, v: val1 },
        { type: ENTRY, k: key2, v: val2 }
      ]
    };
  }
  const addedLeaf = { val: false };
  return assoc(
    assocIndex(EMPTY, shift, key1hash, key1, val1, addedLeaf),
    shift,
    key2hash,
    key2,
    val2,
    addedLeaf
  );
}
function assoc(root2, shift, hash, key, val, addedLeaf) {
  switch (root2.type) {
    case ARRAY_NODE:
      return assocArray(root2, shift, hash, key, val, addedLeaf);
    case INDEX_NODE:
      return assocIndex(root2, shift, hash, key, val, addedLeaf);
    case COLLISION_NODE:
      return assocCollision(root2, shift, hash, key, val, addedLeaf);
  }
}
function assocArray(root2, shift, hash, key, val, addedLeaf) {
  const idx = mask(hash, shift);
  const node = root2.array[idx];
  if (node === void 0) {
    addedLeaf.val = true;
    return {
      type: ARRAY_NODE,
      size: root2.size + 1,
      array: cloneAndSet(root2.array, idx, { type: ENTRY, k: key, v: val })
    };
  }
  if (node.type === ENTRY) {
    if (isEqual(key, node.k)) {
      if (val === node.v) {
        return root2;
      }
      return {
        type: ARRAY_NODE,
        size: root2.size,
        array: cloneAndSet(root2.array, idx, {
          type: ENTRY,
          k: key,
          v: val
        })
      };
    }
    addedLeaf.val = true;
    return {
      type: ARRAY_NODE,
      size: root2.size,
      array: cloneAndSet(
        root2.array,
        idx,
        createNode(shift + SHIFT, node.k, node.v, hash, key, val)
      )
    };
  }
  const n = assoc(node, shift + SHIFT, hash, key, val, addedLeaf);
  if (n === node) {
    return root2;
  }
  return {
    type: ARRAY_NODE,
    size: root2.size,
    array: cloneAndSet(root2.array, idx, n)
  };
}
function assocIndex(root2, shift, hash, key, val, addedLeaf) {
  const bit = bitpos(hash, shift);
  const idx = index(root2.bitmap, bit);
  if ((root2.bitmap & bit) !== 0) {
    const node = root2.array[idx];
    if (node.type !== ENTRY) {
      const n = assoc(node, shift + SHIFT, hash, key, val, addedLeaf);
      if (n === node) {
        return root2;
      }
      return {
        type: INDEX_NODE,
        bitmap: root2.bitmap,
        array: cloneAndSet(root2.array, idx, n)
      };
    }
    const nodeKey = node.k;
    if (isEqual(key, nodeKey)) {
      if (val === node.v) {
        return root2;
      }
      return {
        type: INDEX_NODE,
        bitmap: root2.bitmap,
        array: cloneAndSet(root2.array, idx, {
          type: ENTRY,
          k: key,
          v: val
        })
      };
    }
    addedLeaf.val = true;
    return {
      type: INDEX_NODE,
      bitmap: root2.bitmap,
      array: cloneAndSet(
        root2.array,
        idx,
        createNode(shift + SHIFT, nodeKey, node.v, hash, key, val)
      )
    };
  } else {
    const n = root2.array.length;
    if (n >= MAX_INDEX_NODE) {
      const nodes = new Array(32);
      const jdx = mask(hash, shift);
      nodes[jdx] = assocIndex(EMPTY, shift + SHIFT, hash, key, val, addedLeaf);
      let j = 0;
      let bitmap = root2.bitmap;
      for (let i = 0; i < 32; i++) {
        if ((bitmap & 1) !== 0) {
          const node = root2.array[j++];
          nodes[i] = node;
        }
        bitmap = bitmap >>> 1;
      }
      return {
        type: ARRAY_NODE,
        size: n + 1,
        array: nodes
      };
    } else {
      const newArray = spliceIn(root2.array, idx, {
        type: ENTRY,
        k: key,
        v: val
      });
      addedLeaf.val = true;
      return {
        type: INDEX_NODE,
        bitmap: root2.bitmap | bit,
        array: newArray
      };
    }
  }
}
function assocCollision(root2, shift, hash, key, val, addedLeaf) {
  if (hash === root2.hash) {
    const idx = collisionIndexOf(root2, key);
    if (idx !== -1) {
      const entry = root2.array[idx];
      if (entry.v === val) {
        return root2;
      }
      return {
        type: COLLISION_NODE,
        hash,
        array: cloneAndSet(root2.array, idx, { type: ENTRY, k: key, v: val })
      };
    }
    const size = root2.array.length;
    addedLeaf.val = true;
    return {
      type: COLLISION_NODE,
      hash,
      array: cloneAndSet(root2.array, size, { type: ENTRY, k: key, v: val })
    };
  }
  return assoc(
    {
      type: INDEX_NODE,
      bitmap: bitpos(root2.hash, shift),
      array: [root2]
    },
    shift,
    hash,
    key,
    val,
    addedLeaf
  );
}
function collisionIndexOf(root2, key) {
  const size = root2.array.length;
  for (let i = 0; i < size; i++) {
    if (isEqual(key, root2.array[i].k)) {
      return i;
    }
  }
  return -1;
}
function find(root2, shift, hash, key) {
  switch (root2.type) {
    case ARRAY_NODE:
      return findArray(root2, shift, hash, key);
    case INDEX_NODE:
      return findIndex(root2, shift, hash, key);
    case COLLISION_NODE:
      return findCollision(root2, key);
  }
}
function findArray(root2, shift, hash, key) {
  const idx = mask(hash, shift);
  const node = root2.array[idx];
  if (node === void 0) {
    return void 0;
  }
  if (node.type !== ENTRY) {
    return find(node, shift + SHIFT, hash, key);
  }
  if (isEqual(key, node.k)) {
    return node;
  }
  return void 0;
}
function findIndex(root2, shift, hash, key) {
  const bit = bitpos(hash, shift);
  if ((root2.bitmap & bit) === 0) {
    return void 0;
  }
  const idx = index(root2.bitmap, bit);
  const node = root2.array[idx];
  if (node.type !== ENTRY) {
    return find(node, shift + SHIFT, hash, key);
  }
  if (isEqual(key, node.k)) {
    return node;
  }
  return void 0;
}
function findCollision(root2, key) {
  const idx = collisionIndexOf(root2, key);
  if (idx < 0) {
    return void 0;
  }
  return root2.array[idx];
}
function without(root2, shift, hash, key) {
  switch (root2.type) {
    case ARRAY_NODE:
      return withoutArray(root2, shift, hash, key);
    case INDEX_NODE:
      return withoutIndex(root2, shift, hash, key);
    case COLLISION_NODE:
      return withoutCollision(root2, key);
  }
}
function withoutArray(root2, shift, hash, key) {
  const idx = mask(hash, shift);
  const node = root2.array[idx];
  if (node === void 0) {
    return root2;
  }
  let n = void 0;
  if (node.type === ENTRY) {
    if (!isEqual(node.k, key)) {
      return root2;
    }
  } else {
    n = without(node, shift + SHIFT, hash, key);
    if (n === node) {
      return root2;
    }
  }
  if (n === void 0) {
    if (root2.size <= MIN_ARRAY_NODE) {
      const arr = root2.array;
      const out = new Array(root2.size - 1);
      let i = 0;
      let j = 0;
      let bitmap = 0;
      while (i < idx) {
        const nv = arr[i];
        if (nv !== void 0) {
          out[j] = nv;
          bitmap |= 1 << i;
          ++j;
        }
        ++i;
      }
      ++i;
      while (i < arr.length) {
        const nv = arr[i];
        if (nv !== void 0) {
          out[j] = nv;
          bitmap |= 1 << i;
          ++j;
        }
        ++i;
      }
      return {
        type: INDEX_NODE,
        bitmap,
        array: out
      };
    }
    return {
      type: ARRAY_NODE,
      size: root2.size - 1,
      array: cloneAndSet(root2.array, idx, n)
    };
  }
  return {
    type: ARRAY_NODE,
    size: root2.size,
    array: cloneAndSet(root2.array, idx, n)
  };
}
function withoutIndex(root2, shift, hash, key) {
  const bit = bitpos(hash, shift);
  if ((root2.bitmap & bit) === 0) {
    return root2;
  }
  const idx = index(root2.bitmap, bit);
  const node = root2.array[idx];
  if (node.type !== ENTRY) {
    const n = without(node, shift + SHIFT, hash, key);
    if (n === node) {
      return root2;
    }
    if (n !== void 0) {
      return {
        type: INDEX_NODE,
        bitmap: root2.bitmap,
        array: cloneAndSet(root2.array, idx, n)
      };
    }
    if (root2.bitmap === bit) {
      return void 0;
    }
    return {
      type: INDEX_NODE,
      bitmap: root2.bitmap ^ bit,
      array: spliceOut(root2.array, idx)
    };
  }
  if (isEqual(key, node.k)) {
    if (root2.bitmap === bit) {
      return void 0;
    }
    return {
      type: INDEX_NODE,
      bitmap: root2.bitmap ^ bit,
      array: spliceOut(root2.array, idx)
    };
  }
  return root2;
}
function withoutCollision(root2, key) {
  const idx = collisionIndexOf(root2, key);
  if (idx < 0) {
    return root2;
  }
  if (root2.array.length === 1) {
    return void 0;
  }
  return {
    type: COLLISION_NODE,
    hash: root2.hash,
    array: spliceOut(root2.array, idx)
  };
}
function forEach(root2, fn) {
  if (root2 === void 0) {
    return;
  }
  const items = root2.array;
  const size = items.length;
  for (let i = 0; i < size; i++) {
    const item = items[i];
    if (item === void 0) {
      continue;
    }
    if (item.type === ENTRY) {
      fn(item.v, item.k);
      continue;
    }
    forEach(item, fn);
  }
}
var Dict = class _Dict {
  /**
   * @template V
   * @param {Record<string,V>} o
   * @returns {Dict<string,V>}
   */
  static fromObject(o) {
    const keys2 = Object.keys(o);
    let m = _Dict.new();
    for (let i = 0; i < keys2.length; i++) {
      const k = keys2[i];
      m = m.set(k, o[k]);
    }
    return m;
  }
  /**
   * @template K,V
   * @param {Map<K,V>} o
   * @returns {Dict<K,V>}
   */
  static fromMap(o) {
    let m = _Dict.new();
    o.forEach((v, k) => {
      m = m.set(k, v);
    });
    return m;
  }
  static new() {
    return new _Dict(void 0, 0);
  }
  /**
   * @param {undefined | Node<K,V>} root
   * @param {number} size
   */
  constructor(root2, size) {
    this.root = root2;
    this.size = size;
  }
  /**
   * @template NotFound
   * @param {K} key
   * @param {NotFound} notFound
   * @returns {NotFound | V}
   */
  get(key, notFound) {
    if (this.root === void 0) {
      return notFound;
    }
    const found = find(this.root, 0, getHash(key), key);
    if (found === void 0) {
      return notFound;
    }
    return found.v;
  }
  /**
   * @param {K} key
   * @param {V} val
   * @returns {Dict<K,V>}
   */
  set(key, val) {
    const addedLeaf = { val: false };
    const root2 = this.root === void 0 ? EMPTY : this.root;
    const newRoot = assoc(root2, 0, getHash(key), key, val, addedLeaf);
    if (newRoot === this.root) {
      return this;
    }
    return new _Dict(newRoot, addedLeaf.val ? this.size + 1 : this.size);
  }
  /**
   * @param {K} key
   * @returns {Dict<K,V>}
   */
  delete(key) {
    if (this.root === void 0) {
      return this;
    }
    const newRoot = without(this.root, 0, getHash(key), key);
    if (newRoot === this.root) {
      return this;
    }
    if (newRoot === void 0) {
      return _Dict.new();
    }
    return new _Dict(newRoot, this.size - 1);
  }
  /**
   * @param {K} key
   * @returns {boolean}
   */
  has(key) {
    if (this.root === void 0) {
      return false;
    }
    return find(this.root, 0, getHash(key), key) !== void 0;
  }
  /**
   * @returns {[K,V][]}
   */
  entries() {
    if (this.root === void 0) {
      return [];
    }
    const result = [];
    this.forEach((v, k) => result.push([k, v]));
    return result;
  }
  /**
   *
   * @param {(val:V,key:K)=>void} fn
   */
  forEach(fn) {
    forEach(this.root, fn);
  }
  hashCode() {
    let h = 0;
    this.forEach((v, k) => {
      h = h + hashMerge(getHash(v), getHash(k)) | 0;
    });
    return h;
  }
  /**
   * @param {unknown} o
   * @returns {boolean}
   */
  equals(o) {
    if (!(o instanceof _Dict) || this.size !== o.size) {
      return false;
    }
    let equal = true;
    this.forEach((v, k) => {
      equal = equal && isEqual(o.get(k, !v), v);
    });
    return equal;
  }
};

// build/dev/javascript/gleam_stdlib/gleam_stdlib.mjs
var Nil = void 0;
var NOT_FOUND = {};
function identity(x) {
  return x;
}
function parse_int(value2) {
  if (/^[-+]?(\d+)$/.test(value2)) {
    return new Ok(parseInt(value2));
  } else {
    return new Error(Nil);
  }
}
function parse_float(value2) {
  if (/^[-+]?(\d+)\.(\d+)([eE][-+]?\d+)?$/.test(value2)) {
    return new Ok(parseFloat(value2));
  } else {
    return new Error(Nil);
  }
}
function to_string(term) {
  return term.toString();
}
function graphemes(string3) {
  const iterator = graphemes_iterator(string3);
  if (iterator) {
    return List.fromArray(Array.from(iterator).map((item) => item.segment));
  } else {
    return List.fromArray(string3.match(/./gsu));
  }
}
function graphemes_iterator(string3) {
  if (Intl && Intl.Segmenter) {
    return new Intl.Segmenter().segment(string3)[Symbol.iterator]();
  }
}
function less_than(a, b) {
  return a < b;
}
function concat(xs) {
  let result = "";
  for (const x of xs) {
    result = result + x;
  }
  return result;
}
var unicode_whitespaces = [
  " ",
  // Space
  "	",
  // Horizontal tab
  "\n",
  // Line feed
  "\v",
  // Vertical tab
  "\f",
  // Form feed
  "\r",
  // Carriage return
  "\x85",
  // Next line
  "\u2028",
  // Line separator
  "\u2029"
  // Paragraph separator
].join();
var left_trim_regex = new RegExp(`^([${unicode_whitespaces}]*)`, "g");
var right_trim_regex = new RegExp(`([${unicode_whitespaces}]*)$`, "g");
function trim(string3) {
  return trim_left(trim_right(string3));
}
function trim_left(string3) {
  return string3.replace(left_trim_regex, "");
}
function trim_right(string3) {
  return string3.replace(right_trim_regex, "");
}
function ceiling(float3) {
  return Math.ceil(float3);
}
function power(base, exponent) {
  return Math.pow(base, exponent);
}
function new_map() {
  return Dict.new();
}
function map_to_list(map4) {
  return List.fromArray(map4.entries());
}
function map_get(map4, key) {
  const value2 = map4.get(key, NOT_FOUND);
  if (value2 === NOT_FOUND) {
    return new Error(Nil);
  }
  return new Ok(value2);
}
function map_insert(key, value2, map4) {
  return map4.set(key, value2);
}
function classify_dynamic(data) {
  if (typeof data === "string") {
    return "String";
  } else if (typeof data === "boolean") {
    return "Bool";
  } else if (data instanceof Result) {
    return "Result";
  } else if (data instanceof List) {
    return "List";
  } else if (data instanceof BitArray) {
    return "BitArray";
  } else if (data instanceof Dict) {
    return "Dict";
  } else if (Number.isInteger(data)) {
    return "Int";
  } else if (Array.isArray(data)) {
    return `Tuple of ${data.length} elements`;
  } else if (typeof data === "number") {
    return "Float";
  } else if (data === null) {
    return "Null";
  } else if (data === void 0) {
    return "Nil";
  } else {
    const type = typeof data;
    return type.charAt(0).toUpperCase() + type.slice(1);
  }
}
function decoder_error(expected, got) {
  return decoder_error_no_classify(expected, classify_dynamic(got));
}
function decoder_error_no_classify(expected, got) {
  return new Error(
    List.fromArray([new DecodeError(expected, got, List.fromArray([]))])
  );
}
function decode_string(data) {
  return typeof data === "string" ? new Ok(data) : decoder_error("String", data);
}
function decode_int(data) {
  return Number.isInteger(data) ? new Ok(data) : decoder_error("Int", data);
}
function decode_field(value2, name) {
  const not_a_map_error = () => decoder_error("Dict", value2);
  if (value2 instanceof Dict || value2 instanceof WeakMap || value2 instanceof Map) {
    const entry = map_get(value2, name);
    return new Ok(entry.isOk() ? new Some(entry[0]) : new None());
  } else if (value2 === null) {
    return not_a_map_error();
  } else if (Object.getPrototypeOf(value2) == Object.prototype) {
    return try_get_field(value2, name, () => new Ok(new None()));
  } else {
    return try_get_field(value2, name, not_a_map_error);
  }
}
function try_get_field(value2, field2, or_else) {
  try {
    return field2 in value2 ? new Ok(new Some(value2[field2])) : or_else();
  } catch {
    return or_else();
  }
}
function inspect(v) {
  const t = typeof v;
  if (v === true)
    return "True";
  if (v === false)
    return "False";
  if (v === null)
    return "//js(null)";
  if (v === void 0)
    return "Nil";
  if (t === "string")
    return inspectString(v);
  if (t === "bigint" || t === "number")
    return v.toString();
  if (Array.isArray(v))
    return `#(${v.map(inspect).join(", ")})`;
  if (v instanceof List)
    return inspectList(v);
  if (v instanceof UtfCodepoint)
    return inspectUtfCodepoint(v);
  if (v instanceof BitArray)
    return inspectBitArray(v);
  if (v instanceof CustomType)
    return inspectCustomType(v);
  if (v instanceof Dict)
    return inspectDict(v);
  if (v instanceof Set)
    return `//js(Set(${[...v].map(inspect).join(", ")}))`;
  if (v instanceof RegExp)
    return `//js(${v})`;
  if (v instanceof Date)
    return `//js(Date("${v.toISOString()}"))`;
  if (v instanceof Function) {
    const args = [];
    for (const i of Array(v.length).keys())
      args.push(String.fromCharCode(i + 97));
    return `//fn(${args.join(", ")}) { ... }`;
  }
  return inspectObject(v);
}
function inspectString(str) {
  let new_str = '"';
  for (let i = 0; i < str.length; i++) {
    let char = str[i];
    switch (char) {
      case "\n":
        new_str += "\\n";
        break;
      case "\r":
        new_str += "\\r";
        break;
      case "	":
        new_str += "\\t";
        break;
      case "\f":
        new_str += "\\f";
        break;
      case "\\":
        new_str += "\\\\";
        break;
      case '"':
        new_str += '\\"';
        break;
      default:
        if (char < " " || char > "~" && char < "\xA0") {
          new_str += "\\u{" + char.charCodeAt(0).toString(16).toUpperCase().padStart(4, "0") + "}";
        } else {
          new_str += char;
        }
    }
  }
  new_str += '"';
  return new_str;
}
function inspectDict(map4) {
  let body = "dict.from_list([";
  let first2 = true;
  map4.forEach((value2, key) => {
    if (!first2)
      body = body + ", ";
    body = body + "#(" + inspect(key) + ", " + inspect(value2) + ")";
    first2 = false;
  });
  return body + "])";
}
function inspectObject(v) {
  const name = Object.getPrototypeOf(v)?.constructor?.name || "Object";
  const props = [];
  for (const k of Object.keys(v)) {
    props.push(`${inspect(k)}: ${inspect(v[k])}`);
  }
  const body = props.length ? " " + props.join(", ") + " " : "";
  const head = name === "Object" ? "" : name + " ";
  return `//js(${head}{${body}})`;
}
function inspectCustomType(record) {
  const props = Object.keys(record).map((label) => {
    const value2 = inspect(record[label]);
    return isNaN(parseInt(label)) ? `${label}: ${value2}` : value2;
  }).join(", ");
  return props ? `${record.constructor.name}(${props})` : record.constructor.name;
}
function inspectList(list) {
  return `[${list.toArray().map(inspect).join(", ")}]`;
}
function inspectBitArray(bits) {
  return `<<${Array.from(bits.buffer).join(", ")}>>`;
}
function inspectUtfCodepoint(codepoint2) {
  return `//utfcodepoint(${String.fromCodePoint(codepoint2.value)})`;
}

// build/dev/javascript/gleam_stdlib/gleam/dict.mjs
function new$() {
  return new_map();
}
function get(from2, get2) {
  return map_get(from2, get2);
}
function insert(dict, key, value2) {
  return map_insert(key, value2, dict);
}
function reverse_and_concat(loop$remaining, loop$accumulator) {
  while (true) {
    let remaining = loop$remaining;
    let accumulator = loop$accumulator;
    if (remaining.hasLength(0)) {
      return accumulator;
    } else {
      let item = remaining.head;
      let rest = remaining.tail;
      loop$remaining = rest;
      loop$accumulator = prepend(item, accumulator);
    }
  }
}
function do_keys_acc(loop$list, loop$acc) {
  while (true) {
    let list = loop$list;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse_and_concat(acc, toList([]));
    } else {
      let x = list.head;
      let xs = list.tail;
      loop$list = xs;
      loop$acc = prepend(x[0], acc);
    }
  }
}
function do_keys(dict) {
  let list_of_pairs = map_to_list(dict);
  return do_keys_acc(list_of_pairs, toList([]));
}
function keys(dict) {
  return do_keys(dict);
}
function insert_pair(dict, pair) {
  return insert(dict, pair[0], pair[1]);
}
function fold_inserts(loop$new_entries, loop$dict) {
  while (true) {
    let new_entries = loop$new_entries;
    let dict = loop$dict;
    if (new_entries.hasLength(0)) {
      return dict;
    } else {
      let x = new_entries.head;
      let xs = new_entries.tail;
      loop$new_entries = xs;
      loop$dict = insert_pair(dict, x);
    }
  }
}
function do_merge(dict, new_entries) {
  let _pipe = new_entries;
  let _pipe$1 = map_to_list(_pipe);
  return fold_inserts(_pipe$1, dict);
}
function merge(dict, new_entries) {
  return do_merge(dict, new_entries);
}

// build/dev/javascript/gleam_stdlib/gleam/bool.mjs
function guard(requirement, consequence, alternative) {
  if (requirement) {
    return consequence;
  } else {
    return alternative();
  }
}

// build/dev/javascript/lustre/lustre/effect.mjs
var Effect = class extends CustomType {
  constructor(all) {
    super();
    this.all = all;
  }
};
function none() {
  return new Effect(toList([]));
}

// build/dev/javascript/lustre/lustre/internals/vdom.mjs
var Text = class extends CustomType {
  constructor(content) {
    super();
    this.content = content;
  }
};
var Element = class extends CustomType {
  constructor(key, namespace, tag, attrs, children, self_closing, void$) {
    super();
    this.key = key;
    this.namespace = namespace;
    this.tag = tag;
    this.attrs = attrs;
    this.children = children;
    this.self_closing = self_closing;
    this.void = void$;
  }
};
var Event = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};

// build/dev/javascript/lustre/lustre/attribute.mjs
function on(name, handler) {
  return new Event("on" + name, handler);
}

// build/dev/javascript/lustre/lustre/element.mjs
function element(tag, attrs, children) {
  if (tag === "area") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "base") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "br") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "col") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "embed") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "hr") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "img") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "input") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "link") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "meta") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "param") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "source") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "track") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "wbr") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else {
    return new Element("", "", tag, attrs, children, false, false);
  }
}
function text(content) {
  return new Text(content);
}

// build/dev/javascript/lustre/lustre/internals/runtime.mjs
var Debug = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Dispatch = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Shutdown = class extends CustomType {
};
var ForceModel = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};

// build/dev/javascript/lustre/vdom.ffi.mjs
function morph(prev, next, dispatch, isComponent = false) {
  let out;
  let stack = [{ prev, next, parent: prev.parentNode }];
  while (stack.length) {
    let { prev: prev2, next: next2, parent } = stack.pop();
    if (next2.subtree !== void 0)
      next2 = next2.subtree();
    if (next2.content !== void 0) {
      if (!prev2) {
        const created = document.createTextNode(next2.content);
        parent.appendChild(created);
        out ??= created;
      } else if (prev2.nodeType === Node.TEXT_NODE) {
        if (prev2.textContent !== next2.content)
          prev2.textContent = next2.content;
        out ??= prev2;
      } else {
        const created = document.createTextNode(next2.content);
        parent.replaceChild(created, prev2);
        out ??= created;
      }
    } else if (next2.tag !== void 0) {
      const created = createElementNode({
        prev: prev2,
        next: next2,
        dispatch,
        stack,
        isComponent
      });
      if (!prev2) {
        parent.appendChild(created);
      } else if (prev2 !== created) {
        parent.replaceChild(created, prev2);
      }
      out ??= created;
    } else if (next2.elements !== void 0) {
      iterateElement(next2, (fragmentElement) => {
        stack.unshift({ prev: prev2, next: fragmentElement, parent });
        prev2 = prev2?.nextSibling;
      });
    } else if (next2.subtree !== void 0) {
      stack.push({ prev: prev2, next: next2, parent });
    }
  }
  return out;
}
function createElementNode({ prev, next, dispatch, stack }) {
  const namespace = next.namespace || "http://www.w3.org/1999/xhtml";
  const canMorph = prev && prev.nodeType === Node.ELEMENT_NODE && prev.localName === next.tag && prev.namespaceURI === (next.namespace || "http://www.w3.org/1999/xhtml");
  const el2 = canMorph ? prev : namespace ? document.createElementNS(namespace, next.tag) : document.createElement(next.tag);
  let handlersForEl;
  if (!registeredHandlers.has(el2)) {
    const emptyHandlers = /* @__PURE__ */ new Map();
    registeredHandlers.set(el2, emptyHandlers);
    handlersForEl = emptyHandlers;
  } else {
    handlersForEl = registeredHandlers.get(el2);
  }
  const prevHandlers = canMorph ? new Set(handlersForEl.keys()) : null;
  const prevAttributes = canMorph ? new Set(Array.from(prev.attributes, (a) => a.name)) : null;
  let className = null;
  let style = null;
  let innerHTML = null;
  for (const attr of next.attrs) {
    const name = attr[0];
    const value2 = attr[1];
    if (attr.as_property) {
      if (el2[name] !== value2)
        el2[name] = value2;
      if (canMorph)
        prevAttributes.delete(name);
    } else if (name.startsWith("on")) {
      const eventName = name.slice(2);
      const callback = dispatch(value2);
      if (!handlersForEl.has(eventName)) {
        el2.addEventListener(eventName, lustreGenericEventHandler);
      }
      handlersForEl.set(eventName, callback);
      if (canMorph)
        prevHandlers.delete(eventName);
    } else if (name.startsWith("data-lustre-on-")) {
      const eventName = name.slice(15);
      const callback = dispatch(lustreServerEventHandler);
      if (!handlersForEl.has(eventName)) {
        el2.addEventListener(eventName, lustreGenericEventHandler);
      }
      handlersForEl.set(eventName, callback);
      el2.setAttribute(name, value2);
    } else if (name === "class") {
      className = className === null ? value2 : className + " " + value2;
    } else if (name === "style") {
      style = style === null ? value2 : style + value2;
    } else if (name === "dangerous-unescaped-html") {
      innerHTML = value2;
    } else {
      if (el2.getAttribute(name) !== value2)
        el2.setAttribute(name, value2);
      if (name === "value" || name === "selected")
        el2[name] = value2;
      if (canMorph)
        prevAttributes.delete(name);
    }
  }
  if (className !== null) {
    el2.setAttribute("class", className);
    if (canMorph)
      prevAttributes.delete("class");
  }
  if (style !== null) {
    el2.setAttribute("style", style);
    if (canMorph)
      prevAttributes.delete("style");
  }
  if (canMorph) {
    for (const attr of prevAttributes) {
      el2.removeAttribute(attr);
    }
    for (const eventName of prevHandlers) {
      handlersForEl.delete(eventName);
      el2.removeEventListener(eventName, lustreGenericEventHandler);
    }
  }
  if (next.key !== void 0 && next.key !== "") {
    el2.setAttribute("data-lustre-key", next.key);
  } else if (innerHTML !== null) {
    el2.innerHTML = innerHTML;
    return el2;
  }
  let prevChild = el2.firstChild;
  let seenKeys = null;
  let keyedChildren = null;
  let incomingKeyedChildren = null;
  let firstChild = next.children[Symbol.iterator]().next().value;
  if (canMorph && firstChild !== void 0 && // Explicit checks are more verbose but truthy checks force a bunch of comparisons
  // we don't care about: it's never gonna be a number etc.
  firstChild.key !== void 0 && firstChild.key !== "") {
    seenKeys = /* @__PURE__ */ new Set();
    keyedChildren = getKeyedChildren(prev);
    incomingKeyedChildren = getKeyedChildren(next);
  }
  for (const child of next.children) {
    iterateElement(child, (currElement) => {
      if (currElement.key !== void 0 && seenKeys !== null) {
        prevChild = diffKeyedChild(
          prevChild,
          currElement,
          el2,
          stack,
          incomingKeyedChildren,
          keyedChildren,
          seenKeys
        );
      } else {
        stack.unshift({ prev: prevChild, next: currElement, parent: el2 });
        prevChild = prevChild?.nextSibling;
      }
    });
  }
  while (prevChild) {
    const next2 = prevChild.nextSibling;
    el2.removeChild(prevChild);
    prevChild = next2;
  }
  return el2;
}
var registeredHandlers = /* @__PURE__ */ new WeakMap();
function lustreGenericEventHandler(event2) {
  const target = event2.currentTarget;
  if (!registeredHandlers.has(target)) {
    target.removeEventListener(event2.type, lustreGenericEventHandler);
    return;
  }
  const handlersForEventTarget = registeredHandlers.get(target);
  if (!handlersForEventTarget.has(event2.type)) {
    target.removeEventListener(event2.type, lustreGenericEventHandler);
    return;
  }
  handlersForEventTarget.get(event2.type)(event2);
}
function lustreServerEventHandler(event2) {
  const el2 = event2.currentTarget;
  const tag = el2.getAttribute(`data-lustre-on-${event2.type}`);
  const data = JSON.parse(el2.getAttribute("data-lustre-data") || "{}");
  const include = JSON.parse(el2.getAttribute("data-lustre-include") || "[]");
  switch (event2.type) {
    case "input":
    case "change":
      include.push("target.value");
      break;
  }
  return {
    tag,
    data: include.reduce(
      (data2, property) => {
        const path = property.split(".");
        for (let i = 0, o = data2, e = event2; i < path.length; i++) {
          if (i === path.length - 1) {
            o[path[i]] = e[path[i]];
          } else {
            o[path[i]] ??= {};
            e = e[path[i]];
            o = o[path[i]];
          }
        }
        return data2;
      },
      { data }
    )
  };
}
function getKeyedChildren(el2) {
  const keyedChildren = /* @__PURE__ */ new Map();
  if (el2) {
    for (const child of el2.children) {
      iterateElement(child, (currElement) => {
        const key = currElement?.key || currElement?.getAttribute?.("data-lustre-key");
        if (key)
          keyedChildren.set(key, currElement);
      });
    }
  }
  return keyedChildren;
}
function diffKeyedChild(prevChild, child, el2, stack, incomingKeyedChildren, keyedChildren, seenKeys) {
  while (prevChild && !incomingKeyedChildren.has(prevChild.getAttribute("data-lustre-key"))) {
    const nextChild = prevChild.nextSibling;
    el2.removeChild(prevChild);
    prevChild = nextChild;
  }
  if (keyedChildren.size === 0) {
    iterateElement(child, (currChild) => {
      stack.unshift({ prev: prevChild, next: currChild, parent: el2 });
      prevChild = prevChild?.nextSibling;
    });
    return prevChild;
  }
  if (seenKeys.has(child.key)) {
    console.warn(`Duplicate key found in Lustre vnode: ${child.key}`);
    stack.unshift({ prev: null, next: child, parent: el2 });
    return prevChild;
  }
  seenKeys.add(child.key);
  const keyedChild = keyedChildren.get(child.key);
  if (!keyedChild && !prevChild) {
    stack.unshift({ prev: null, next: child, parent: el2 });
    return prevChild;
  }
  if (!keyedChild && prevChild !== null) {
    const placeholder = document.createTextNode("");
    el2.insertBefore(placeholder, prevChild);
    stack.unshift({ prev: placeholder, next: child, parent: el2 });
    return prevChild;
  }
  if (!keyedChild || keyedChild === prevChild) {
    stack.unshift({ prev: prevChild, next: child, parent: el2 });
    prevChild = prevChild?.nextSibling;
    return prevChild;
  }
  el2.insertBefore(keyedChild, prevChild);
  stack.unshift({ prev: keyedChild, next: child, parent: el2 });
  return prevChild;
}
function iterateElement(element2, processElement) {
  if (element2.elements !== void 0) {
    for (const currElement of element2.elements) {
      iterateElement(currElement, processElement);
    }
  } else if (element2.subtree !== void 0) {
    iterateElement(element2.subtree(), processElement);
  } else {
    processElement(element2);
  }
}

// build/dev/javascript/lustre/client-runtime.ffi.mjs
var LustreClientApplication2 = class _LustreClientApplication {
  #root = null;
  #queue = [];
  #effects = [];
  #didUpdate = false;
  #isComponent = false;
  #model = null;
  #update = null;
  #view = null;
  static start(flags, selector, init3, update2, view2) {
    if (!is_browser())
      return new Error(new NotABrowser());
    const root2 = selector instanceof HTMLElement ? selector : document.querySelector(selector);
    if (!root2)
      return new Error(new ElementNotFound(selector));
    const app = new _LustreClientApplication(init3(flags), update2, view2, root2);
    return new Ok((msg) => app.send(msg));
  }
  constructor([model, effects], update2, view2, root2 = document.body, isComponent = false) {
    this.#model = model;
    this.#update = update2;
    this.#view = view2;
    this.#root = root2;
    this.#effects = effects.all.toArray();
    this.#didUpdate = true;
    this.#isComponent = isComponent;
    window.requestAnimationFrame(() => this.#tick());
  }
  send(action) {
    switch (true) {
      case action instanceof Dispatch: {
        this.#queue.push(action[0]);
        this.#tick();
        return;
      }
      case action instanceof Shutdown: {
        this.#shutdown();
        return;
      }
      case action instanceof Debug: {
        this.#debug(action[0]);
        return;
      }
      default:
        return;
    }
  }
  emit(event2, data) {
    this.#root.dispatchEvent(
      new CustomEvent(event2, {
        bubbles: true,
        detail: data,
        composed: true
      })
    );
  }
  #tick() {
    this.#flush_queue();
    if (this.#didUpdate) {
      const vdom = this.#view(this.#model);
      const dispatch = (handler) => (e) => {
        const result = handler(e);
        if (result instanceof Ok) {
          this.send(new Dispatch(result[0]));
        }
      };
      this.#didUpdate = false;
      this.#root = morph(this.#root, vdom, dispatch, this.#isComponent);
    }
  }
  #flush_queue(iterations = 0) {
    while (this.#queue.length) {
      const [next, effects] = this.#update(this.#model, this.#queue.shift());
      this.#didUpdate ||= this.#model !== next;
      this.#model = next;
      this.#effects = this.#effects.concat(effects.all.toArray());
    }
    while (this.#effects.length) {
      this.#effects.shift()(
        (msg) => this.send(new Dispatch(msg)),
        (event2, data) => this.emit(event2, data)
      );
    }
    if (this.#queue.length) {
      if (iterations < 5) {
        this.#flush_queue(++iterations);
      } else {
        window.requestAnimationFrame(() => this.#tick());
      }
    }
  }
  #debug(action) {
    switch (true) {
      case action instanceof ForceModel: {
        const vdom = this.#view(action[0]);
        const dispatch = (handler) => (e) => {
          const result = handler(e);
          if (result instanceof Ok) {
            this.send(new Dispatch(result[0]));
          }
        };
        this.#queue = [];
        this.#effects = [];
        this.#didUpdate = false;
        this.#root = morph(this.#root, vdom, dispatch, this.#isComponent);
      }
    }
  }
  #shutdown() {
    this.#root.remove();
    this.#root = null;
    this.#model = null;
    this.#queue = [];
    this.#effects = [];
    this.#didUpdate = false;
    this.#update = () => {
    };
    this.#view = () => {
    };
  }
};
var start = (app, selector, flags) => LustreClientApplication2.start(
  flags,
  selector,
  app.init,
  app.update,
  app.view
);
var is_browser = () => globalThis.window && window.document;

// build/dev/javascript/lustre/lustre.mjs
var App = class extends CustomType {
  constructor(init3, update2, view2, on_attribute_change) {
    super();
    this.init = init3;
    this.update = update2;
    this.view = view2;
    this.on_attribute_change = on_attribute_change;
  }
};
var ElementNotFound = class extends CustomType {
  constructor(selector) {
    super();
    this.selector = selector;
  }
};
var NotABrowser = class extends CustomType {
};
function application(init3, update2, view2) {
  return new App(init3, update2, view2, new None());
}
function start3(app, selector, flags) {
  return guard(
    !is_browser(),
    new Error(new NotABrowser()),
    () => {
      return start(app, selector, flags);
    }
  );
}

// build/dev/javascript/lustre/lustre/element/html.mjs
function text2(content) {
  return text(content);
}
function div(attrs, children) {
  return element("div", attrs, children);
}
function p(attrs, children) {
  return element("p", attrs, children);
}
function input(attrs) {
  return element("input", attrs, toList([]));
}

// build/dev/javascript/lustre/lustre/event.mjs
function on2(name, handler) {
  return on(name, handler);
}
function on_click(msg) {
  return on2("click", (_) => {
    return new Ok(msg);
  });
}
function value(event2) {
  let _pipe = event2;
  return field("target", field("value", string))(
    _pipe
  );
}
function on_input(msg) {
  return on2(
    "input",
    (event2) => {
      let _pipe = value(event2);
      return map2(_pipe, msg);
    }
  );
}

// build/dev/javascript/form/form/lang/scanner.mjs
var Plus = class extends CustomType {
};
var Minus = class extends CustomType {
};
var Star = class extends CustomType {
};
var Div = class extends CustomType {
};
var StarStar = class extends CustomType {
};
var Equal = class extends CustomType {
};
var EqualEqual = class extends CustomType {
};
var BangEqual = class extends CustomType {
};
var Bang = class extends CustomType {
};
var Less = class extends CustomType {
};
var LessEqual = class extends CustomType {
};
var Greater = class extends CustomType {
};
var GreaterEqual = class extends CustomType {
};
var IntegerLiteral = class extends CustomType {
  constructor(n) {
    super();
    this.n = n;
  }
};
var FloatLiteral = class extends CustomType {
  constructor(f) {
    super();
    this.f = f;
  }
};
var TrueToken = class extends CustomType {
};
var FalseToken = class extends CustomType {
};
var And = class extends CustomType {
};
var Or = class extends CustomType {
};
var LParen = class extends CustomType {
};
var RParen = class extends CustomType {
};
var CellReference = class extends CustomType {
  constructor(key) {
    super();
    this.key = key;
  }
};
var StringLiteral = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var ScanError = class extends CustomType {
};
function maybe_parse_integer(loop$src, loop$acc) {
  while (true) {
    let src = loop$src;
    let acc = loop$acc;
    if (src.startsWith("1")) {
      let rest = src.slice(1);
      let x = "1";
      loop$src = rest;
      loop$acc = acc + x;
    } else if (src.startsWith("2")) {
      let rest = src.slice(1);
      let x = "2";
      loop$src = rest;
      loop$acc = acc + x;
    } else if (src.startsWith("3")) {
      let rest = src.slice(1);
      let x = "3";
      loop$src = rest;
      loop$acc = acc + x;
    } else if (src.startsWith("4")) {
      let rest = src.slice(1);
      let x = "4";
      loop$src = rest;
      loop$acc = acc + x;
    } else if (src.startsWith("5")) {
      let rest = src.slice(1);
      let x = "5";
      loop$src = rest;
      loop$acc = acc + x;
    } else if (src.startsWith("6")) {
      let rest = src.slice(1);
      let x = "6";
      loop$src = rest;
      loop$acc = acc + x;
    } else if (src.startsWith("7")) {
      let rest = src.slice(1);
      let x = "7";
      loop$src = rest;
      loop$acc = acc + x;
    } else if (src.startsWith("8")) {
      let rest = src.slice(1);
      let x = "8";
      loop$src = rest;
      loop$acc = acc + x;
    } else if (src.startsWith("9")) {
      let rest = src.slice(1);
      let x = "9";
      loop$src = rest;
      loop$acc = acc + x;
    } else if (src.startsWith("0")) {
      let rest = src.slice(1);
      let x = "0";
      loop$src = rest;
      loop$acc = acc + x;
    } else {
      let $ = parse2(acc);
      if ($.isOk()) {
        let x = $[0];
        return new Some([x, src]);
      } else {
        return new None();
      }
    }
  }
}
function maybe_parse_cell_ref(loop$src, loop$acc) {
  while (true) {
    let src = loop$src;
    let acc = loop$acc;
    if (src.startsWith("A")) {
      let rest = src.slice(1);
      let l = "A";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("B")) {
      let rest = src.slice(1);
      let l = "B";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("C")) {
      let rest = src.slice(1);
      let l = "C";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("D")) {
      let rest = src.slice(1);
      let l = "D";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("E")) {
      let rest = src.slice(1);
      let l = "E";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("F")) {
      let rest = src.slice(1);
      let l = "F";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("G")) {
      let rest = src.slice(1);
      let l = "G";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("H")) {
      let rest = src.slice(1);
      let l = "H";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("I")) {
      let rest = src.slice(1);
      let l = "I";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("J")) {
      let rest = src.slice(1);
      let l = "J";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("K")) {
      let rest = src.slice(1);
      let l = "K";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("L")) {
      let rest = src.slice(1);
      let l = "L";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("M")) {
      let rest = src.slice(1);
      let l = "M";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("N")) {
      let rest = src.slice(1);
      let l = "N";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("O")) {
      let rest = src.slice(1);
      let l = "O";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("P")) {
      let rest = src.slice(1);
      let l = "P";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("Q")) {
      let rest = src.slice(1);
      let l = "Q";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("R")) {
      let rest = src.slice(1);
      let l = "R";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("S")) {
      let rest = src.slice(1);
      let l = "S";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("T")) {
      let rest = src.slice(1);
      let l = "T";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("U")) {
      let rest = src.slice(1);
      let l = "U";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("V")) {
      let rest = src.slice(1);
      let l = "V";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("W")) {
      let rest = src.slice(1);
      let l = "W";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("X")) {
      let rest = src.slice(1);
      let l = "X";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("Y")) {
      let rest = src.slice(1);
      let l = "Y";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("Z")) {
      let rest = src.slice(1);
      let l = "Z";
      loop$src = rest;
      loop$acc = acc + l;
    } else {
      if (acc === "") {
        return new None();
      } else {
        let $ = maybe_parse_integer(src, "");
        if ($ instanceof Some) {
          let n = $[0][0];
          let rest = $[0][1];
          return new Some([acc + to_string2(n), rest]);
        } else {
          return new None();
        }
      }
    }
  }
}
function do_scan(loop$src, loop$acc) {
  while (true) {
    let src = loop$src;
    let acc = loop$acc;
    if (src === "") {
      return new Ok(
        (() => {
          let _pipe = acc;
          return reverse(_pipe);
        })()
      );
    } else if (src.startsWith("TRUE")) {
      let rest = src.slice(4);
      loop$src = trim_left2(rest);
      loop$acc = prepend(new TrueToken(), acc);
    } else if (src.startsWith("FALSE")) {
      let rest = src.slice(5);
      loop$src = trim_left2(rest);
      loop$acc = prepend(new FalseToken(), acc);
    } else if (src.startsWith("&&")) {
      let rest = src.slice(2);
      loop$src = trim_left2(rest);
      loop$acc = prepend(new And(), acc);
    } else if (src.startsWith("||")) {
      let rest = src.slice(2);
      loop$src = trim_left2(rest);
      loop$acc = prepend(new Or(), acc);
    } else if (src.startsWith("**")) {
      let rest = src.slice(2);
      loop$src = trim_left2(rest);
      loop$acc = prepend(new StarStar(), acc);
    } else if (src.startsWith("==")) {
      let rest = src.slice(2);
      loop$src = trim_left2(rest);
      loop$acc = prepend(new EqualEqual(), acc);
    } else if (src.startsWith("!=")) {
      let rest = src.slice(2);
      loop$src = trim_left2(rest);
      loop$acc = prepend(new BangEqual(), acc);
    } else if (src.startsWith("<=")) {
      let rest = src.slice(2);
      loop$src = trim_left2(rest);
      loop$acc = prepend(new LessEqual(), acc);
    } else if (src.startsWith(">=")) {
      let rest = src.slice(2);
      loop$src = trim_left2(rest);
      loop$acc = prepend(new GreaterEqual(), acc);
    } else if (src.startsWith("+")) {
      let rest = src.slice(1);
      loop$src = trim_left2(rest);
      loop$acc = prepend(new Plus(), acc);
    } else if (src.startsWith("-")) {
      let rest = src.slice(1);
      loop$src = trim_left2(rest);
      loop$acc = prepend(new Minus(), acc);
    } else if (src.startsWith("*")) {
      let rest = src.slice(1);
      loop$src = trim_left2(rest);
      loop$acc = prepend(new Star(), acc);
    } else if (src.startsWith("/")) {
      let rest = src.slice(1);
      loop$src = trim_left2(rest);
      loop$acc = prepend(new Div(), acc);
    } else if (src.startsWith("=")) {
      let rest = src.slice(1);
      loop$src = trim_left2(rest);
      loop$acc = prepend(new Equal(), acc);
    } else if (src.startsWith("!")) {
      let rest = src.slice(1);
      loop$src = trim_left2(rest);
      loop$acc = prepend(new Bang(), acc);
    } else if (src.startsWith("<")) {
      let rest = src.slice(1);
      loop$src = trim_left2(rest);
      loop$acc = prepend(new Less(), acc);
    } else if (src.startsWith(">")) {
      let rest = src.slice(1);
      loop$src = trim_left2(rest);
      loop$acc = prepend(new Greater(), acc);
    } else if (src.startsWith("(")) {
      let rest = src.slice(1);
      loop$src = trim_left2(rest);
      loop$acc = prepend(new LParen(), acc);
    } else if (src.startsWith(")")) {
      let rest = src.slice(1);
      loop$src = trim_left2(rest);
      loop$acc = prepend(new RParen(), acc);
    } else {
      let $ = maybe_parse_integer(src, "");
      if ($ instanceof Some) {
        let n = $[0][0];
        let rest = $[0][1];
        if (rest.startsWith(".")) {
          let rest$1 = rest.slice(1);
          let $1 = maybe_parse_integer(rest$1, "");
          if ($1 instanceof Some) {
            let m = $1[0][0];
            let rest$2 = $1[0][1];
            let $2 = parse(to_string2(n) + "." + to_string2(m));
            if (!$2.isOk()) {
              throw makeError(
                "assignment_no_match",
                "form/lang/scanner",
                97,
                "do_scan",
                "Assignment pattern did not match",
                { value: $2 }
              );
            }
            let f = $2[0];
            loop$src = trim_left2(rest$2);
            loop$acc = prepend(new FloatLiteral(f), acc);
          } else {
            return new Error(new ScanError());
          }
        } else {
          loop$src = trim_left2(rest);
          loop$acc = prepend(new IntegerLiteral(n), acc);
        }
      } else {
        let $1 = maybe_parse_cell_ref(src, "");
        if ($1 instanceof Some) {
          let cell_ref = $1[0][0];
          let rest = $1[0][1];
          loop$src = trim_left2(rest);
          loop$acc = prepend(new CellReference(cell_ref), acc);
        } else {
          return new Error(new ScanError());
        }
      }
    }
  }
}
function scan(src) {
  let $ = trim2(src);
  if ($.startsWith("=")) {
    let rest = $.slice(1);
    return do_scan(
      (() => {
        let _pipe = rest;
        return trim_left2(_pipe);
      })(),
      toList([])
    );
  } else {
    return new Ok(toList([new StringLiteral(src)]));
  }
}

// build/dev/javascript/form/form/lang/parser.mjs
var Empty2 = class extends CustomType {
};
var FloatLiteral2 = class extends CustomType {
  constructor(f) {
    super();
    this.f = f;
  }
};
var StringLiteral2 = class extends CustomType {
  constructor(txt) {
    super();
    this.txt = txt;
  }
};
var IntegerLiteral2 = class extends CustomType {
  constructor(n) {
    super();
    this.n = n;
  }
};
var CellReference2 = class extends CustomType {
  constructor(key) {
    super();
    this.key = key;
  }
};
var BooleanLiteral = class extends CustomType {
  constructor(val) {
    super();
    this.val = val;
  }
};
var UnaryOp = class extends CustomType {
  constructor(op, expr) {
    super();
    this.op = op;
    this.expr = expr;
  }
};
var BinaryOp = class extends CustomType {
  constructor(lhs, op, rhs) {
    super();
    this.lhs = lhs;
    this.op = op;
    this.rhs = rhs;
  }
};
var Group = class extends CustomType {
  constructor(inner) {
    super();
    this.inner = inner;
  }
};
var Add = class extends CustomType {
};
var Subtract = class extends CustomType {
};
var Multiply = class extends CustomType {
};
var Divide = class extends CustomType {
};
var Power = class extends CustomType {
};
var EqualCheck = class extends CustomType {
};
var NotEqualCheck = class extends CustomType {
};
var LessThanCheck = class extends CustomType {
};
var LessThanOrEqualCheck = class extends CustomType {
};
var GreaterThanCheck = class extends CustomType {
};
var GreaterThanOrEqualCheck = class extends CustomType {
};
var And2 = class extends CustomType {
};
var Or2 = class extends CustomType {
};
var Negate = class extends CustomType {
};
var Not = class extends CustomType {
};
var ParseError = class extends CustomType {
  constructor(context) {
    super();
    this.context = context;
  }
};
function try_parse_binary_ops(tokens) {
  if (tokens.atLeastLength(1) && tokens.head instanceof Plus) {
    let rest = tokens.tail;
    return try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new BinaryOp(_capture, new Add(), rhs);
            },
            rest$1
          ]
        );
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof Minus) {
    let rest = tokens.tail;
    return try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new BinaryOp(_capture, new Subtract(), rhs);
            },
            rest$1
          ]
        );
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof Star) {
    let rest = tokens.tail;
    return try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new BinaryOp(_capture, new Multiply(), rhs);
            },
            rest$1
          ]
        );
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof Div) {
    let rest = tokens.tail;
    return try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new BinaryOp(_capture, new Divide(), rhs);
            },
            rest$1
          ]
        );
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof StarStar) {
    let rest = tokens.tail;
    return try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new BinaryOp(_capture, new Power(), rhs);
            },
            rest$1
          ]
        );
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof BangEqual) {
    let rest = tokens.tail;
    return try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new BinaryOp(_capture, new NotEqualCheck(), rhs);
            },
            rest$1
          ]
        );
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof EqualEqual) {
    let rest = tokens.tail;
    return try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new BinaryOp(_capture, new EqualCheck(), rhs);
            },
            rest$1
          ]
        );
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof LessEqual) {
    let rest = tokens.tail;
    return try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new BinaryOp(_capture, new LessThanOrEqualCheck(), rhs);
            },
            rest$1
          ]
        );
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof Less) {
    let rest = tokens.tail;
    return try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new BinaryOp(_capture, new LessThanCheck(), rhs);
            },
            rest$1
          ]
        );
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof GreaterEqual) {
    let rest = tokens.tail;
    return try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new BinaryOp(_capture, new GreaterThanOrEqualCheck(), rhs);
            },
            rest$1
          ]
        );
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof Greater) {
    let rest = tokens.tail;
    return try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new BinaryOp(_capture, new GreaterThanCheck(), rhs);
            },
            rest$1
          ]
        );
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof And) {
    let rest = tokens.tail;
    return try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new BinaryOp(_capture, new And2(), rhs);
            },
            rest$1
          ]
        );
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof Or) {
    let rest = tokens.tail;
    return try$(
      do_parse(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [
            (_capture) => {
              return new BinaryOp(_capture, new Or2(), rhs);
            },
            rest$1
          ]
        );
      }
    );
  } else {
    return new Error(new ParseError("Not a binary operation"));
  }
}
function do_parse(tokens) {
  if (tokens.hasLength(0)) {
    return new Ok([new Empty2(), toList([])]);
  } else if (tokens.atLeastLength(1) && tokens.head instanceof StringLiteral) {
    let str = tokens.head[0];
    let rest = tokens.tail;
    let $ = try_parse_binary_ops(rest);
    if ($.isOk()) {
      let op = $[0][0];
      let rest$1 = $[0][1];
      return new Ok([op(new StringLiteral2(str)), rest$1]);
    } else {
      return new Ok([new StringLiteral2(str), rest]);
    }
  } else if (tokens.atLeastLength(1) && tokens.head instanceof IntegerLiteral) {
    let n = tokens.head.n;
    let rest = tokens.tail;
    let $ = try_parse_binary_ops(rest);
    if ($.isOk()) {
      let op = $[0][0];
      let rest$1 = $[0][1];
      return new Ok([op(new IntegerLiteral2(n)), rest$1]);
    } else {
      return new Ok([new IntegerLiteral2(n), rest]);
    }
  } else if (tokens.atLeastLength(1) && tokens.head instanceof FloatLiteral) {
    let f = tokens.head.f;
    let rest = tokens.tail;
    let $ = try_parse_binary_ops(rest);
    if ($.isOk()) {
      let op = $[0][0];
      let rest$1 = $[0][1];
      return new Ok([op(new FloatLiteral2(f)), rest$1]);
    } else {
      return new Ok([new FloatLiteral2(f), rest]);
    }
  } else if (tokens.atLeastLength(1) && tokens.head instanceof CellReference) {
    let key = tokens.head.key;
    let rest = tokens.tail;
    let $ = try_parse_binary_ops(rest);
    if ($.isOk()) {
      let op = $[0][0];
      let rest$1 = $[0][1];
      return new Ok([op(new CellReference2(key)), rest$1]);
    } else {
      return new Ok([new CellReference2(key), rest]);
    }
  } else if (tokens.atLeastLength(1) && tokens.head instanceof TrueToken) {
    let rest = tokens.tail;
    let $ = try_parse_binary_ops(rest);
    if ($.isOk()) {
      let op = $[0][0];
      let rest$1 = $[0][1];
      return new Ok([op(new BooleanLiteral(true)), rest$1]);
    } else {
      return new Ok([new BooleanLiteral(true), rest]);
    }
  } else if (tokens.atLeastLength(1) && tokens.head instanceof FalseToken) {
    let rest = tokens.tail;
    let $ = try_parse_binary_ops(rest);
    if ($.isOk()) {
      let op = $[0][0];
      let rest$1 = $[0][1];
      return new Ok([op(new BooleanLiteral(false)), rest$1]);
    } else {
      return new Ok([new BooleanLiteral(false), rest]);
    }
  } else if (tokens.atLeastLength(1) && tokens.head instanceof Minus) {
    let rest = tokens.tail;
    return try$(
      do_parse(rest),
      (_use0) => {
        let parsed_remainder = _use0[0];
        let rest$1 = _use0[1];
        return new Ok([new UnaryOp(new Negate(), parsed_remainder), rest$1]);
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof Bang) {
    let rest = tokens.tail;
    return try$(
      do_parse(rest),
      (_use0) => {
        let parsed_remainder = _use0[0];
        let rest$1 = _use0[1];
        return new Ok([new UnaryOp(new Not(), parsed_remainder), rest$1]);
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof LParen) {
    let rest = tokens.tail;
    return try$(
      do_parse(rest),
      (_use0) => {
        let body = _use0[0];
        let rest$1 = _use0[1];
        if (rest$1.atLeastLength(1) && rest$1.head instanceof RParen) {
          let rest$2 = rest$1.tail;
          return new Ok([new Group(body), rest$2]);
        } else {
          return new Error(new ParseError("missing closing parentheses"));
        }
      }
    );
  } else {
    let x = tokens.head;
    return new Error(new ParseError("Unexpected token: " + inspect2(x)));
  }
}
function parse3(tokens) {
  return try$(
    do_parse(tokens),
    (_use0) => {
      let expr = _use0[0];
      let rest = _use0[1];
      if (rest.hasLength(0)) {
        return new Ok(expr);
      } else {
        return new Error(
          new ParseError("After parsing there were leftover tokens")
        );
      }
    }
  );
}

// build/dev/javascript/form/form/lang/interpreter.mjs
var Empty3 = class extends CustomType {
};
var Text2 = class extends CustomType {
  constructor(inner) {
    super();
    this.inner = inner;
  }
};
var Integer = class extends CustomType {
  constructor(n) {
    super();
    this.n = n;
  }
};
var FloatingPointNumber = class extends CustomType {
  constructor(f) {
    super();
    this.f = f;
  }
};
var Boolean = class extends CustomType {
  constructor(b) {
    super();
    this.b = b;
  }
};
var ScanError2 = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var ParseError2 = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var RuntimeError = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
function interpret(loop$environment, loop$expr) {
  while (true) {
    let environment = loop$environment;
    let expr = loop$expr;
    if (expr instanceof Empty2) {
      return new Ok(new Empty3());
    } else if (expr instanceof Group) {
      let expr$1 = expr.inner;
      loop$environment = environment;
      loop$expr = expr$1;
    } else if (expr instanceof StringLiteral2) {
      let txt = expr.txt;
      return new Ok(new Text2(txt));
    } else if (expr instanceof BooleanLiteral) {
      let b = expr.val;
      return new Ok(new Boolean(b));
    } else if (expr instanceof IntegerLiteral2) {
      let n = expr.n;
      return new Ok(new Integer(n));
    } else if (expr instanceof FloatLiteral2) {
      let f = expr.f;
      return new Ok(new FloatingPointNumber(f));
    } else if (expr instanceof CellReference2) {
      let cell_ref = expr.key;
      return try$(
        (() => {
          let _pipe = get(environment, cell_ref);
          return replace_error(
            _pipe,
            new RuntimeError("Referemced cell without data")
          );
        })(),
        (cell_src) => {
          return interpret(environment, cell_src);
        }
      );
    } else if (expr instanceof UnaryOp) {
      let op = expr.op;
      let expr$1 = expr.expr;
      return try$(
        interpret(environment, expr$1),
        (value2) => {
          if (op instanceof Negate && value2 instanceof Integer) {
            let n = value2.n;
            return new Ok(new Integer(-n));
          } else if (op instanceof Not && value2 instanceof Boolean) {
            let b = value2.b;
            return new Ok(new Boolean(!b));
          } else {
            return new Error(
              new RuntimeError(
                "Unexpected unary operation: " + inspect2(op) + " not compatible with " + inspect2(
                  value2
                )
              )
            );
          }
        }
      );
    } else {
      let lhs = expr.lhs;
      let op = expr.op;
      let rhs = expr.rhs;
      return try$(
        interpret(environment, lhs),
        (lhs2) => {
          return try$(
            interpret(environment, rhs),
            (rhs2) => {
              if (lhs2 instanceof Integer && op instanceof Add && rhs2 instanceof Integer) {
                let a = lhs2.n;
                let b = rhs2.n;
                return new Ok(new Integer(a + b));
              } else if (lhs2 instanceof Integer && op instanceof Subtract && rhs2 instanceof Integer) {
                let a = lhs2.n;
                let b = rhs2.n;
                return new Ok(new Integer(a - b));
              } else if (lhs2 instanceof Integer && op instanceof Multiply && rhs2 instanceof Integer) {
                let a = lhs2.n;
                let b = rhs2.n;
                return new Ok(new Integer(a * b));
              } else if (lhs2 instanceof Integer && op instanceof Divide && rhs2 instanceof Integer) {
                let a = lhs2.n;
                let b = rhs2.n;
                return new Ok(new Integer(divideInt(a, b)));
              } else if (lhs2 instanceof Integer && op instanceof EqualCheck && rhs2 instanceof Integer) {
                let a = lhs2.n;
                let b = rhs2.n;
                return new Ok(new Boolean(a === b));
              } else if (lhs2 instanceof Integer && op instanceof NotEqualCheck && rhs2 instanceof Integer) {
                let a = lhs2.n;
                let b = rhs2.n;
                return new Ok(new Boolean(a !== b));
              } else if (lhs2 instanceof Integer && op instanceof GreaterThanCheck && rhs2 instanceof Integer) {
                let a = lhs2.n;
                let b = rhs2.n;
                return new Ok(new Boolean(a > b));
              } else if (lhs2 instanceof Integer && op instanceof GreaterThanOrEqualCheck && rhs2 instanceof Integer) {
                let a = lhs2.n;
                let b = rhs2.n;
                return new Ok(new Boolean(a >= b));
              } else if (lhs2 instanceof Integer && op instanceof LessThanCheck && rhs2 instanceof Integer) {
                let a = lhs2.n;
                let b = rhs2.n;
                return new Ok(new Boolean(a < b));
              } else if (lhs2 instanceof Integer && op instanceof LessThanOrEqualCheck && rhs2 instanceof Integer) {
                let a = lhs2.n;
                let b = rhs2.n;
                return new Ok(new Boolean(a <= b));
              } else if (lhs2 instanceof FloatingPointNumber && op instanceof Add && rhs2 instanceof FloatingPointNumber) {
                let a = lhs2.f;
                let b = rhs2.f;
                return new Ok(new FloatingPointNumber(a + b));
              } else if (lhs2 instanceof FloatingPointNumber && op instanceof Subtract && rhs2 instanceof FloatingPointNumber) {
                let a = lhs2.f;
                let b = rhs2.f;
                return new Ok(new FloatingPointNumber(a - b));
              } else if (lhs2 instanceof FloatingPointNumber && op instanceof Multiply && rhs2 instanceof FloatingPointNumber) {
                let a = lhs2.f;
                let b = rhs2.f;
                return new Ok(new FloatingPointNumber(a * b));
              } else if (lhs2 instanceof FloatingPointNumber && op instanceof Divide && rhs2 instanceof FloatingPointNumber) {
                let a = lhs2.f;
                let b = rhs2.f;
                return new Ok(new FloatingPointNumber(divideFloat(a, b)));
              } else if (lhs2 instanceof FloatingPointNumber && op instanceof EqualCheck && rhs2 instanceof FloatingPointNumber) {
                let a = lhs2.f;
                let b = rhs2.f;
                return new Ok(new Boolean(a === b));
              } else if (lhs2 instanceof FloatingPointNumber && op instanceof NotEqualCheck && rhs2 instanceof FloatingPointNumber) {
                let a = lhs2.f;
                let b = rhs2.f;
                return new Ok(new Boolean(a !== b));
              } else if (lhs2 instanceof FloatingPointNumber && op instanceof GreaterThanCheck && rhs2 instanceof FloatingPointNumber) {
                let a = lhs2.f;
                let b = rhs2.f;
                return new Ok(new Boolean(a > b));
              } else if (lhs2 instanceof FloatingPointNumber && op instanceof GreaterThanOrEqualCheck && rhs2 instanceof FloatingPointNumber) {
                let a = lhs2.f;
                let b = rhs2.f;
                return new Ok(new Boolean(a >= b));
              } else if (lhs2 instanceof FloatingPointNumber && op instanceof LessThanCheck && rhs2 instanceof FloatingPointNumber) {
                let a = lhs2.f;
                let b = rhs2.f;
                return new Ok(new Boolean(a < b));
              } else if (lhs2 instanceof FloatingPointNumber && op instanceof LessThanOrEqualCheck && rhs2 instanceof FloatingPointNumber) {
                let a = lhs2.f;
                let b = rhs2.f;
                return new Ok(new Boolean(a <= b));
              } else if (lhs2 instanceof Integer && op instanceof Power && rhs2 instanceof FloatingPointNumber) {
                let a = lhs2.n;
                let b = rhs2.f;
                let $ = power3(a, b);
                if (!$.isOk()) {
                  throw makeError(
                    "assignment_no_match",
                    "form/lang/interpreter",
                    104,
                    "",
                    "Assignment pattern did not match",
                    { value: $ }
                  );
                }
                let p2 = $[0];
                return new Ok(new FloatingPointNumber(p2));
              } else if (lhs2 instanceof FloatingPointNumber && op instanceof Power && rhs2 instanceof FloatingPointNumber) {
                let a = lhs2.f;
                let b = rhs2.f;
                let $ = power2(a, b);
                if (!$.isOk()) {
                  throw makeError(
                    "assignment_no_match",
                    "form/lang/interpreter",
                    108,
                    "",
                    "Assignment pattern did not match",
                    { value: $ }
                  );
                }
                let p2 = $[0];
                return new Ok(new FloatingPointNumber(p2));
              } else if (lhs2 instanceof Boolean && op instanceof And2 && rhs2 instanceof Boolean) {
                let a = lhs2.b;
                let b = rhs2.b;
                return new Ok(new Boolean(a && b));
              } else if (lhs2 instanceof Boolean && op instanceof Or2 && rhs2 instanceof Boolean) {
                let a = lhs2.b;
                let b = rhs2.b;
                return new Ok(new Boolean(a || b));
              } else if (lhs2 instanceof Boolean && op instanceof EqualCheck && rhs2 instanceof Boolean) {
                let a = lhs2.b;
                let b = rhs2.b;
                return new Ok(new Boolean(a === b));
              } else if (lhs2 instanceof Boolean && op instanceof NotEqualCheck && rhs2 instanceof Boolean) {
                let a = lhs2.b;
                let b = rhs2.b;
                return new Ok(new Boolean(a !== b));
              } else {
                return new Error(
                  new RuntimeError("Unexpected binary operation")
                );
              }
            }
          );
        }
      );
    }
  }
}

// build/dev/javascript/form/form.mjs
var Model = class extends CustomType {
  constructor(active_cell, grid, error) {
    super();
    this.active_cell = active_cell;
    this.grid = grid;
    this.error = error;
  }
};
var UserClickedCell = class extends CustomType {
  constructor(key) {
    super();
    this.key = key;
  }
};
var UserSetCellValue = class extends CustomType {
  constructor(key, val) {
    super();
    this.key = key;
    this.val = val;
  }
};
function init2(_) {
  let cols = (() => {
    let _pipe = "ABCDE";
    return graphemes(_pipe);
  })();
  let rows = toList([1, 2, 3, 4, 5]);
  let grid = fold(
    cols,
    new$(),
    (grid2, c) => {
      let _pipe = fold(
        rows,
        new$(),
        (partial_grid, r) => {
          let key = c + to_string2(r);
          let _pipe2 = partial_grid;
          return insert(_pipe2, key, new Empty2());
        }
      );
      return merge(_pipe, grid2);
    }
  );
  return [new Model("A1", grid, new None()), none()];
}
function update(model, msg) {
  if (msg instanceof UserSetCellValue) {
    let key = msg.key;
    let val = msg.val;
    let res = try$(
      (() => {
        let _pipe = scan(val);
        return map_error(
          _pipe,
          (var0) => {
            return new ScanError2(var0);
          }
        );
      })(),
      (tokens) => {
        return try$(
          (() => {
            let _pipe = parse3(tokens);
            return map_error(
              _pipe,
              (var0) => {
                return new ParseError2(var0);
              }
            );
          })(),
          (expr) => {
            return new Ok(expr);
          }
        );
      }
    );
    if (res.isOk()) {
      let val$1 = res[0];
      return [
        model.withFields({
          grid: (() => {
            let _pipe = model.grid;
            return insert(_pipe, key, val$1);
          })()
        }),
        none()
      ];
    } else {
      let e = res[0];
      return [model.withFields({ error: new Some(e) }), none()];
    }
  } else {
    let key = msg.key;
    return [model.withFields({ active_cell: key }), none()];
  }
}
function view(model) {
  let cells = (() => {
    let _pipe = keys(model.grid);
    let _pipe$1 = sort(_pipe, compare3);
    return map(
      _pipe$1,
      (c) => {
        return input(
          toList([
            on_input(
              (_capture) => {
                return new UserSetCellValue(c, _capture);
              }
            ),
            on_click(new UserClickedCell(c))
          ])
        );
      }
    );
  })();
  let active_cell_value = (() => {
    let $ = get(model.grid, model.active_cell);
    if (!$.isOk()) {
      throw makeError(
        "assignment_no_match",
        "form",
        91,
        "view",
        "Assignment pattern did not match",
        { value: $ }
      );
    }
    let expr = $[0];
    return inspect2(interpret(model.grid, expr));
  })();
  return div(
    toList([]),
    toList([
      div(toList([]), cells),
      p(
        toList([]),
        toList([text2(model.active_cell + ": " + active_cell_value)])
      )
    ])
  );
}
function main() {
  let app = application(init2, update, view);
  let $ = start3(app, "#app", void 0);
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "form",
      18,
      "main",
      "Assignment pattern did not match",
      { value: $ }
    );
  }
  return void 0;
}

// build/.lustre/entry.mjs
main();
