// build/dev/javascript/prelude.mjs
var CustomType = class {
  withFields(fields) {
    let properties = Object.keys(this).map(
      (label2) => label2 in fields ? fields[label2] : this[label2]
    );
    return new this.constructor(...properties);
  }
};
var List = class {
  static fromArray(array3, tail) {
    let t2 = tail || new Empty();
    for (let i = array3.length - 1; i >= 0; --i) {
      t2 = new NonEmpty(array3[i], t2);
    }
    return t2;
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
  // @internal
  countLength() {
    let length4 = 0;
    for (let _ of this)
      length4++;
    return length4;
  }
};
function prepend(element2, tail) {
  return new NonEmpty(element2, tail);
}
function toList(elements2, tail) {
  return List.fromArray(elements2, tail);
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
  byteAt(index3) {
    return this.buffer[index3];
  }
  // @internal
  floatFromSlice(start3, end, isBigEndian) {
    return byteArrayToFloat(this.buffer, start3, end, isBigEndian);
  }
  // @internal
  intFromSlice(start3, end, isBigEndian, isSigned) {
    return byteArrayToInt(this.buffer, start3, end, isBigEndian, isSigned);
  }
  // @internal
  binaryFromSlice(start3, end) {
    return new _BitArray(this.buffer.slice(start3, end));
  }
  // @internal
  sliceAfter(index3) {
    return new _BitArray(this.buffer.slice(index3));
  }
};
var UtfCodepoint = class {
  constructor(value3) {
    this.value = value3;
  }
};
function byteArrayToInt(byteArray, start3, end, isBigEndian, isSigned) {
  let value3 = 0;
  if (isBigEndian) {
    for (let i = start3; i < end; i++) {
      value3 = value3 * 256 + byteArray[i];
    }
  } else {
    for (let i = end - 1; i >= start3; i--) {
      value3 = value3 * 256 + byteArray[i];
    }
  }
  if (isSigned) {
    const byteSize = end - start3;
    const highBit = 2 ** (byteSize * 8 - 1);
    if (value3 >= highBit) {
      value3 -= highBit * 2;
    }
  }
  return value3;
}
function byteArrayToFloat(byteArray, start3, end, isBigEndian) {
  const view2 = new DataView(byteArray.buffer);
  const byteSize = end - start3;
  if (byteSize === 8) {
    return view2.getFloat64(start3, !isBigEndian);
  } else if (byteSize === 4) {
    return view2.getFloat32(start3, !isBigEndian);
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
  constructor(value3) {
    super();
    this[0] = value3;
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
    let [keys2, get5] = getters(a);
    for (let k of keys2(a)) {
      values.push(get5(a, k), get5(b, k));
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
  error.function = fn;
  error.fn = fn;
  for (let k in extra)
    error[k] = extra[k];
  return error;
}

// build/dev/javascript/gleam_stdlib/gleam/order.mjs
var Lt = class extends CustomType {
};
var Eq = class extends CustomType {
};
var Gt = class extends CustomType {
};

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
function from_result(result) {
  if (result.isOk()) {
    let a = result[0];
    return new Some(a);
  } else {
    return new None();
  }
}
function map(option, fun) {
  if (option instanceof Some) {
    let x = option[0];
    return new Some(fun(x));
  } else {
    return new None();
  }
}
function flatten(option) {
  if (option instanceof Some) {
    let x = option[0];
    return x;
  } else {
    return new None();
  }
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
function assoc(root, shift, hash, key, val, addedLeaf) {
  switch (root.type) {
    case ARRAY_NODE:
      return assocArray(root, shift, hash, key, val, addedLeaf);
    case INDEX_NODE:
      return assocIndex(root, shift, hash, key, val, addedLeaf);
    case COLLISION_NODE:
      return assocCollision(root, shift, hash, key, val, addedLeaf);
  }
}
function assocArray(root, shift, hash, key, val, addedLeaf) {
  const idx = mask(hash, shift);
  const node = root.array[idx];
  if (node === void 0) {
    addedLeaf.val = true;
    return {
      type: ARRAY_NODE,
      size: root.size + 1,
      array: cloneAndSet(root.array, idx, { type: ENTRY, k: key, v: val })
    };
  }
  if (node.type === ENTRY) {
    if (isEqual(key, node.k)) {
      if (val === node.v) {
        return root;
      }
      return {
        type: ARRAY_NODE,
        size: root.size,
        array: cloneAndSet(root.array, idx, {
          type: ENTRY,
          k: key,
          v: val
        })
      };
    }
    addedLeaf.val = true;
    return {
      type: ARRAY_NODE,
      size: root.size,
      array: cloneAndSet(
        root.array,
        idx,
        createNode(shift + SHIFT, node.k, node.v, hash, key, val)
      )
    };
  }
  const n = assoc(node, shift + SHIFT, hash, key, val, addedLeaf);
  if (n === node) {
    return root;
  }
  return {
    type: ARRAY_NODE,
    size: root.size,
    array: cloneAndSet(root.array, idx, n)
  };
}
function assocIndex(root, shift, hash, key, val, addedLeaf) {
  const bit = bitpos(hash, shift);
  const idx = index(root.bitmap, bit);
  if ((root.bitmap & bit) !== 0) {
    const node = root.array[idx];
    if (node.type !== ENTRY) {
      const n = assoc(node, shift + SHIFT, hash, key, val, addedLeaf);
      if (n === node) {
        return root;
      }
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap,
        array: cloneAndSet(root.array, idx, n)
      };
    }
    const nodeKey = node.k;
    if (isEqual(key, nodeKey)) {
      if (val === node.v) {
        return root;
      }
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap,
        array: cloneAndSet(root.array, idx, {
          type: ENTRY,
          k: key,
          v: val
        })
      };
    }
    addedLeaf.val = true;
    return {
      type: INDEX_NODE,
      bitmap: root.bitmap,
      array: cloneAndSet(
        root.array,
        idx,
        createNode(shift + SHIFT, nodeKey, node.v, hash, key, val)
      )
    };
  } else {
    const n = root.array.length;
    if (n >= MAX_INDEX_NODE) {
      const nodes = new Array(32);
      const jdx = mask(hash, shift);
      nodes[jdx] = assocIndex(EMPTY, shift + SHIFT, hash, key, val, addedLeaf);
      let j = 0;
      let bitmap = root.bitmap;
      for (let i = 0; i < 32; i++) {
        if ((bitmap & 1) !== 0) {
          const node = root.array[j++];
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
      const newArray2 = spliceIn(root.array, idx, {
        type: ENTRY,
        k: key,
        v: val
      });
      addedLeaf.val = true;
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap | bit,
        array: newArray2
      };
    }
  }
}
function assocCollision(root, shift, hash, key, val, addedLeaf) {
  if (hash === root.hash) {
    const idx = collisionIndexOf(root, key);
    if (idx !== -1) {
      const entry = root.array[idx];
      if (entry.v === val) {
        return root;
      }
      return {
        type: COLLISION_NODE,
        hash,
        array: cloneAndSet(root.array, idx, { type: ENTRY, k: key, v: val })
      };
    }
    const size = root.array.length;
    addedLeaf.val = true;
    return {
      type: COLLISION_NODE,
      hash,
      array: cloneAndSet(root.array, size, { type: ENTRY, k: key, v: val })
    };
  }
  return assoc(
    {
      type: INDEX_NODE,
      bitmap: bitpos(root.hash, shift),
      array: [root]
    },
    shift,
    hash,
    key,
    val,
    addedLeaf
  );
}
function collisionIndexOf(root, key) {
  const size = root.array.length;
  for (let i = 0; i < size; i++) {
    if (isEqual(key, root.array[i].k)) {
      return i;
    }
  }
  return -1;
}
function find(root, shift, hash, key) {
  switch (root.type) {
    case ARRAY_NODE:
      return findArray(root, shift, hash, key);
    case INDEX_NODE:
      return findIndex(root, shift, hash, key);
    case COLLISION_NODE:
      return findCollision(root, key);
  }
}
function findArray(root, shift, hash, key) {
  const idx = mask(hash, shift);
  const node = root.array[idx];
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
function findIndex(root, shift, hash, key) {
  const bit = bitpos(hash, shift);
  if ((root.bitmap & bit) === 0) {
    return void 0;
  }
  const idx = index(root.bitmap, bit);
  const node = root.array[idx];
  if (node.type !== ENTRY) {
    return find(node, shift + SHIFT, hash, key);
  }
  if (isEqual(key, node.k)) {
    return node;
  }
  return void 0;
}
function findCollision(root, key) {
  const idx = collisionIndexOf(root, key);
  if (idx < 0) {
    return void 0;
  }
  return root.array[idx];
}
function without(root, shift, hash, key) {
  switch (root.type) {
    case ARRAY_NODE:
      return withoutArray(root, shift, hash, key);
    case INDEX_NODE:
      return withoutIndex(root, shift, hash, key);
    case COLLISION_NODE:
      return withoutCollision(root, key);
  }
}
function withoutArray(root, shift, hash, key) {
  const idx = mask(hash, shift);
  const node = root.array[idx];
  if (node === void 0) {
    return root;
  }
  let n = void 0;
  if (node.type === ENTRY) {
    if (!isEqual(node.k, key)) {
      return root;
    }
  } else {
    n = without(node, shift + SHIFT, hash, key);
    if (n === node) {
      return root;
    }
  }
  if (n === void 0) {
    if (root.size <= MIN_ARRAY_NODE) {
      const arr = root.array;
      const out = new Array(root.size - 1);
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
      size: root.size - 1,
      array: cloneAndSet(root.array, idx, n)
    };
  }
  return {
    type: ARRAY_NODE,
    size: root.size,
    array: cloneAndSet(root.array, idx, n)
  };
}
function withoutIndex(root, shift, hash, key) {
  const bit = bitpos(hash, shift);
  if ((root.bitmap & bit) === 0) {
    return root;
  }
  const idx = index(root.bitmap, bit);
  const node = root.array[idx];
  if (node.type !== ENTRY) {
    const n = without(node, shift + SHIFT, hash, key);
    if (n === node) {
      return root;
    }
    if (n !== void 0) {
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap,
        array: cloneAndSet(root.array, idx, n)
      };
    }
    if (root.bitmap === bit) {
      return void 0;
    }
    return {
      type: INDEX_NODE,
      bitmap: root.bitmap ^ bit,
      array: spliceOut(root.array, idx)
    };
  }
  if (isEqual(key, node.k)) {
    if (root.bitmap === bit) {
      return void 0;
    }
    return {
      type: INDEX_NODE,
      bitmap: root.bitmap ^ bit,
      array: spliceOut(root.array, idx)
    };
  }
  return root;
}
function withoutCollision(root, key) {
  const idx = collisionIndexOf(root, key);
  if (idx < 0) {
    return root;
  }
  if (root.array.length === 1) {
    return void 0;
  }
  return {
    type: COLLISION_NODE,
    hash: root.hash,
    array: spliceOut(root.array, idx)
  };
}
function forEach(root, fn) {
  if (root === void 0) {
    return;
  }
  const items = root.array;
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
  constructor(root, size) {
    this.root = root;
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
    const root = this.root === void 0 ? EMPTY : this.root;
    const newRoot = assoc(root, 0, getHash(key), key, val, addedLeaf);
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
function parse_int(value3) {
  if (/^[-+]?(\d+)$/.test(value3)) {
    return new Ok(parseInt(value3));
  } else {
    return new Error(Nil);
  }
}
function parse_float(value3) {
  if (/^[-+]?(\d+)\.(\d+)([eE][-+]?\d+)?$/.test(value3)) {
    return new Ok(parseFloat(value3));
  } else {
    return new Error(Nil);
  }
}
function to_string(term) {
  return term.toString();
}
function float_to_string(float3) {
  const string3 = float3.toString().replace("+", "");
  if (string3.indexOf(".") >= 0) {
    return string3;
  } else {
    const index3 = string3.indexOf("e");
    if (index3 >= 0) {
      return string3.slice(0, index3) + ".0" + string3.slice(index3);
    } else {
      return string3 + ".0";
    }
  }
}
function string_replace(string3, target, substitute) {
  if (typeof string3.replaceAll !== "undefined") {
    return string3.replaceAll(target, substitute);
  }
  return string3.replace(
    // $& means the whole matched string
    new RegExp(target.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"), "g"),
    substitute
  );
}
function string_length(string3) {
  if (string3 === "") {
    return 0;
  }
  const iterator = graphemes_iterator(string3);
  if (iterator) {
    let i = 0;
    for (const _ of iterator) {
      i++;
    }
    return i;
  } else {
    return string3.match(/./gsu).length;
  }
}
var segmenter = void 0;
function graphemes_iterator(string3) {
  if (globalThis.Intl && Intl.Segmenter) {
    segmenter ||= new Intl.Segmenter();
    return segmenter.segment(string3)[Symbol.iterator]();
  }
}
function uppercase(string3) {
  return string3.toUpperCase();
}
function join(xs, separator) {
  const iterator = xs[Symbol.iterator]();
  let result = iterator.next().value || "";
  let current = iterator.next();
  while (!current.done) {
    result = result + separator + current.value;
    current = iterator.next();
  }
  return result;
}
function concat(xs) {
  let result = "";
  for (const x of xs) {
    result = result + x;
  }
  return result;
}
function string_slice(string3, idx, len) {
  if (len <= 0 || idx >= string3.length) {
    return "";
  }
  const iterator = graphemes_iterator(string3);
  if (iterator) {
    while (idx-- > 0) {
      iterator.next();
    }
    let result = "";
    while (len-- > 0) {
      const v = iterator.next().value;
      if (v === void 0) {
        break;
      }
      result += v.segment;
    }
    return result;
  } else {
    return string3.match(/./gsu).slice(idx, idx + len).join("");
  }
}
function contains_string(haystack, needle) {
  return haystack.indexOf(needle) >= 0;
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
].join("");
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
function print_debug(string3) {
  if (typeof process === "object" && process.stderr?.write) {
    process.stderr.write(string3 + "\n");
  } else if (typeof Deno === "object") {
    Deno.stderr.writeSync(new TextEncoder().encode(string3 + "\n"));
  } else {
    console.log(string3);
  }
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
function map_to_list(map6) {
  return List.fromArray(map6.entries());
}
function map_get(map6, key) {
  const value3 = map6.get(key, NOT_FOUND);
  if (value3 === NOT_FOUND) {
    return new Error(Nil);
  }
  return new Ok(value3);
}
function map_insert(key, value3, map6) {
  return map6.set(key, value3);
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
function decode_bool(data) {
  return typeof data === "boolean" ? new Ok(data) : decoder_error("Bool", data);
}
function decode_field(value3, name2) {
  const not_a_map_error = () => decoder_error("Dict", value3);
  if (value3 instanceof Dict || value3 instanceof WeakMap || value3 instanceof Map) {
    const entry = map_get(value3, name2);
    return new Ok(entry.isOk() ? new Some(entry[0]) : new None());
  } else if (value3 === null) {
    return not_a_map_error();
  } else if (Object.getPrototypeOf(value3) == Object.prototype) {
    return try_get_field(value3, name2, () => new Ok(new None()));
  } else {
    return try_get_field(value3, name2, not_a_map_error);
  }
}
function try_get_field(value3, field2, or_else) {
  try {
    return field2 in value3 ? new Ok(new Some(value3[field2])) : or_else();
  } catch {
    return or_else();
  }
}
function inspect(v) {
  const t2 = typeof v;
  if (v === true)
    return "True";
  if (v === false)
    return "False";
  if (v === null)
    return "//js(null)";
  if (v === void 0)
    return "Nil";
  if (t2 === "string")
    return inspectString(v);
  if (t2 === "bigint" || Number.isInteger(v))
    return v.toString();
  if (t2 === "number")
    return float_to_string(v);
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
function inspectDict(map6) {
  let body = "dict.from_list([";
  let first2 = true;
  map6.forEach((value3, key) => {
    if (!first2)
      body = body + ", ";
    body = body + "#(" + inspect(key) + ", " + inspect(value3) + ")";
    first2 = false;
  });
  return body + "])";
}
function inspectObject(v) {
  const name2 = Object.getPrototypeOf(v)?.constructor?.name || "Object";
  const props = [];
  for (const k of Object.keys(v)) {
    props.push(`${inspect(k)}: ${inspect(v[k])}`);
  }
  const body = props.length ? " " + props.join(", ") + " " : "";
  const head = name2 === "Object" ? "" : name2 + " ";
  return `//js(${head}{${body}})`;
}
function inspectCustomType(record) {
  const props = Object.keys(record).map((label2) => {
    const value3 = inspect(record[label2]);
    return isNaN(parseInt(label2)) ? `${label2}: ${value3}` : value3;
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

// build/dev/javascript/gleam_stdlib/gleam/float.mjs
function parse(string3) {
  return parse_float(string3);
}
function to_string2(x) {
  return float_to_string(x);
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
function negate(x) {
  return -1 * x;
}
function do_sum(loop$numbers, loop$initial) {
  while (true) {
    let numbers = loop$numbers;
    let initial = loop$initial;
    if (numbers.hasLength(0)) {
      return initial;
    } else {
      let x = numbers.head;
      let rest = numbers.tail;
      loop$numbers = rest;
      loop$initial = x + initial;
    }
  }
}
function sum(numbers) {
  let _pipe = numbers;
  return do_sum(_pipe, 0);
}

// build/dev/javascript/gleam_stdlib/gleam/int.mjs
function parse2(string3) {
  return parse_int(string3);
}
function to_string3(x) {
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
function compare(a, b) {
  let $ = a === b;
  if ($) {
    return new Eq();
  } else {
    let $1 = a < b;
    if ($1) {
      return new Lt();
    } else {
      return new Gt();
    }
  }
}
function do_sum2(loop$numbers, loop$initial) {
  while (true) {
    let numbers = loop$numbers;
    let initial = loop$initial;
    if (numbers.hasLength(0)) {
      return initial;
    } else {
      let x = numbers.head;
      let rest = numbers.tail;
      loop$numbers = rest;
      loop$initial = x + initial;
    }
  }
}
function sum2(numbers) {
  let _pipe = numbers;
  return do_sum2(_pipe, 0);
}

// build/dev/javascript/gleam_stdlib/gleam/dict.mjs
function new$() {
  return new_map();
}
function get(from2, get5) {
  return map_get(from2, get5);
}
function insert(dict, key, value3) {
  return map_insert(key, value3, dict);
}
function fold_list_of_pair(loop$list, loop$initial) {
  while (true) {
    let list = loop$list;
    let initial = loop$initial;
    if (list.hasLength(0)) {
      return initial;
    } else {
      let x = list.head;
      let rest = list.tail;
      loop$list = rest;
      loop$initial = insert(initial, x[0], x[1]);
    }
  }
}
function from_list(list) {
  return fold_list_of_pair(list, new$());
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
      let first2 = list.head;
      let rest = list.tail;
      loop$list = rest;
      loop$acc = prepend(first2[0], acc);
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
      let first2 = new_entries.head;
      let rest = new_entries.tail;
      loop$new_entries = rest;
      loop$dict = insert_pair(dict, first2);
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
function do_fold(loop$list, loop$initial, loop$fun) {
  while (true) {
    let list = loop$list;
    let initial = loop$initial;
    let fun = loop$fun;
    if (list.hasLength(0)) {
      return initial;
    } else {
      let k = list.head[0];
      let v = list.head[1];
      let rest = list.tail;
      loop$list = rest;
      loop$initial = fun(initial, k, v);
      loop$fun = fun;
    }
  }
}
function fold(dict, initial, fun) {
  let _pipe = dict;
  let _pipe$1 = map_to_list(_pipe);
  return do_fold(_pipe$1, initial, fun);
}
function do_map_values(f, dict) {
  let f$1 = (dict2, k, v) => {
    return insert(dict2, k, f(k, v));
  };
  return fold(dict, new$(), f$1);
}
function map_values(dict, fun) {
  return do_map_values(fun, dict);
}

// build/dev/javascript/gleam_stdlib/gleam/list.mjs
var Continue = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Stop = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
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
function reverse(list) {
  return do_reverse(list, toList([]));
}
function contains(loop$list, loop$elem) {
  while (true) {
    let list = loop$list;
    let elem = loop$elem;
    if (list.hasLength(0)) {
      return false;
    } else if (list.atLeastLength(1) && isEqual(list.head, elem)) {
      let first$1 = list.head;
      return true;
    } else {
      let rest$1 = list.tail;
      loop$list = rest$1;
      loop$elem = elem;
    }
  }
}
function update_group(f) {
  return (groups, elem) => {
    let $ = get(groups, f(elem));
    if ($.isOk()) {
      let existing = $[0];
      return insert(groups, f(elem), prepend(elem, existing));
    } else {
      return insert(groups, f(elem), toList([elem]));
    }
  };
}
function do_filter(loop$list, loop$fun, loop$acc) {
  while (true) {
    let list = loop$list;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse(acc);
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let new_acc = (() => {
        let $ = fun(first$1);
        if ($) {
          return prepend(first$1, acc);
        } else {
          return acc;
        }
      })();
      loop$list = rest$1;
      loop$fun = fun;
      loop$acc = new_acc;
    }
  }
}
function filter(list, predicate) {
  return do_filter(list, predicate, toList([]));
}
function do_filter_map(loop$list, loop$fun, loop$acc) {
  while (true) {
    let list = loop$list;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse(acc);
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let new_acc = (() => {
        let $ = fun(first$1);
        if ($.isOk()) {
          let first$2 = $[0];
          return prepend(first$2, acc);
        } else {
          return acc;
        }
      })();
      loop$list = rest$1;
      loop$fun = fun;
      loop$acc = new_acc;
    }
  }
}
function filter_map(list, fun) {
  return do_filter_map(list, fun, toList([]));
}
function do_map(loop$list, loop$fun, loop$acc) {
  while (true) {
    let list = loop$list;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse(acc);
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      loop$list = rest$1;
      loop$fun = fun;
      loop$acc = prepend(fun(first$1), acc);
    }
  }
}
function map2(list, fun) {
  return do_map(list, fun, toList([]));
}
function do_index_map(loop$list, loop$fun, loop$index, loop$acc) {
  while (true) {
    let list = loop$list;
    let fun = loop$fun;
    let index3 = loop$index;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse(acc);
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let acc$1 = prepend(fun(first$1, index3), acc);
      loop$list = rest$1;
      loop$fun = fun;
      loop$index = index3 + 1;
      loop$acc = acc$1;
    }
  }
}
function index_map(list, fun) {
  return do_index_map(list, fun, 0, toList([]));
}
function do_take(loop$list, loop$n, loop$acc) {
  while (true) {
    let list = loop$list;
    let n = loop$n;
    let acc = loop$acc;
    let $ = n <= 0;
    if ($) {
      return reverse(acc);
    } else {
      if (list.hasLength(0)) {
        return reverse(acc);
      } else {
        let first$1 = list.head;
        let rest$1 = list.tail;
        loop$list = rest$1;
        loop$n = n - 1;
        loop$acc = prepend(first$1, acc);
      }
    }
  }
}
function take(list, n) {
  return do_take(list, n, toList([]));
}
function reverse_and_prepend(loop$prefix, loop$suffix) {
  while (true) {
    let prefix = loop$prefix;
    let suffix = loop$suffix;
    if (prefix.hasLength(0)) {
      return suffix;
    } else {
      let first$1 = prefix.head;
      let rest$1 = prefix.tail;
      loop$prefix = rest$1;
      loop$suffix = prepend(first$1, suffix);
    }
  }
}
function do_concat(loop$lists, loop$acc) {
  while (true) {
    let lists = loop$lists;
    let acc = loop$acc;
    if (lists.hasLength(0)) {
      return reverse(acc);
    } else {
      let list = lists.head;
      let further_lists = lists.tail;
      loop$lists = further_lists;
      loop$acc = reverse_and_prepend(list, acc);
    }
  }
}
function flatten2(lists) {
  return do_concat(lists, toList([]));
}
function fold2(loop$list, loop$initial, loop$fun) {
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
function group(list, key) {
  return fold2(list, new$(), update_group(key));
}
function do_index_fold(loop$over, loop$acc, loop$with, loop$index) {
  while (true) {
    let over = loop$over;
    let acc = loop$acc;
    let with$ = loop$with;
    let index3 = loop$index;
    if (over.hasLength(0)) {
      return acc;
    } else {
      let first$1 = over.head;
      let rest$1 = over.tail;
      loop$over = rest$1;
      loop$acc = with$(acc, first$1, index3);
      loop$with = with$;
      loop$index = index3 + 1;
    }
  }
}
function index_fold(list, initial, fun) {
  return do_index_fold(list, initial, fun, 0);
}
function fold_until(loop$list, loop$initial, loop$fun) {
  while (true) {
    let list = loop$list;
    let initial = loop$initial;
    let fun = loop$fun;
    if (list.hasLength(0)) {
      return initial;
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let $ = fun(initial, first$1);
      if ($ instanceof Continue) {
        let next_accumulator = $[0];
        loop$list = rest$1;
        loop$initial = next_accumulator;
        loop$fun = fun;
      } else {
        let b = $[0];
        return b;
      }
    }
  }
}
function find2(loop$list, loop$is_desired) {
  while (true) {
    let list = loop$list;
    let is_desired = loop$is_desired;
    if (list.hasLength(0)) {
      return new Error(void 0);
    } else {
      let x = list.head;
      let rest$1 = list.tail;
      let $ = is_desired(x);
      if ($) {
        return new Ok(x);
      } else {
        loop$list = rest$1;
        loop$is_desired = is_desired;
      }
    }
  }
}
function find_map(loop$list, loop$fun) {
  while (true) {
    let list = loop$list;
    let fun = loop$fun;
    if (list.hasLength(0)) {
      return new Error(void 0);
    } else {
      let x = list.head;
      let rest$1 = list.tail;
      let $ = fun(x);
      if ($.isOk()) {
        let x$1 = $[0];
        return new Ok(x$1);
      } else {
        loop$list = rest$1;
        loop$fun = fun;
      }
    }
  }
}
function any(loop$list, loop$predicate) {
  while (true) {
    let list = loop$list;
    let predicate = loop$predicate;
    if (list.hasLength(0)) {
      return false;
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let $ = predicate(first$1);
      if ($) {
        return true;
      } else {
        loop$list = rest$1;
        loop$predicate = predicate;
      }
    }
  }
}
function unique(list) {
  if (list.hasLength(0)) {
    return toList([]);
  } else {
    let x = list.head;
    let rest$1 = list.tail;
    return prepend(
      x,
      unique(filter(rest$1, (y) => {
        return !isEqual(y, x);
      }))
    );
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
function tail_recursive_range(loop$start, loop$stop, loop$acc) {
  while (true) {
    let start3 = loop$start;
    let stop = loop$stop;
    let acc = loop$acc;
    let $ = compare(start3, stop);
    if ($ instanceof Eq) {
      return prepend(stop, acc);
    } else if ($ instanceof Gt) {
      loop$start = start3;
      loop$stop = stop + 1;
      loop$acc = prepend(stop, acc);
    } else {
      loop$start = start3;
      loop$stop = stop - 1;
      loop$acc = prepend(stop, acc);
    }
  }
}
function range(start3, stop) {
  return tail_recursive_range(start3, stop, toList([]));
}
function do_take_while(loop$list, loop$predicate, loop$acc) {
  while (true) {
    let list = loop$list;
    let predicate = loop$predicate;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse(acc);
    } else {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let $ = predicate(first$1);
      if ($) {
        loop$list = rest$1;
        loop$predicate = predicate;
        loop$acc = prepend(first$1, acc);
      } else {
        return reverse(acc);
      }
    }
  }
}
function take_while(list, predicate) {
  return do_take_while(list, predicate, toList([]));
}
function do_chunk(loop$list, loop$f, loop$previous_key, loop$current_chunk, loop$acc) {
  while (true) {
    let list = loop$list;
    let f = loop$f;
    let previous_key = loop$previous_key;
    let current_chunk = loop$current_chunk;
    let acc = loop$acc;
    if (list.atLeastLength(1)) {
      let first$1 = list.head;
      let rest$1 = list.tail;
      let key = f(first$1);
      let $ = isEqual(key, previous_key);
      if (!$) {
        let new_acc = prepend(reverse(current_chunk), acc);
        loop$list = rest$1;
        loop$f = f;
        loop$previous_key = key;
        loop$current_chunk = toList([first$1]);
        loop$acc = new_acc;
      } else {
        loop$list = rest$1;
        loop$f = f;
        loop$previous_key = key;
        loop$current_chunk = prepend(first$1, current_chunk);
        loop$acc = acc;
      }
    } else {
      return reverse(prepend(reverse(current_chunk), acc));
    }
  }
}
function chunk(list, f) {
  if (list.hasLength(0)) {
    return toList([]);
  } else {
    let first$1 = list.head;
    let rest$1 = list.tail;
    return do_chunk(rest$1, f, f(first$1), toList([first$1]), toList([]));
  }
}

// build/dev/javascript/gleam_stdlib/gleam/string_builder.mjs
function from_strings(strings) {
  return concat(strings);
}
function from_string(string3) {
  return identity(string3);
}
function to_string4(builder) {
  return identity(builder);
}

// build/dev/javascript/gleam_stdlib/gleam/string.mjs
function length2(string3) {
  return string_length(string3);
}
function replace(string3, pattern, substitute) {
  let _pipe = string3;
  let _pipe$1 = from_string(_pipe);
  let _pipe$2 = string_replace(_pipe$1, pattern, substitute);
  return to_string4(_pipe$2);
}
function uppercase2(string3) {
  return uppercase(string3);
}
function slice(string3, idx, len) {
  let $ = len < 0;
  if ($) {
    return "";
  } else {
    let $1 = idx < 0;
    if ($1) {
      let translated_idx = length2(string3) + idx;
      let $2 = translated_idx < 0;
      if ($2) {
        return "";
      } else {
        return string_slice(string3, translated_idx, len);
      }
    } else {
      return string_slice(string3, idx, len);
    }
  }
}
function drop_left(string3, num_graphemes) {
  let $ = num_graphemes < 0;
  if ($) {
    return string3;
  } else {
    return slice(string3, num_graphemes, length2(string3) - num_graphemes);
  }
}
function join2(strings, separator) {
  return join(strings, separator);
}
function trim2(string3) {
  return trim(string3);
}
function trim_left2(string3) {
  return trim_left(string3);
}
function inspect2(term) {
  let _pipe = inspect(term);
  return to_string4(_pipe);
}

// build/dev/javascript/gleam_stdlib/gleam/result.mjs
function is_error(result) {
  if (result.isOk()) {
    return false;
  } else {
    return true;
  }
}
function map3(result, fun) {
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
function unwrap(result, default$) {
  if (result.isOk()) {
    let v = result[0];
    return v;
  } else {
    return default$;
  }
}
function nil_error(result) {
  return map_error(result, (_) => {
    return void 0;
  });
}
function replace_error(result, error) {
  if (result.isOk()) {
    let x = result[0];
    return new Ok(x);
  } else {
    return new Error(error);
  }
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
function classify(data) {
  return classify_dynamic(data);
}
function int(data) {
  return decode_int(data);
}
function bool(data) {
  return decode_bool(data);
}
function any2(decoders) {
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
        return any2(decoders$1)(data);
      }
    }
  };
}
function push_path(error, name2) {
  let name$1 = identity(name2);
  let decoder = any2(
    toList([string, (x) => {
      return map3(int(x), to_string3);
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
      return to_string4(_pipe$1);
    }
  })();
  return error.withFields({ path: prepend(name$2, error.path) });
}
function map_errors(result, f) {
  return map_error(
    result,
    (_capture) => {
      return map2(_capture, f);
    }
  );
}
function string(data) {
  return decode_string(data);
}
function field(name2, inner_type) {
  return (value3) => {
    let missing_field_error = new DecodeError("field", "nothing", toList([]));
    return try$(
      decode_field(value3, name2),
      (maybe_inner) => {
        let _pipe = maybe_inner;
        let _pipe$1 = to_result(_pipe, toList([missing_field_error]));
        let _pipe$2 = try$(_pipe$1, inner_type);
        return map_errors(
          _pipe$2,
          (_capture) => {
            return push_path(_capture, name2);
          }
        );
      }
    );
  };
}

// build/dev/javascript/gleam_javascript/gleam_javascript_ffi.mjs
var PromiseLayer = class _PromiseLayer {
  constructor(promise) {
    this.promise = promise;
  }
  static wrap(value3) {
    return value3 instanceof Promise ? new _PromiseLayer(value3) : value3;
  }
  static unwrap(value3) {
    return value3 instanceof _PromiseLayer ? value3.promise : value3;
  }
};
function newPromise(executor) {
  return new Promise(
    (resolve2) => executor((value3) => {
      resolve2(PromiseLayer.wrap(value3));
    })
  );
}
function then_await(promise, fn) {
  return promise.then((value3) => fn(PromiseLayer.unwrap(value3)));
}

// build/dev/javascript/gleam_stdlib/gleam/bool.mjs
function to_string5(bool3) {
  if (!bool3) {
    return "False";
  } else {
    return "True";
  }
}
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
function custom(run) {
  return new Effect(
    toList([
      (actions) => {
        return run(actions.dispatch, actions.emit, actions.select, actions.root);
      }
    ])
  );
}
function from(effect) {
  return custom((dispatch, _, _1, _2) => {
    return effect(dispatch);
  });
}
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
  constructor(key, namespace, tag, attrs, children2, self_closing, void$) {
    super();
    this.key = key;
    this.namespace = namespace;
    this.tag = tag;
    this.attrs = attrs;
    this.children = children2;
    this.self_closing = self_closing;
    this.void = void$;
  }
};
var Map2 = class extends CustomType {
  constructor(subtree) {
    super();
    this.subtree = subtree;
  }
};
var Attribute = class extends CustomType {
  constructor(x0, x1, as_property) {
    super();
    this[0] = x0;
    this[1] = x1;
    this.as_property = as_property;
  }
};
var Event = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
function attribute_to_event_handler(attribute2) {
  if (attribute2 instanceof Attribute) {
    return new Error(void 0);
  } else {
    let name2 = attribute2[0];
    let handler = attribute2[1];
    let name$1 = drop_left(name2, 2);
    return new Ok([name$1, handler]);
  }
}
function do_element_list_handlers(elements2, handlers2, key) {
  return index_fold(
    elements2,
    handlers2,
    (handlers3, element2, index3) => {
      let key$1 = key + "-" + to_string3(index3);
      return do_handlers(element2, handlers3, key$1);
    }
  );
}
function do_handlers(loop$element, loop$handlers, loop$key) {
  while (true) {
    let element2 = loop$element;
    let handlers2 = loop$handlers;
    let key = loop$key;
    if (element2 instanceof Text) {
      return handlers2;
    } else if (element2 instanceof Map2) {
      let subtree = element2.subtree;
      loop$element = subtree();
      loop$handlers = handlers2;
      loop$key = key;
    } else {
      let attrs = element2.attrs;
      let children2 = element2.children;
      let handlers$1 = fold2(
        attrs,
        handlers2,
        (handlers3, attr) => {
          let $ = attribute_to_event_handler(attr);
          if ($.isOk()) {
            let name2 = $[0][0];
            let handler = $[0][1];
            return insert(handlers3, key + "-" + name2, handler);
          } else {
            return handlers3;
          }
        }
      );
      return do_element_list_handlers(children2, handlers$1, key);
    }
  }
}
function handlers(element2) {
  return do_handlers(element2, new$(), "0");
}

// build/dev/javascript/lustre/lustre/attribute.mjs
function attribute(name2, value3) {
  return new Attribute(name2, identity(value3), false);
}
function on(name2, handler) {
  return new Event("on" + name2, handler);
}
function style(properties) {
  return attribute(
    "style",
    fold2(
      properties,
      "",
      (styles, _use1) => {
        let name$1 = _use1[0];
        let value$1 = _use1[1];
        return styles + name$1 + ":" + value$1 + ";";
      }
    )
  );
}
function class$(name2) {
  return attribute("class", name2);
}
function none2() {
  return class$("");
}
function id(name2) {
  return attribute("id", name2);
}
function type_(name2) {
  return attribute("type", name2);
}
function value(val) {
  return attribute("value", val);
}
function name(name2) {
  return attribute("name", name2);
}
function for$(id2) {
  return attribute("for", id2);
}

// build/dev/javascript/lustre/lustre/element.mjs
function element(tag, attrs, children2) {
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
    return new Element("", "", tag, attrs, children2, false, false);
  }
}
function text(content) {
  return new Text(content);
}

// build/dev/javascript/gleam_stdlib/gleam/set.mjs
var Set2 = class extends CustomType {
  constructor(dict) {
    super();
    this.dict = dict;
  }
};
function new$3() {
  return new Set2(new$());
}

// build/dev/javascript/lustre/lustre/internals/patch.mjs
var Diff = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Emit = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
var Init = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
function is_empty_element_diff(diff2) {
  return isEqual(diff2.created, new$()) && isEqual(
    diff2.removed,
    new$3()
  ) && isEqual(diff2.updated, new$());
}

// build/dev/javascript/lustre/lustre/internals/runtime.mjs
var Attrs = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Batch = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
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
var Emit2 = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
var Event2 = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
var Shutdown = class extends CustomType {
};
var Subscribe = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
var Unsubscribe = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var ForceModel = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};

// build/dev/javascript/lustre/vdom.ffi.mjs
if (window && window.customElements) {
  window.customElements.define(
    "lustre-fragment",
    class LustreFragment extends HTMLElement {
      constructor() {
        super();
      }
    }
  );
}
function morph(prev, next, dispatch) {
  let out;
  let stack = [{ prev, next, parent: prev.parentNode }];
  while (stack.length) {
    let { prev: prev2, next: next2, parent } = stack.pop();
    while (next2.subtree !== void 0)
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
        stack
      });
      if (!prev2) {
        parent.appendChild(created);
      } else if (prev2 !== created) {
        parent.replaceChild(created, prev2);
      }
      out ??= created;
    }
  }
  return out;
}
function createElementNode({ prev, next, dispatch, stack }) {
  const namespace = next.namespace || "http://www.w3.org/1999/xhtml";
  const canMorph = prev && prev.nodeType === Node.ELEMENT_NODE && prev.localName === next.tag && prev.namespaceURI === (next.namespace || "http://www.w3.org/1999/xhtml");
  const el = canMorph ? prev : namespace ? document.createElementNS(namespace, next.tag) : document.createElement(next.tag);
  let handlersForEl;
  if (!registeredHandlers.has(el)) {
    const emptyHandlers = /* @__PURE__ */ new Map();
    registeredHandlers.set(el, emptyHandlers);
    handlersForEl = emptyHandlers;
  } else {
    handlersForEl = registeredHandlers.get(el);
  }
  const prevHandlers = canMorph ? new Set(handlersForEl.keys()) : null;
  const prevAttributes = canMorph ? new Set(Array.from(prev.attributes, (a) => a.name)) : null;
  let className = null;
  let style2 = null;
  let innerHTML = null;
  if (canMorph && next.tag === "textarea") {
    const innertText = next.children[Symbol.iterator]().next().value?.content;
    if (innertText !== void 0)
      el.value = innertText;
  }
  const delegated = [];
  for (const attr of next.attrs) {
    const name2 = attr[0];
    const value3 = attr[1];
    if (attr.as_property) {
      if (el[name2] !== value3)
        el[name2] = value3;
      if (canMorph)
        prevAttributes.delete(name2);
    } else if (name2.startsWith("on")) {
      const eventName = name2.slice(2);
      const callback = dispatch(value3, eventName === "input");
      if (!handlersForEl.has(eventName)) {
        el.addEventListener(eventName, lustreGenericEventHandler);
      }
      handlersForEl.set(eventName, callback);
      if (canMorph)
        prevHandlers.delete(eventName);
    } else if (name2.startsWith("data-lustre-on-")) {
      const eventName = name2.slice(15);
      const callback = dispatch(lustreServerEventHandler);
      if (!handlersForEl.has(eventName)) {
        el.addEventListener(eventName, lustreGenericEventHandler);
      }
      handlersForEl.set(eventName, callback);
      el.setAttribute(name2, value3);
    } else if (name2.startsWith("delegate:data-") || name2.startsWith("delegate:aria-")) {
      el.setAttribute(name2, value3);
      delegated.push([name2.slice(10), value3]);
    } else if (name2 === "class") {
      className = className === null ? value3 : className + " " + value3;
    } else if (name2 === "style") {
      style2 = style2 === null ? value3 : style2 + value3;
    } else if (name2 === "dangerous-unescaped-html") {
      innerHTML = value3;
    } else {
      if (el.getAttribute(name2) !== value3)
        el.setAttribute(name2, value3);
      if (name2 === "value" || name2 === "selected")
        el[name2] = value3;
      if (canMorph)
        prevAttributes.delete(name2);
    }
  }
  if (className !== null) {
    el.setAttribute("class", className);
    if (canMorph)
      prevAttributes.delete("class");
  }
  if (style2 !== null) {
    el.setAttribute("style", style2);
    if (canMorph)
      prevAttributes.delete("style");
  }
  if (canMorph) {
    for (const attr of prevAttributes) {
      el.removeAttribute(attr);
    }
    for (const eventName of prevHandlers) {
      handlersForEl.delete(eventName);
      el.removeEventListener(eventName, lustreGenericEventHandler);
    }
  }
  if (next.tag === "slot") {
    window.queueMicrotask(() => {
      for (const child of el.assignedElements()) {
        for (const [name2, value3] of delegated) {
          if (!child.hasAttribute(name2)) {
            child.setAttribute(name2, value3);
          }
        }
      }
    });
  }
  if (next.key !== void 0 && next.key !== "") {
    el.setAttribute("data-lustre-key", next.key);
  } else if (innerHTML !== null) {
    el.innerHTML = innerHTML;
    return el;
  }
  let prevChild = el.firstChild;
  let seenKeys = null;
  let keyedChildren = null;
  let incomingKeyedChildren = null;
  let firstChild = children(next).next().value;
  if (canMorph && firstChild !== void 0 && // Explicit checks are more verbose but truthy checks force a bunch of comparisons
  // we don't care about: it's never gonna be a number etc.
  firstChild.key !== void 0 && firstChild.key !== "") {
    seenKeys = /* @__PURE__ */ new Set();
    keyedChildren = getKeyedChildren(prev);
    incomingKeyedChildren = getKeyedChildren(next);
    for (const child of children(next)) {
      prevChild = diffKeyedChild(
        prevChild,
        child,
        el,
        stack,
        incomingKeyedChildren,
        keyedChildren,
        seenKeys
      );
    }
  } else {
    for (const child of children(next)) {
      stack.unshift({ prev: prevChild, next: child, parent: el });
      prevChild = prevChild?.nextSibling;
    }
  }
  while (prevChild) {
    const next2 = prevChild.nextSibling;
    el.removeChild(prevChild);
    prevChild = next2;
  }
  return el;
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
  const el = event2.currentTarget;
  const tag = el.getAttribute(`data-lustre-on-${event2.type}`);
  const data = JSON.parse(el.getAttribute("data-lustre-data") || "{}");
  const include = JSON.parse(el.getAttribute("data-lustre-include") || "[]");
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
function getKeyedChildren(el) {
  const keyedChildren = /* @__PURE__ */ new Map();
  if (el) {
    for (const child of children(el)) {
      const key = child?.key || child?.getAttribute?.("data-lustre-key");
      if (key)
        keyedChildren.set(key, child);
    }
  }
  return keyedChildren;
}
function diffKeyedChild(prevChild, child, el, stack, incomingKeyedChildren, keyedChildren, seenKeys) {
  while (prevChild && !incomingKeyedChildren.has(prevChild.getAttribute("data-lustre-key"))) {
    const nextChild = prevChild.nextSibling;
    el.removeChild(prevChild);
    prevChild = nextChild;
  }
  if (keyedChildren.size === 0) {
    stack.unshift({ prev: prevChild, next: child, parent: el });
    prevChild = prevChild?.nextSibling;
    return prevChild;
  }
  if (seenKeys.has(child.key)) {
    console.warn(`Duplicate key found in Lustre vnode: ${child.key}`);
    stack.unshift({ prev: null, next: child, parent: el });
    return prevChild;
  }
  seenKeys.add(child.key);
  const keyedChild = keyedChildren.get(child.key);
  if (!keyedChild && !prevChild) {
    stack.unshift({ prev: null, next: child, parent: el });
    return prevChild;
  }
  if (!keyedChild && prevChild !== null) {
    const placeholder = document.createTextNode("");
    el.insertBefore(placeholder, prevChild);
    stack.unshift({ prev: placeholder, next: child, parent: el });
    return prevChild;
  }
  if (!keyedChild || keyedChild === prevChild) {
    stack.unshift({ prev: prevChild, next: child, parent: el });
    prevChild = prevChild?.nextSibling;
    return prevChild;
  }
  el.insertBefore(keyedChild, prevChild);
  stack.unshift({ prev: keyedChild, next: child, parent: el });
  return prevChild;
}
function* children(element2) {
  for (const child of element2.children) {
    yield* forceChild(child);
  }
}
function* forceChild(element2) {
  if (element2.subtree !== void 0) {
    yield* forceChild(element2.subtree());
  } else {
    yield element2;
  }
}

// build/dev/javascript/lustre/lustre.ffi.mjs
var LustreClientApplication = class _LustreClientApplication {
  /**
   * @template Flags
   *
   * @param {object} app
   * @param {(flags: Flags) => [Model, Lustre.Effect<Msg>]} app.init
   * @param {(msg: Msg, model: Model) => [Model, Lustre.Effect<Msg>]} app.update
   * @param {(model: Model) => Lustre.Element<Msg>} app.view
   * @param {string | HTMLElement} selector
   * @param {Flags} flags
   *
   * @returns {Gleam.Ok<(action: Lustre.Action<Lustre.Client, Msg>>) => void>}
   */
  static start({ init: init3, update: update2, view: view2 }, selector, flags) {
    if (!is_browser())
      return new Error(new NotABrowser());
    const root = selector instanceof HTMLElement ? selector : document.querySelector(selector);
    if (!root)
      return new Error(new ElementNotFound(selector));
    const app = new _LustreClientApplication(root, init3(flags), update2, view2);
    return new Ok((action) => app.send(action));
  }
  /**
   * @param {Element} root
   * @param {[Model, Lustre.Effect<Msg>]} init
   * @param {(model: Model, msg: Msg) => [Model, Lustre.Effect<Msg>]} update
   * @param {(model: Model) => Lustre.Element<Msg>} view
   *
   * @returns {LustreClientApplication}
   */
  constructor(root, [init3, effects], update2, view2) {
    this.root = root;
    this.#model = init3;
    this.#update = update2;
    this.#view = view2;
    this.#tickScheduled = window.requestAnimationFrame(
      () => this.#tick(effects.all.toArray(), true)
    );
  }
  /** @type {Element} */
  root;
  /**
   * @param {Lustre.Action<Lustre.Client, Msg>} action
   *
   * @returns {void}
   */
  send(action) {
    if (action instanceof Debug) {
      if (action[0] instanceof ForceModel) {
        this.#tickScheduled = window.cancelAnimationFrame(this.#tickScheduled);
        this.#queue = [];
        this.#model = action[0][0];
        const vdom = this.#view(this.#model);
        const dispatch = (handler, immediate = false) => (event2) => {
          const result = handler(event2);
          if (result instanceof Ok) {
            this.send(new Dispatch(result[0], immediate));
          }
        };
        const prev = this.root.firstChild ?? this.root.appendChild(document.createTextNode(""));
        morph(prev, vdom, dispatch);
      }
    } else if (action instanceof Dispatch) {
      const msg = action[0];
      const immediate = action[1] ?? false;
      this.#queue.push(msg);
      if (immediate) {
        this.#tickScheduled = window.cancelAnimationFrame(this.#tickScheduled);
        this.#tick();
      } else if (!this.#tickScheduled) {
        this.#tickScheduled = window.requestAnimationFrame(() => this.#tick());
      }
    } else if (action instanceof Emit2) {
      const event2 = action[0];
      const data = action[1];
      this.root.dispatchEvent(
        new CustomEvent(event2, {
          detail: data,
          bubbles: true,
          composed: true
        })
      );
    } else if (action instanceof Shutdown) {
      this.#tickScheduled = window.cancelAnimationFrame(this.#tickScheduled);
      this.#model = null;
      this.#update = null;
      this.#view = null;
      this.#queue = null;
      while (this.root.firstChild) {
        this.root.firstChild.remove();
      }
    }
  }
  /** @type {Model} */
  #model;
  /** @type {(model: Model, msg: Msg) => [Model, Lustre.Effect<Msg>]} */
  #update;
  /** @type {(model: Model) => Lustre.Element<Msg>} */
  #view;
  /** @type {Array<Msg>} */
  #queue = [];
  /** @type {number | undefined} */
  #tickScheduled;
  /**
   * @param {Lustre.Effect<Msg>[]} effects
   * @param {boolean} isFirstRender
   */
  #tick(effects = [], isFirstRender = false) {
    this.#tickScheduled = void 0;
    if (!this.#flush(effects, isFirstRender))
      return;
    const vdom = this.#view(this.#model);
    const dispatch = (handler, immediate = false) => (event2) => {
      const result = handler(event2);
      if (result instanceof Ok) {
        this.send(new Dispatch(result[0], immediate));
      }
    };
    const prev = this.root.firstChild ?? this.root.appendChild(document.createTextNode(""));
    morph(prev, vdom, dispatch);
  }
  #flush(effects = [], didUpdate = false) {
    while (this.#queue.length > 0) {
      const msg = this.#queue.shift();
      const [next, effect] = this.#update(this.#model, msg);
      didUpdate ||= this.#model !== next;
      effects = effects.concat(effect.all.toArray());
      this.#model = next;
    }
    while (effects.length > 0) {
      const effect = effects.shift();
      const dispatch = (msg) => this.send(new Dispatch(msg));
      const emit2 = (event2, data) => this.root.dispatchEvent(
        new CustomEvent(event2, {
          detail: data,
          bubbles: true,
          composed: true
        })
      );
      const select = () => {
      };
      const root = this.root;
      effect({ dispatch, emit: emit2, select, root });
    }
    if (this.#queue.length > 0) {
      return this.#flush(effects, didUpdate);
    } else {
      return didUpdate;
    }
  }
};
var start = LustreClientApplication.start;
var LustreServerApplication = class _LustreServerApplication {
  static start({ init: init3, update: update2, view: view2, on_attribute_change }, flags) {
    const app = new _LustreServerApplication(
      init3(flags),
      update2,
      view2,
      on_attribute_change
    );
    return new Ok((action) => app.send(action));
  }
  constructor([model, effects], update2, view2, on_attribute_change) {
    this.#model = model;
    this.#update = update2;
    this.#view = view2;
    this.#html = view2(model);
    this.#onAttributeChange = on_attribute_change;
    this.#renderers = /* @__PURE__ */ new Map();
    this.#handlers = handlers(this.#html);
    this.#tick(effects.all.toArray());
  }
  send(action) {
    if (action instanceof Attrs) {
      for (const attr of action[0]) {
        const decoder = this.#onAttributeChange.get(attr[0]);
        if (!decoder)
          continue;
        const msg = decoder(attr[1]);
        if (msg instanceof Error)
          continue;
        this.#queue.push(msg);
      }
      this.#tick();
    } else if (action instanceof Batch) {
      this.#queue = this.#queue.concat(action[0].toArray());
      this.#tick(action[1].all.toArray());
    } else if (action instanceof Debug) {
    } else if (action instanceof Dispatch) {
      this.#queue.push(action[0]);
      this.#tick();
    } else if (action instanceof Emit2) {
      const event2 = new Emit(action[0], action[1]);
      for (const [_, renderer] of this.#renderers) {
        renderer(event2);
      }
    } else if (action instanceof Event2) {
      const handler = this.#handlers.get(action[0]);
      if (!handler)
        return;
      const msg = handler(action[1]);
      if (msg instanceof Error)
        return;
      this.#queue.push(msg[0]);
      this.#tick();
    } else if (action instanceof Subscribe) {
      const attrs = keys(this.#onAttributeChange);
      const patch = new Init(attrs, this.#html);
      this.#renderers = this.#renderers.set(action[0], action[1]);
      action[1](patch);
    } else if (action instanceof Unsubscribe) {
      this.#renderers = this.#renderers.delete(action[0]);
    }
  }
  #model;
  #update;
  #queue;
  #view;
  #html;
  #renderers;
  #handlers;
  #onAttributeChange;
  #tick(effects = []) {
    if (!this.#flush(false, effects))
      return;
    const vdom = this.#view(this.#model);
    const diff2 = elements(this.#html, vdom);
    if (!is_empty_element_diff(diff2)) {
      const patch = new Diff(diff2);
      for (const [_, renderer] of this.#renderers) {
        renderer(patch);
      }
    }
    this.#html = vdom;
    this.#handlers = diff2.handlers;
  }
  #flush(didUpdate = false, effects = []) {
    while (this.#queue.length > 0) {
      const msg = this.#queue.shift();
      const [next, effect] = this.#update(this.#model, msg);
      didUpdate ||= this.#model !== next;
      effects = effects.concat(effect.all.toArray());
      this.#model = next;
    }
    while (effects.length > 0) {
      const effect = effects.shift();
      const dispatch = (msg) => this.send(new Dispatch(msg));
      const emit2 = (event2, data) => this.root.dispatchEvent(
        new CustomEvent(event2, {
          detail: data,
          bubbles: true,
          composed: true
        })
      );
      const select = () => {
      };
      const root = null;
      effect({ dispatch, emit: emit2, select, root });
    }
    if (this.#queue.length > 0) {
      return this.#flush(didUpdate, effects);
    } else {
      return didUpdate;
    }
  }
};
var start_server_application = LustreServerApplication.start;
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
function start2(app, selector, flags) {
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
function h4(attrs, children2) {
  return element("h4", attrs, children2);
}
function div(attrs, children2) {
  return element("div", attrs, children2);
}
function p(attrs, children2) {
  return element("p", attrs, children2);
}
function br(attrs) {
  return element("br", attrs, toList([]));
}
function table(attrs, children2) {
  return element("table", attrs, children2);
}
function tbody(attrs, children2) {
  return element("tbody", attrs, children2);
}
function td(attrs, children2) {
  return element("td", attrs, children2);
}
function tr(attrs, children2) {
  return element("tr", attrs, children2);
}
function button(attrs, children2) {
  return element("button", attrs, children2);
}
function input(attrs) {
  return element("input", attrs, toList([]));
}
function label(attrs, children2) {
  return element("label", attrs, children2);
}

// build/dev/javascript/lustre/lustre/event.mjs
function on2(name2, handler) {
  return on(name2, handler);
}
function on_click(msg) {
  return on2("click", (_) => {
    return new Ok(msg);
  });
}
function on_keydown(msg) {
  return on2(
    "keydown",
    (event2) => {
      let _pipe = event2;
      let _pipe$1 = field("key", string)(_pipe);
      return map3(_pipe$1, msg);
    }
  );
}
function on_keyup(msg) {
  return on2(
    "keyup",
    (event2) => {
      let _pipe = event2;
      let _pipe$1 = field("key", string)(_pipe);
      return map3(_pipe$1, msg);
    }
  );
}
function on_focus(msg) {
  return on2("focus", (_) => {
    return new Ok(msg);
  });
}
function on_blur(msg) {
  return on2("blur", (_) => {
    return new Ok(msg);
  });
}
function value2(event2) {
  let _pipe = event2;
  return field("target", field("value", string))(
    _pipe
  );
}
function on_input(msg) {
  return on2(
    "input",
    (event2) => {
      let _pipe = value2(event2);
      return map3(_pipe, msg);
    }
  );
}
function checked(event2) {
  let _pipe = event2;
  return field("target", field("checked", bool))(
    _pipe
  );
}
function on_check(msg) {
  return on2(
    "change",
    (event2) => {
      let _pipe = checked(event2);
      return map3(_pipe, msg);
    }
  );
}

// build/dev/javascript/squared_away/squared_away/renderable_error.mjs
var RenderableError = class extends CustomType {
  constructor(title, info, hint) {
    super();
    this.title = title;
    this.info = info;
    this.hint = hint;
  }
};

// build/dev/javascript/squared_away/squared_away/squared_away_lang/interpreter/runtime_error.mjs
var RuntimeError = class extends CustomType {
  constructor(context) {
    super();
    this.context = context;
  }
};

// build/dev/javascript/squared_away/squared_away/squared_away_lang/parser/parse_error.mjs
var ParseError = class extends CustomType {
  constructor(context) {
    super();
    this.context = context;
  }
};

// build/dev/javascript/squared_away/squared_away/squared_away_lang/scanner/scan_error.mjs
var ScanError = class extends CustomType {
  constructor(context) {
    super();
    this.context = context;
  }
};

// build/dev/javascript/gleam_stdlib/gleam/io.mjs
function debug(term) {
  let _pipe = term;
  let _pipe$1 = inspect2(_pipe);
  print_debug(_pipe$1);
  return term;
}

// build/dev/javascript/gsv/gsv_ffi.mjs
function slice3(string3, start3, size) {
  return string3.slice(start3, start3 + size);
}
function drop_bytes(string3, bytes) {
  return string3.slice(bytes);
}

// build/dev/javascript/gsv/gsv.mjs
var UnescapedQuote = class extends CustomType {
  constructor(position) {
    super();
    this.position = position;
  }
};
var UnclosedEscapedField = class extends CustomType {
  constructor(start3) {
    super();
    this.start = start3;
  }
};
var Windows = class extends CustomType {
};
var Unix = class extends CustomType {
};
var ParsingEscapedField = class extends CustomType {
};
var ParsingUnescapedField = class extends CustomType {
};
var CommaFound = class extends CustomType {
};
var NewlineFound = class extends CustomType {
};
function line_ending_to_string(le) {
  if (le instanceof Windows) {
    return "\r\n";
  } else {
    return "\n";
  }
}
function escape_field(field2, separator) {
  let $ = contains_string(field2, '"');
  if ($) {
    return '"' + replace(field2, '"', '""') + '"';
  } else {
    let $1 = contains_string(field2, separator) || contains_string(field2, "\n");
    if ($1) {
      return '"' + field2 + '"';
    } else {
      return field2;
    }
  }
}
function from_lists(rows, separator, line_ending) {
  let line_ending$1 = line_ending_to_string(line_ending);
  let _pipe = map2(
    rows,
    (row2) => {
      let _pipe2 = map2(
        row2,
        (_capture) => {
          return escape_field(_capture, separator);
        }
      );
      return join2(_pipe2, separator);
    }
  );
  return join2(_pipe, line_ending$1);
}
function extract_field(string3, from2, length4, status) {
  let field2 = slice3(string3, from2, length4);
  if (status instanceof CommaFound) {
    return field2;
  } else if (status instanceof ParsingUnescapedField) {
    return field2;
  } else if (status instanceof NewlineFound) {
    return field2;
  } else {
    return replace(field2, '""', '"');
  }
}
function do_parse(loop$string, loop$original, loop$field_start, loop$field_length, loop$row, loop$rows, loop$status) {
  while (true) {
    let string3 = loop$string;
    let original = loop$original;
    let field_start = loop$field_start;
    let field_length = loop$field_length;
    let row2 = loop$row;
    let rows = loop$rows;
    let status = loop$status;
    if (string3.startsWith(",") && status instanceof CommaFound) {
      let rest = string3.slice(1);
      let field2 = extract_field(original, field_start, field_length, status);
      let row$1 = prepend(field2, row2);
      let field_start$1 = field_start + field_length + 1;
      loop$string = rest;
      loop$original = original;
      loop$field_start = field_start$1;
      loop$field_length = 0;
      loop$row = row$1;
      loop$rows = rows;
      loop$status = new CommaFound();
    } else if (string3.startsWith(",") && status instanceof NewlineFound) {
      let rest = string3.slice(1);
      let field2 = extract_field(original, field_start, field_length, status);
      let row$1 = prepend(field2, row2);
      let field_start$1 = field_start + field_length + 1;
      loop$string = rest;
      loop$original = original;
      loop$field_start = field_start$1;
      loop$field_length = 0;
      loop$row = row$1;
      loop$rows = rows;
      loop$status = new CommaFound();
    } else if (string3.startsWith(",") && status instanceof ParsingUnescapedField) {
      let rest = string3.slice(1);
      let field2 = extract_field(original, field_start, field_length, status);
      let row$1 = prepend(field2, row2);
      let field_start$1 = field_start + field_length + 1;
      loop$string = rest;
      loop$original = original;
      loop$field_start = field_start$1;
      loop$field_length = 0;
      loop$row = row$1;
      loop$rows = rows;
      loop$status = new CommaFound();
    } else if (string3.startsWith('",') && status instanceof ParsingEscapedField) {
      let rest = string3.slice(2);
      let field2 = extract_field(original, field_start, field_length, status);
      let row$1 = prepend(field2, row2);
      let field_start$1 = field_start + field_length + 2;
      loop$string = rest;
      loop$original = original;
      loop$field_start = field_start$1;
      loop$field_length = 0;
      loop$row = row$1;
      loop$rows = rows;
      loop$status = new CommaFound();
    } else if (string3 === "" && status instanceof ParsingUnescapedField) {
      let field2 = extract_field(original, field_start, field_length, status);
      let row$1 = reverse(prepend(field2, row2));
      return new Ok(reverse(prepend(row$1, rows)));
    } else if (string3 === '"' && status instanceof ParsingEscapedField) {
      let field2 = extract_field(original, field_start, field_length, status);
      let row$1 = reverse(prepend(field2, row2));
      return new Ok(reverse(prepend(row$1, rows)));
    } else if (string3 === "" && status instanceof CommaFound) {
      let row$1 = reverse(prepend("", row2));
      return new Ok(reverse(prepend(row$1, rows)));
    } else if (string3 === "" && status instanceof NewlineFound) {
      return new Ok(reverse(rows));
    } else if (string3 === "" && status instanceof ParsingEscapedField) {
      return new Error(new UnclosedEscapedField(field_start));
    } else if (string3.startsWith("\n") && status instanceof ParsingUnescapedField) {
      let rest = string3.slice(1);
      let field2 = extract_field(original, field_start, field_length, status);
      let row$1 = reverse(prepend(field2, row2));
      let rows$1 = prepend(row$1, rows);
      let field_start$1 = field_start + field_length + 1;
      loop$string = rest;
      loop$original = original;
      loop$field_start = field_start$1;
      loop$field_length = 0;
      loop$row = toList([]);
      loop$rows = rows$1;
      loop$status = new NewlineFound();
    } else if (string3.startsWith("\r\n") && status instanceof ParsingUnescapedField) {
      let rest = string3.slice(2);
      let field2 = extract_field(original, field_start, field_length, status);
      let row$1 = reverse(prepend(field2, row2));
      let rows$1 = prepend(row$1, rows);
      let field_start$1 = field_start + field_length + 2;
      loop$string = rest;
      loop$original = original;
      loop$field_start = field_start$1;
      loop$field_length = 0;
      loop$row = toList([]);
      loop$rows = rows$1;
      loop$status = new NewlineFound();
    } else if (string3.startsWith('"\n') && status instanceof ParsingEscapedField) {
      let rest = string3.slice(2);
      let field2 = extract_field(original, field_start, field_length, status);
      let row$1 = reverse(prepend(field2, row2));
      let rows$1 = prepend(row$1, rows);
      let field_start$1 = field_start + field_length + 2;
      loop$string = rest;
      loop$original = original;
      loop$field_start = field_start$1;
      loop$field_length = 0;
      loop$row = toList([]);
      loop$rows = rows$1;
      loop$status = new NewlineFound();
    } else if (string3.startsWith('"\r\n') && status instanceof ParsingEscapedField) {
      let rest = string3.slice(3);
      let field2 = extract_field(original, field_start, field_length, status);
      let row$1 = reverse(prepend(field2, row2));
      let rows$1 = prepend(row$1, rows);
      let field_start$1 = field_start + field_length + 3;
      loop$string = rest;
      loop$original = original;
      loop$field_start = field_start$1;
      loop$field_length = 0;
      loop$row = toList([]);
      loop$rows = rows$1;
      loop$status = new NewlineFound();
    } else if (string3.startsWith("\n") && status instanceof CommaFound) {
      let rest = string3.slice(1);
      let row$1 = reverse(prepend("", row2));
      let rows$1 = prepend(row$1, rows);
      loop$string = rest;
      loop$original = original;
      loop$field_start = field_start + 1;
      loop$field_length = 0;
      loop$row = toList([]);
      loop$rows = rows$1;
      loop$status = new NewlineFound();
    } else if (string3.startsWith("\r\n") && status instanceof CommaFound) {
      let rest = string3.slice(2);
      let row$1 = reverse(prepend("", row2));
      let rows$1 = prepend(row$1, rows);
      loop$string = rest;
      loop$original = original;
      loop$field_start = field_start + 2;
      loop$field_length = 0;
      loop$row = toList([]);
      loop$rows = rows$1;
      loop$status = new NewlineFound();
    } else if (string3.startsWith("\n") && status instanceof NewlineFound) {
      let rest = string3.slice(1);
      loop$string = rest;
      loop$original = original;
      loop$field_start = field_start + 1;
      loop$field_length = 0;
      loop$row = row2;
      loop$rows = rows;
      loop$status = status;
    } else if (string3.startsWith("\r\n") && status instanceof NewlineFound) {
      let rest = string3.slice(2);
      loop$string = rest;
      loop$original = original;
      loop$field_start = field_start + 2;
      loop$field_length = 0;
      loop$row = row2;
      loop$rows = rows;
      loop$status = status;
    } else if (string3.startsWith('""') && status instanceof ParsingEscapedField) {
      let rest = string3.slice(2);
      loop$string = rest;
      loop$original = original;
      loop$field_start = field_start;
      loop$field_length = field_length + 2;
      loop$row = row2;
      loop$rows = rows;
      loop$status = status;
    } else if (string3.startsWith('"') && status instanceof ParsingUnescapedField) {
      return new Error(new UnescapedQuote(field_start + field_length));
    } else if (string3.startsWith('"') && status instanceof ParsingEscapedField) {
      return new Error(new UnescapedQuote(field_start + field_length));
    } else if (string3.startsWith('"') && status instanceof CommaFound) {
      let rest = string3.slice(1);
      let status$1 = new ParsingEscapedField();
      loop$string = rest;
      loop$original = original;
      loop$field_start = field_start + 1;
      loop$field_length = 0;
      loop$row = row2;
      loop$rows = rows;
      loop$status = status$1;
    } else if (string3.startsWith('"') && status instanceof NewlineFound) {
      let rest = string3.slice(1);
      let status$1 = new ParsingEscapedField();
      loop$string = rest;
      loop$original = original;
      loop$field_start = field_start + 1;
      loop$field_length = 0;
      loop$row = row2;
      loop$rows = rows;
      loop$status = status$1;
    } else if (status instanceof CommaFound) {
      let status$1 = (() => {
        if (status instanceof ParsingEscapedField) {
          return new ParsingEscapedField();
        } else if (status instanceof CommaFound) {
          return new ParsingUnescapedField();
        } else if (status instanceof NewlineFound) {
          return new ParsingUnescapedField();
        } else {
          return new ParsingUnescapedField();
        }
      })();
      let rest = drop_bytes(string3, 1);
      loop$string = rest;
      loop$original = original;
      loop$field_start = field_start;
      loop$field_length = field_length + 1;
      loop$row = row2;
      loop$rows = rows;
      loop$status = status$1;
    } else if (status instanceof NewlineFound) {
      let status$1 = (() => {
        if (status instanceof ParsingEscapedField) {
          return new ParsingEscapedField();
        } else if (status instanceof CommaFound) {
          return new ParsingUnescapedField();
        } else if (status instanceof NewlineFound) {
          return new ParsingUnescapedField();
        } else {
          return new ParsingUnescapedField();
        }
      })();
      let rest = drop_bytes(string3, 1);
      loop$string = rest;
      loop$original = original;
      loop$field_start = field_start;
      loop$field_length = field_length + 1;
      loop$row = row2;
      loop$rows = rows;
      loop$status = status$1;
    } else if (status instanceof ParsingUnescapedField) {
      let status$1 = (() => {
        if (status instanceof ParsingEscapedField) {
          return new ParsingEscapedField();
        } else if (status instanceof CommaFound) {
          return new ParsingUnescapedField();
        } else if (status instanceof NewlineFound) {
          return new ParsingUnescapedField();
        } else {
          return new ParsingUnescapedField();
        }
      })();
      let rest = drop_bytes(string3, 1);
      loop$string = rest;
      loop$original = original;
      loop$field_start = field_start;
      loop$field_length = field_length + 1;
      loop$row = row2;
      loop$rows = rows;
      loop$status = status$1;
    } else {
      let status$1 = (() => {
        if (status instanceof ParsingEscapedField) {
          return new ParsingEscapedField();
        } else if (status instanceof CommaFound) {
          return new ParsingUnescapedField();
        } else if (status instanceof NewlineFound) {
          return new ParsingUnescapedField();
        } else {
          return new ParsingUnescapedField();
        }
      })();
      let rest = drop_bytes(string3, 1);
      loop$string = rest;
      loop$original = original;
      loop$field_start = field_start;
      loop$field_length = field_length + 1;
      loop$row = row2;
      loop$rows = rows;
      loop$status = status$1;
    }
  }
}
function to_lists(loop$input) {
  while (true) {
    let input2 = loop$input;
    if (input2.startsWith("\n")) {
      let rest = input2.slice(1);
      loop$input = rest;
    } else if (input2.startsWith("\r\n")) {
      let rest = input2.slice(2);
      loop$input = rest;
    } else if (input2.startsWith('"')) {
      let rest = input2.slice(1);
      return do_parse(
        rest,
        input2,
        1,
        0,
        toList([]),
        toList([]),
        new ParsingEscapedField()
      );
    } else if (input2.startsWith(",")) {
      let rest = input2.slice(1);
      return do_parse(
        rest,
        input2,
        1,
        0,
        toList([""]),
        toList([]),
        new CommaFound()
      );
    } else {
      return do_parse(
        input2,
        input2,
        0,
        0,
        toList([]),
        toList([]),
        new ParsingUnescapedField()
      );
    }
  }
}

// build/dev/javascript/squared_away/squared_away/squared_away_lang/grid.mjs
var Grid = class extends CustomType {
  constructor(inner, cells) {
    super();
    this.inner = inner;
    this.cells = cells;
  }
};
var GridKey = class extends CustomType {
  constructor(row2, col2) {
    super();
    this.row = row2;
    this.col = col2;
  }
};
function to_string7(key) {
  let row$1 = key.row;
  let col$1 = key.col;
  return "(" + to_string3(row$1) + "," + to_string3(col$1) + ")";
}
function row(grid_key) {
  return grid_key.row;
}
function col(grid_key) {
  return grid_key.col;
}
function new$4(width, height, default$) {
  let cols = range(1, width);
  let rows = range(1, height);
  let $ = fold2(
    rows,
    [new$(), toList([])],
    (acc, row2) => {
      let grid = acc[0];
      let cells = acc[1];
      let $1 = fold2(
        cols,
        [new$(), toList([])],
        (acc2, col2) => {
          let g3 = acc2[0];
          let c3 = acc2[1];
          return [
            insert(g3, new GridKey(row2, col2), default$),
            prepend(new GridKey(row2, col2), c3)
          ];
        }
      );
      let g2 = $1[0];
      let c2 = $1[1];
      return [merge(grid, g2), flatten2(toList([c2, cells]))];
    }
  );
  let g = $[0];
  let c = $[1];
  return new Grid(g, c);
}
function insert4(grid, key, item) {
  return grid.withFields({ inner: insert(grid.inner, key, item) });
}
function find3(grid, item) {
  let _pipe = grid.inner;
  let _pipe$1 = map_to_list(_pipe);
  return find_map(
    _pipe$1,
    (i) => {
      let k = i[0];
      let i$1 = i[1];
      let $ = isEqual(i$1, item);
      if (!$) {
        return new Error(void 0);
      } else {
        return new Ok(k);
      }
    }
  );
}
function fold4(grid, acc, do$) {
  return fold(grid.inner, acc, do$);
}
function map_values2(grid, do$) {
  return new Grid(map_values(grid.inner, do$), grid.cells);
}
function get4(grid, key) {
  let $ = get(grid.inner, key);
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "squared_away/squared_away_lang/grid",
      85,
      "get",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    );
  }
  let item = $[0];
  return item;
}
function cell_to_the_right(grid, key) {
  return find2(
    grid.cells,
    (k) => {
      return k.row === key.row && k.col === key.col + 1;
    }
  );
}
function cell_underneath(grid, key) {
  return find2(
    grid.cells,
    (k) => {
      return k.row === key.row + 1 && k.col === key.col;
    }
  );
}
function cell_above(grid, key) {
  return find2(
    grid.cells,
    (k) => {
      return k.row + 1 === key.row && k.col === key.col;
    }
  );
}
function cell_to_the_left(grid, key) {
  return find2(
    grid.cells,
    (k) => {
      return k.row === key.row && k.col + 1 === key.col;
    }
  );
}
function intersect(row_cell, col_cell) {
  let row$1 = row_cell.row;
  let col_check = row_cell.col;
  let row_check = col_cell.row;
  let col$1 = col_cell.col;
  let $ = row$1 !== row_check && col$1 !== col_check;
  if (!$) {
    return new Error(void 0);
  } else {
    return new Ok(new GridKey(row$1, col$1));
  }
}
function to_list3(grid) {
  return map_to_list(grid.inner);
}
function src_csv(grid) {
  let _pipe = to_list3(grid);
  let _pipe$1 = sort(
    _pipe,
    (c1, c2) => {
      let r1 = c1[0].row;
      let c1$1 = c1[0].col;
      let r2 = c2[0].row;
      let c2$1 = c2[0].col;
      let $ = compare(r1, r2);
      if ($ instanceof Eq) {
        return compare(c1$1, c2$1);
      } else {
        let x = $;
        return x;
      }
    }
  );
  let _pipe$2 = chunk(
    _pipe$1,
    (c) => {
      let r = c[0].row;
      return r;
    }
  );
  let _pipe$3 = map2(
    _pipe$2,
    (_capture) => {
      return map2(
        _capture,
        (c) => {
          let src = c[1];
          return src;
        }
      );
    }
  );
  return from_lists(_pipe$3, ",", new Unix());
}
function from_src_csv(src, width, height) {
  let $ = (() => {
    let _pipe = to_lists(src);
    return debug(_pipe);
  })();
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "squared_away/squared_away_lang/grid",
      137,
      "from_src_csv",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    );
  }
  let src$1 = $[0];
  let inner = (() => {
    let _pipe = take(src$1, height);
    let _pipe$1 = map2(
      _pipe,
      (_capture) => {
        return take(_capture, width);
      }
    );
    let _pipe$2 = index_map(
      _pipe$1,
      (src_row, row_index) => {
        return index_map(
          src_row,
          (cell_content, col_index) => {
            return [new GridKey(row_index + 1, col_index + 1), cell_content];
          }
        );
      }
    );
    let _pipe$3 = flatten2(_pipe$2);
    return from_list(_pipe$3);
  })();
  let cells = keys(inner);
  return new Grid(inner, cells);
}

// build/dev/javascript/squared_away/squared_away/squared_away_lang/parser/expr.mjs
var Empty2 = class extends CustomType {
};
var FloatLiteral = class extends CustomType {
  constructor(f) {
    super();
    this.f = f;
  }
};
var LabelDef = class extends CustomType {
  constructor(txt) {
    super();
    this.txt = txt;
  }
};
var Label = class extends CustomType {
  constructor(txt) {
    super();
    this.txt = txt;
  }
};
var CrossLabel = class extends CustomType {
  constructor(row2, col2) {
    super();
    this.row = row2;
    this.col = col2;
  }
};
var IntegerLiteral = class extends CustomType {
  constructor(n) {
    super();
    this.n = n;
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
var BuiltinSum = class extends CustomType {
  constructor(key) {
    super();
    this.key = key;
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
var And = class extends CustomType {
};
var Or = class extends CustomType {
};
var MustBe = class extends CustomType {
};
var Negate = class extends CustomType {
};
var Not = class extends CustomType {
};
function binary_to_string(b) {
  if (b instanceof Add) {
    return "+";
  } else if (b instanceof And) {
    return "&&";
  } else if (b instanceof Divide) {
    return "/";
  } else if (b instanceof EqualCheck) {
    return "==";
  } else if (b instanceof GreaterThanCheck) {
    return ">";
  } else if (b instanceof GreaterThanOrEqualCheck) {
    return ">=";
  } else if (b instanceof LessThanCheck) {
    return "<";
  } else if (b instanceof LessThanOrEqualCheck) {
    return "<=";
  } else if (b instanceof Multiply) {
    return "*";
  } else if (b instanceof NotEqualCheck) {
    return "!=";
  } else if (b instanceof Or) {
    return "||";
  } else if (b instanceof Power) {
    return "**";
  } else if (b instanceof Subtract) {
    return "-";
  } else {
    return "mustbe";
  }
}
function unary_to_string(u) {
  if (u instanceof Negate) {
    return "-";
  } else {
    return "!";
  }
}

// build/dev/javascript/squared_away/squared_away/squared_away_lang/typechecker/typ.mjs
var TNil = class extends CustomType {
};
var TFloat = class extends CustomType {
};
var TString = class extends CustomType {
};
var TInt = class extends CustomType {
};
var TBool = class extends CustomType {
};
function to_string8(typ) {
  if (typ instanceof TNil) {
    return "Empty";
  } else if (typ instanceof TFloat) {
    return "Floating Point Number";
  } else if (typ instanceof TString) {
    return "Text";
  } else if (typ instanceof TInt) {
    return "Integer";
  } else if (typ instanceof TBool) {
    return "Boolean (True or False)";
  } else {
    return "Test Result (Pass or Fail)";
  }
}

// build/dev/javascript/squared_away/squared_away/squared_away_lang/typechecker/type_error.mjs
var TypeError = class extends CustomType {
  constructor(context) {
    super();
    this.context = context;
  }
};
var IncorrectTypesForBinaryOp = class extends CustomType {
  constructor(lhs, rhs, binary_op) {
    super();
    this.lhs = lhs;
    this.rhs = rhs;
    this.binary_op = binary_op;
  }
};
function describe_binary_op_kind_for_err(bo) {
  if (bo instanceof Add) {
    return "Addition `+`";
  } else if (bo instanceof And) {
    return "Boolean And `&&`";
  } else if (bo instanceof Divide) {
    return "Division `/`";
  } else if (bo instanceof EqualCheck) {
    return "Equality Check `==`";
  } else if (bo instanceof GreaterThanCheck) {
    return "Greater Than Check `>`";
  } else if (bo instanceof GreaterThanOrEqualCheck) {
    return "Greater Than Or Equal Check `>=`";
  } else if (bo instanceof LessThanCheck) {
    return "Less Than Check `<`";
  } else if (bo instanceof LessThanOrEqualCheck) {
    return "Less Than or Equal Check `<=`";
  } else if (bo instanceof Multiply) {
    return "Multiplication `*`";
  } else if (bo instanceof NotEqualCheck) {
    return "Not Equal Check `!=`";
  } else if (bo instanceof Or) {
    return "Boolean Or `||`";
  } else if (bo instanceof Power) {
    return "To The Power Of `**`";
  } else if (bo instanceof Subtract) {
    return "Subtraction `-`";
  } else {
    return "MustBe `mustbe`";
  }
}
function to_renderable_error(te) {
  if (te instanceof IncorrectTypesForBinaryOp) {
    let lhs = te.lhs;
    let rhs = te.rhs;
    let op = te.binary_op;
    return new RenderableError(
      "Unexpected arguments to binary operation " + describe_binary_op_kind_for_err(
        op
      ),
      "Got " + to_string8(lhs) + " on the left and " + to_string8(
        rhs
      ) + " on the right",
      new None()
    );
  } else {
    let t2 = te.context;
    return new RenderableError("Type Error", t2, new None());
  }
}

// build/dev/javascript/squared_away/squared_away/squared_away_lang/error.mjs
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
var TypeError2 = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var RuntimeError2 = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
function to_renderable_error2(ce) {
  if (ce instanceof TypeError2) {
    let te = ce[0];
    return to_renderable_error(te);
  } else if (ce instanceof ParseError2) {
    let pe = ce[0];
    return new RenderableError("", pe.context, new None());
  } else if (ce instanceof RuntimeError2) {
    let re = ce[0];
    return new RenderableError("", re.context, new None());
  } else {
    return new RenderableError(
      "Compiler error",
      "Todo: implement this error description",
      new None()
    );
  }
}

// build/dev/javascript/squared_away/squared_away/squared_away_lang/interpreter/value.mjs
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
var TestFail = class extends CustomType {
};
var TestPass = class extends CustomType {
};
function value_to_string(fv) {
  if (fv instanceof Empty3) {
    return "";
  } else if (fv instanceof Text2) {
    let t2 = fv.inner;
    return t2;
  } else if (fv instanceof Integer) {
    let n = fv.n;
    return to_string3(n);
  } else if (fv instanceof Boolean) {
    let b = fv.b;
    let _pipe = to_string5(b);
    return uppercase2(_pipe);
  } else if (fv instanceof FloatingPointNumber) {
    let f = fv.f;
    return to_string2(f);
  } else if (fv instanceof TestFail) {
    return "Test Failure";
  } else {
    return "Test Passing";
  }
}

// build/dev/javascript/squared_away/squared_away/squared_away_lang/typechecker/typed_expr.mjs
var Empty4 = class extends CustomType {
  constructor(type_2) {
    super();
    this.type_ = type_2;
  }
};
var FloatLiteral2 = class extends CustomType {
  constructor(type_2, f) {
    super();
    this.type_ = type_2;
    this.f = f;
  }
};
var Label2 = class extends CustomType {
  constructor(type_2, txt) {
    super();
    this.type_ = type_2;
    this.txt = txt;
  }
};
var CrossLabel2 = class extends CustomType {
  constructor(type_2, key, row_label, col_label) {
    super();
    this.type_ = type_2;
    this.key = key;
    this.row_label = row_label;
    this.col_label = col_label;
  }
};
var LabelDef2 = class extends CustomType {
  constructor(type_2, txt) {
    super();
    this.type_ = type_2;
    this.txt = txt;
  }
};
var IntegerLiteral2 = class extends CustomType {
  constructor(type_2, n) {
    super();
    this.type_ = type_2;
    this.n = n;
  }
};
var BooleanLiteral2 = class extends CustomType {
  constructor(type_2, b) {
    super();
    this.type_ = type_2;
    this.b = b;
  }
};
var UnaryOp2 = class extends CustomType {
  constructor(type_2, op, expr) {
    super();
    this.type_ = type_2;
    this.op = op;
    this.expr = expr;
  }
};
var BinaryOp2 = class extends CustomType {
  constructor(type_2, lhs, op, rhs) {
    super();
    this.type_ = type_2;
    this.lhs = lhs;
    this.op = op;
    this.rhs = rhs;
  }
};
var Group2 = class extends CustomType {
  constructor(type_2, expr) {
    super();
    this.type_ = type_2;
    this.expr = expr;
  }
};
var BuiltinSum2 = class extends CustomType {
  constructor(type_2, keys2) {
    super();
    this.type_ = type_2;
    this.keys = keys2;
  }
};
function visit_cross_labels(te, f) {
  if (te instanceof CrossLabel2) {
    let key = te.key;
    let row2 = te.row_label;
    let col2 = te.col_label;
    return f(key, row2, col2);
  } else if (te instanceof UnaryOp2) {
    let t2 = te.type_;
    let o = te.op;
    let expr = te.expr;
    let $ = visit_cross_labels(expr, f);
    if (!$.isOk()) {
      return new Error(void 0);
    } else {
      let modified_expr = $[0];
      return new Ok(new UnaryOp2(t2, o, modified_expr));
    }
  } else if (te instanceof BinaryOp2) {
    let t2 = te.type_;
    let lhs = te.lhs;
    let o = te.op;
    let rhs = te.rhs;
    let $ = visit_cross_labels(lhs, f);
    let $1 = visit_cross_labels(rhs, f);
    if ($.isOk() && $1.isOk()) {
      let modified_lhs = $[0];
      let modified_rhs = $1[0];
      return new Ok(new BinaryOp2(t2, modified_lhs, o, modified_rhs));
    } else {
      return new Error(void 0);
    }
  } else {
    return new Ok(te);
  }
}
function to_string9(te) {
  if (te instanceof BooleanLiteral2) {
    let b = te.b;
    if (!b) {
      return "FALSE";
    } else {
      return "TRUE";
    }
  } else if (te instanceof CrossLabel2) {
    let rl = te.row_label;
    let cl = te.col_label;
    return rl + "_" + cl;
  } else if (te instanceof Empty4) {
    return "";
  } else if (te instanceof FloatLiteral2) {
    let f = te.f;
    return to_string2(f);
  } else if (te instanceof IntegerLiteral2) {
    let i = te.n;
    return to_string3(i);
  } else if (te instanceof Label2) {
    let l = te.txt;
    return l;
  } else if (te instanceof LabelDef2) {
    let l = te.txt;
    return l;
  } else if (te instanceof Group2) {
    let t2 = te.expr;
    return "(" + to_string9(t2) + ")";
  } else if (te instanceof UnaryOp2) {
    let op = te.op;
    let te$1 = te.expr;
    return unary_to_string(op) + to_string9(te$1);
  } else if (te instanceof BinaryOp2) {
    let lhs = te.lhs;
    let bop = te.op;
    let rhs = te.rhs;
    return to_string9(lhs) + binary_to_string(bop) + to_string9(rhs);
  } else {
    return "sum";
  }
}

// build/dev/javascript/squared_away/squared_away/squared_away_lang/interpreter.mjs
function interpret(loop$env, loop$expr) {
  while (true) {
    let env = loop$env;
    let expr = loop$expr;
    if (expr instanceof Empty4) {
      return new Ok(new Empty3());
    } else if (expr instanceof LabelDef2) {
      let txt = expr.txt;
      return new Ok(new Text2(txt));
    } else if (expr instanceof Group2) {
      let expr$1 = expr.expr;
      loop$env = env;
      loop$expr = expr$1;
    } else if (expr instanceof CrossLabel2) {
      let key = expr.key;
      let $ = get4(env, key);
      if ($.isOk()) {
        let expr$1 = $[0];
        loop$env = env;
        loop$expr = expr$1;
      } else {
        return new Ok(new Empty3());
      }
    } else if (expr instanceof Label2) {
      let txt = expr.txt;
      let key = (() => {
        let _pipe = env;
        let _pipe$1 = to_list3(_pipe);
        let _pipe$2 = fold_until(
          _pipe$1,
          new None(),
          (_, i) => {
            if (i[1].isOk() && i[1][0] instanceof LabelDef2 && i[1][0].txt === txt) {
              let cell_ref = i[0];
              let label_txt = i[1][0].txt;
              return new Stop(new Some(cell_ref));
            } else {
              return new Continue(new None());
            }
          }
        );
        let _pipe$3 = map(
          _pipe$2,
          (_capture) => {
            return cell_to_the_right(env, _capture);
          }
        );
        let _pipe$4 = map(_pipe$3, from_result);
        return flatten(_pipe$4);
      })();
      if (key instanceof None) {
        return new Error(
          new RuntimeError2(
            new RuntimeError("Label doesn't point to anything")
          )
        );
      } else {
        let key$1 = key[0];
        let $ = get4(env, key$1);
        if (!$.isOk()) {
          let e = $[0];
          return new Error(e);
        } else {
          let te = $[0];
          if (te instanceof Label2 && te.txt === txt) {
            let ltxt = te.txt;
            return new Error(
              new RuntimeError2(
                new RuntimeError("Label points to itself")
              )
            );
          } else {
            loop$env = env;
            loop$expr = te;
          }
        }
      }
    } else if (expr instanceof BooleanLiteral2) {
      let b = expr.b;
      return new Ok(new Boolean(b));
    } else if (expr instanceof IntegerLiteral2) {
      let n = expr.n;
      return new Ok(new Integer(n));
    } else if (expr instanceof FloatLiteral2) {
      let f = expr.f;
      return new Ok(new FloatingPointNumber(f));
    } else if (expr instanceof UnaryOp2) {
      let op = expr.op;
      let expr$1 = expr.expr;
      return try$(
        interpret(env, expr$1),
        (value3) => {
          if (op instanceof Negate && value3 instanceof Integer) {
            let n = value3.n;
            return new Ok(new Integer(-n));
          } else if (op instanceof Negate && value3 instanceof FloatingPointNumber) {
            let f = value3.f;
            return new Ok(new FloatingPointNumber(negate(f)));
          } else if (op instanceof Not && value3 instanceof Boolean) {
            let b = value3.b;
            return new Ok(new Boolean(!b));
          } else {
            throw makeError(
              "panic",
              "squared_away/squared_away_lang/interpreter",
              83,
              "",
              "These should be the only options if the typechecker is working",
              {}
            );
          }
        }
      );
    } else if (expr instanceof BinaryOp2) {
      let lhs = expr.lhs;
      let op = expr.op;
      let rhs = expr.rhs;
      return try$(
        interpret(env, lhs),
        (lhs2) => {
          return try$(
            interpret(env, rhs),
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
                    "let_assert",
                    "squared_away/squared_away_lang/interpreter",
                    151,
                    "",
                    "Pattern match failed, no pattern matched the value.",
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
                    "let_assert",
                    "squared_away/squared_away_lang/interpreter",
                    155,
                    "",
                    "Pattern match failed, no pattern matched the value.",
                    { value: $ }
                  );
                }
                let p2 = $[0];
                return new Ok(new FloatingPointNumber(p2));
              } else if (lhs2 instanceof Boolean && op instanceof And && rhs2 instanceof Boolean) {
                let a = lhs2.b;
                let b = rhs2.b;
                return new Ok(new Boolean(a && b));
              } else if (lhs2 instanceof Boolean && op instanceof Or && rhs2 instanceof Boolean) {
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
              } else if (op instanceof MustBe) {
                let vlhs = lhs2;
                let vrhs = rhs2;
                let $ = isEqual(vlhs, vrhs);
                if (!$) {
                  return new Ok(new TestFail());
                } else {
                  return new Ok(new TestPass());
                }
              } else {
                let lhs$1 = lhs2;
                let op$1 = op;
                let rhs$1 = rhs2;
                let msg = "these should be the only options if the typechecker is working properly. " + value_to_string(
                  lhs$1
                ) + binary_to_string(op$1) + value_to_string(
                  rhs$1
                );
                throw makeError(
                  "panic",
                  "squared_away/squared_away_lang/interpreter",
                  181,
                  "",
                  msg,
                  {}
                );
              }
            }
          );
        }
      );
    } else {
      let type_2 = expr.type_;
      let keys2 = expr.keys;
      let values = (() => {
        let _pipe = to_list3(env);
        return filter_map(
          _pipe,
          (i) => {
            let gk = i[0];
            let item = i[1];
            let $ = contains(keys2, gk);
            if (!$) {
              return new Error(void 0);
            } else {
              if (!item.isOk()) {
                return new Error(void 0);
              } else {
                let x = item[0];
                let _pipe$1 = interpret(env, x);
                return nil_error(_pipe$1);
              }
            }
          }
        );
      })();
      let val = (() => {
        if (type_2 instanceof TFloat) {
          let _pipe = map2(
            values,
            (v) => {
              if (!(v instanceof FloatingPointNumber)) {
                throw makeError(
                  "let_assert",
                  "squared_away/squared_away_lang/interpreter",
                  206,
                  "",
                  "Pattern match failed, no pattern matched the value.",
                  { value: v }
                );
              }
              let f = v.f;
              return f;
            }
          );
          let _pipe$1 = sum(_pipe);
          return new FloatingPointNumber(_pipe$1);
        } else if (type_2 instanceof TInt) {
          let _pipe = map2(
            values,
            (v) => {
              if (!(v instanceof Integer)) {
                throw makeError(
                  "let_assert",
                  "squared_away/squared_away_lang/interpreter",
                  213,
                  "",
                  "Pattern match failed, no pattern matched the value.",
                  { value: v }
                );
              }
              let i = v.n;
              return i;
            }
          );
          let _pipe$1 = sum2(_pipe);
          return new Integer(_pipe$1);
        } else {
          throw makeError(
            "panic",
            "squared_away/squared_away_lang/interpreter",
            218,
            "interpret",
            "internal compiler error sum function interpret",
            {}
          );
        }
      })();
      return new Ok(val);
    }
  }
}

// build/dev/javascript/squared_away/squared_away/squared_away_lang/scanner/token.mjs
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
var IntegerLiteral3 = class extends CustomType {
  constructor(n) {
    super();
    this.n = n;
  }
};
var FloatLiteral3 = class extends CustomType {
  constructor(f) {
    super();
    this.f = f;
  }
};
var TrueToken = class extends CustomType {
};
var FalseToken = class extends CustomType {
};
var And2 = class extends CustomType {
};
var Or2 = class extends CustomType {
};
var LParen = class extends CustomType {
};
var RParen = class extends CustomType {
};
var Label3 = class extends CustomType {
  constructor(key) {
    super();
    this.key = key;
  }
};
var LabelDef3 = class extends CustomType {
  constructor(txt) {
    super();
    this.txt = txt;
  }
};
var Underscore = class extends CustomType {
};
var BuiltinSum3 = class extends CustomType {
  constructor(key) {
    super();
    this.key = key;
  }
};
var MustBe2 = class extends CustomType {
};

// build/dev/javascript/squared_away/squared_away/squared_away_lang/parser.mjs
function try_parse_binary_ops(tokens) {
  if (tokens.atLeastLength(1) && tokens.head instanceof Plus) {
    let rest = tokens.tail;
    return try$(
      do_parse2(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return guard(
          isEqual(rhs, new Empty2()),
          new Error(
            new ParseError(
              "No item on right hand side of binary operation."
            )
          ),
          () => {
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
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof Minus) {
    let rest = tokens.tail;
    return try$(
      do_parse2(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return guard(
          isEqual(rhs, new Empty2()),
          new Error(
            new ParseError(
              "No item on right hand side of binary operation."
            )
          ),
          () => {
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
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof Star) {
    let rest = tokens.tail;
    return try$(
      do_parse2(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return guard(
          isEqual(rhs, new Empty2()),
          new Error(
            new ParseError(
              "No item on right hand side of binary operation."
            )
          ),
          () => {
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
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof Div) {
    let rest = tokens.tail;
    return try$(
      do_parse2(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return guard(
          isEqual(rhs, new Empty2()),
          new Error(
            new ParseError(
              "No item on right hand side of binary operation."
            )
          ),
          () => {
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
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof StarStar) {
    let rest = tokens.tail;
    return try$(
      do_parse2(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return guard(
          isEqual(rhs, new Empty2()),
          new Error(
            new ParseError(
              "No item on right hand side of binary operation."
            )
          ),
          () => {
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
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof BangEqual) {
    let rest = tokens.tail;
    return try$(
      do_parse2(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return guard(
          isEqual(rhs, new Empty2()),
          new Error(
            new ParseError(
              "No item on right hand side of binary operation."
            )
          ),
          () => {
            return new Ok(
              [
                (_capture) => {
                  return new BinaryOp(
                    _capture,
                    new NotEqualCheck(),
                    rhs
                  );
                },
                rest$1
              ]
            );
          }
        );
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof EqualEqual) {
    let rest = tokens.tail;
    return try$(
      do_parse2(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return guard(
          isEqual(rhs, new Empty2()),
          new Error(
            new ParseError(
              "No item on right hand side of binary operation."
            )
          ),
          () => {
            return new Ok(
              [
                (_capture) => {
                  return new BinaryOp(
                    _capture,
                    new EqualCheck(),
                    rhs
                  );
                },
                rest$1
              ]
            );
          }
        );
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof LessEqual) {
    let rest = tokens.tail;
    return try$(
      do_parse2(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return guard(
          isEqual(rhs, new Empty2()),
          new Error(
            new ParseError(
              "No item on right hand side of binary operation."
            )
          ),
          () => {
            return new Ok(
              [
                (_capture) => {
                  return new BinaryOp(
                    _capture,
                    new LessThanOrEqualCheck(),
                    rhs
                  );
                },
                rest$1
              ]
            );
          }
        );
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof Less) {
    let rest = tokens.tail;
    return try$(
      do_parse2(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return guard(
          isEqual(rhs, new Empty2()),
          new Error(
            new ParseError(
              "No item on right hand side of binary operation."
            )
          ),
          () => {
            return new Ok(
              [
                (_capture) => {
                  return new BinaryOp(
                    _capture,
                    new LessThanCheck(),
                    rhs
                  );
                },
                rest$1
              ]
            );
          }
        );
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof GreaterEqual) {
    let rest = tokens.tail;
    return try$(
      do_parse2(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return guard(
          isEqual(rhs, new Empty2()),
          new Error(
            new ParseError(
              "No item on right hand side of binary operation."
            )
          ),
          () => {
            return new Ok(
              [
                (_capture) => {
                  return new BinaryOp(
                    _capture,
                    new GreaterThanOrEqualCheck(),
                    rhs
                  );
                },
                rest$1
              ]
            );
          }
        );
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof Greater) {
    let rest = tokens.tail;
    return try$(
      do_parse2(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return guard(
          isEqual(rhs, new Empty2()),
          new Error(
            new ParseError(
              "No item on right hand side of binary operation."
            )
          ),
          () => {
            return new Ok(
              [
                (_capture) => {
                  return new BinaryOp(
                    _capture,
                    new GreaterThanCheck(),
                    rhs
                  );
                },
                rest$1
              ]
            );
          }
        );
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof And2) {
    let rest = tokens.tail;
    return try$(
      do_parse2(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return guard(
          isEqual(rhs, new Empty2()),
          new Error(
            new ParseError(
              "No item on right hand side of binary operation."
            )
          ),
          () => {
            return new Ok(
              [
                (_capture) => {
                  return new BinaryOp(_capture, new And(), rhs);
                },
                rest$1
              ]
            );
          }
        );
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof Or2) {
    let rest = tokens.tail;
    return try$(
      do_parse2(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return guard(
          isEqual(rhs, new Empty2()),
          new Error(
            new ParseError(
              "No item on right hand side of binary operation."
            )
          ),
          () => {
            return new Ok(
              [
                (_capture) => {
                  return new BinaryOp(_capture, new Or(), rhs);
                },
                rest$1
              ]
            );
          }
        );
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof MustBe2) {
    let rest = tokens.tail;
    return try$(
      do_parse2(rest),
      (_use0) => {
        let rhs = _use0[0];
        let rest$1 = _use0[1];
        return guard(
          isEqual(rhs, new Empty2()),
          new Error(
            new ParseError(
              "No item on right hand side of binary operation."
            )
          ),
          () => {
            return new Ok(
              [
                (_capture) => {
                  return new BinaryOp(_capture, new MustBe(), rhs);
                },
                rest$1
              ]
            );
          }
        );
      }
    );
  } else {
    return new Error(new ParseError("Not a binary operation"));
  }
}
function do_parse2(tokens) {
  if (tokens.hasLength(0)) {
    return new Ok([new Empty2(), toList([])]);
  } else if (tokens.atLeastLength(1) && tokens.head instanceof LabelDef3) {
    let str = tokens.head.txt;
    let rest = tokens.tail;
    return new Ok([new LabelDef(str), rest]);
  } else if (tokens.atLeastLength(3) && tokens.head instanceof Label3 && tokens.tail.head instanceof Underscore && tokens.tail.tail.head instanceof Label3) {
    let row2 = tokens.head.key;
    let col2 = tokens.tail.tail.head.key;
    let rest = tokens.tail.tail.tail;
    let $ = try_parse_binary_ops(rest);
    if ($.isOk()) {
      let op = $[0][0];
      let rest$1 = $[0][1];
      return new Ok([op(new CrossLabel(row2, col2)), rest$1]);
    } else {
      return new Ok([new CrossLabel(row2, col2), rest]);
    }
  } else if (tokens.atLeastLength(1) && tokens.head instanceof BuiltinSum3) {
    let key = tokens.head.key;
    let rest = tokens.tail;
    let $ = try_parse_binary_ops(rest);
    if ($.isOk()) {
      let op = $[0][0];
      let rest$1 = $[0][1];
      return new Ok([op(new BuiltinSum(key)), rest$1]);
    } else {
      return new Ok([new BuiltinSum(key), rest]);
    }
  } else if (tokens.atLeastLength(1) && tokens.head instanceof Label3) {
    let str = tokens.head.key;
    let rest = tokens.tail;
    let $ = try_parse_binary_ops(rest);
    if ($.isOk()) {
      let op = $[0][0];
      let rest$1 = $[0][1];
      return new Ok([op(new Label(str)), rest$1]);
    } else {
      return new Ok([new Label(str), rest]);
    }
  } else if (tokens.atLeastLength(1) && tokens.head instanceof IntegerLiteral3) {
    let n = tokens.head.n;
    let rest = tokens.tail;
    let $ = try_parse_binary_ops(rest);
    if ($.isOk()) {
      let op = $[0][0];
      let rest$1 = $[0][1];
      return new Ok([op(new IntegerLiteral(n)), rest$1]);
    } else {
      return new Ok([new IntegerLiteral(n), rest]);
    }
  } else if (tokens.atLeastLength(1) && tokens.head instanceof FloatLiteral3) {
    let f = tokens.head.f;
    let rest = tokens.tail;
    let $ = try_parse_binary_ops(rest);
    if ($.isOk()) {
      let op = $[0][0];
      let rest$1 = $[0][1];
      return new Ok([op(new FloatLiteral(f)), rest$1]);
    } else {
      return new Ok([new FloatLiteral(f), rest]);
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
      do_parse2(rest),
      (_use0) => {
        let parsed_remainder = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [new UnaryOp(new Negate(), parsed_remainder), rest$1]
        );
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof Bang) {
    let rest = tokens.tail;
    return try$(
      do_parse2(rest),
      (_use0) => {
        let parsed_remainder = _use0[0];
        let rest$1 = _use0[1];
        return new Ok(
          [new UnaryOp(new Not(), parsed_remainder), rest$1]
        );
      }
    );
  } else if (tokens.atLeastLength(1) && tokens.head instanceof LParen) {
    let rest = tokens.tail;
    return try$(
      do_parse2(rest),
      (_use0) => {
        let body = _use0[0];
        let rest$1 = _use0[1];
        if (rest$1.atLeastLength(1) && rest$1.head instanceof RParen) {
          let rest$2 = rest$1.tail;
          let $ = try_parse_binary_ops(rest$2);
          if ($.isOk()) {
            let op = $[0][0];
            let rest$3 = $[0][1];
            return new Ok([op(new Group(body)), rest$3]);
          } else {
            return new Ok([new Group(body), rest$2]);
          }
        } else {
          return new Error(
            new ParseError("missing closing parentheses")
          );
        }
      }
    );
  } else {
    let x = tokens.head;
    return new Error(
      new ParseError("Unexpected token: " + inspect2(x))
    );
  }
}
function parse3(tokens) {
  return try$(
    do_parse2(tokens),
    (_use0) => {
      let expr = _use0[0];
      let rest = _use0[1];
      if (rest.hasLength(0)) {
        return new Ok(expr);
      } else {
        return new Error(
          new ParseError(
            "After parsing there were leftover tokens " + inspect2(rest)
          )
        );
      }
    }
  );
}

// build/dev/javascript/squared_away/squared_away/squared_away_lang/scanner.mjs
function parse_identifier(loop$src, loop$acc) {
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
    } else if (src.startsWith("a")) {
      let rest = src.slice(1);
      let l = "a";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("b")) {
      let rest = src.slice(1);
      let l = "b";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("c")) {
      let rest = src.slice(1);
      let l = "c";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("d")) {
      let rest = src.slice(1);
      let l = "d";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("e")) {
      let rest = src.slice(1);
      let l = "e";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("f")) {
      let rest = src.slice(1);
      let l = "f";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("g")) {
      let rest = src.slice(1);
      let l = "g";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("h")) {
      let rest = src.slice(1);
      let l = "h";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("i")) {
      let rest = src.slice(1);
      let l = "i";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("j")) {
      let rest = src.slice(1);
      let l = "j";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("k")) {
      let rest = src.slice(1);
      let l = "k";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("l")) {
      let rest = src.slice(1);
      let l = "l";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("m")) {
      let rest = src.slice(1);
      let l = "m";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("n")) {
      let rest = src.slice(1);
      let l = "n";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("o")) {
      let rest = src.slice(1);
      let l = "o";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("p")) {
      let rest = src.slice(1);
      let l = "p";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("q")) {
      let rest = src.slice(1);
      let l = "q";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("r")) {
      let rest = src.slice(1);
      let l = "r";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("s")) {
      let rest = src.slice(1);
      let l = "s";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("t")) {
      let rest = src.slice(1);
      let l = "t";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("u")) {
      let rest = src.slice(1);
      let l = "u";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("v")) {
      let rest = src.slice(1);
      let l = "v";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("w")) {
      let rest = src.slice(1);
      let l = "w";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("x")) {
      let rest = src.slice(1);
      let l = "x";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("y")) {
      let rest = src.slice(1);
      let l = "y";
      loop$src = rest;
      loop$acc = acc + l;
    } else if (src.startsWith("z")) {
      let rest = src.slice(1);
      let l = "z";
      loop$src = rest;
      loop$acc = acc + l;
    } else {
      if (acc === "") {
        return new Error(void 0);
      } else {
        return new Ok([acc, src]);
      }
    }
  }
}
function parse_integer(loop$src, loop$acc) {
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
      let _pipe = parse2(acc);
      return map3(_pipe, (n) => {
        return [n, src];
      });
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
    } else if (src.startsWith("mustbe")) {
      let rest = src.slice(6);
      loop$src = trim_left2(rest);
      loop$acc = prepend(new MustBe2(), acc);
    } else if (src.startsWith("&&")) {
      let rest = src.slice(2);
      loop$src = trim_left2(rest);
      loop$acc = prepend(new And2(), acc);
    } else if (src.startsWith("||")) {
      let rest = src.slice(2);
      loop$src = trim_left2(rest);
      loop$acc = prepend(new Or2(), acc);
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
    } else if (src.startsWith("_")) {
      let rest = src.slice(1);
      loop$src = trim_left2(rest);
      loop$acc = prepend(new Underscore(), acc);
    } else if (src.startsWith("sum")) {
      let rest = src.slice(3);
      loop$src = trim_left2(rest);
      loop$acc = prepend(new BuiltinSum3(new None()), acc);
    } else {
      let $ = parse_integer(src, "");
      if ($.isOk()) {
        let n = $[0][0];
        let rest = $[0][1];
        if (rest.startsWith(".")) {
          let rest$1 = rest.slice(1);
          return try$(
            (() => {
              let _pipe = parse_integer(rest$1, "");
              return replace_error(
                _pipe,
                new ScanError(
                  "Expected more digits after the decimal."
                )
              );
            })(),
            (_use0) => {
              let m = _use0[0];
              let rest$2 = _use0[1];
              let $1 = parse(
                to_string3(n) + "." + to_string3(m)
              );
              if (!$1.isOk()) {
                throw makeError(
                  "let_assert",
                  "squared_away/squared_away_lang/scanner",
                  89,
                  "",
                  "Pattern match failed, no pattern matched the value.",
                  { value: $1 }
                );
              }
              let f = $1[0];
              return do_scan(
                trim_left2(rest$2),
                prepend(new FloatLiteral3(f), acc)
              );
            }
          );
        } else {
          loop$src = trim_left2(rest);
          loop$acc = prepend(new IntegerLiteral3(n), acc);
        }
      } else {
        let $1 = parse_identifier(src, "");
        if (!$1.isOk() && !$1[0]) {
          return new Error(
            new ScanError(
              "Could not understand provided txt: " + src
            )
          );
        } else {
          let ident = $1[0][0];
          let rest = $1[0][1];
          loop$src = trim_left2(rest);
          loop$acc = prepend(new Label3(ident), acc);
        }
      }
    }
  }
}
function scan(src) {
  let $ = trim2(src);
  if ($ === "") {
    return new Ok(toList([]));
  } else if ($ === "TRUE") {
    return new Ok(toList([new TrueToken()]));
  } else if ($ === "FALSE") {
    return new Ok(toList([new FalseToken()]));
  } else if ($.startsWith("=")) {
    let rest = $.slice(1);
    return do_scan(
      (() => {
        let _pipe = rest;
        return trim_left2(_pipe);
      })(),
      toList([])
    );
  } else {
    let txt = $;
    let $1 = parse(txt);
    if ($1.isOk()) {
      let f = $1[0];
      return new Ok(toList([new FloatLiteral3(f)]));
    } else {
      let $2 = parse2(txt);
      if ($2.isOk()) {
        let i = $2[0];
        return new Ok(toList([new IntegerLiteral3(i)]));
      } else {
        let $3 = parse_identifier(txt, "");
        if ($3.isOk() && $3[0][1] === "") {
          let ident = $3[0][0];
          return new Ok(toList([new LabelDef3(ident)]));
        } else {
          return new Error(
            new ScanError(
              "Not a label definition, boolean, float, or integer. Are you possibly missing an `=` sign?"
            )
          );
        }
      }
    }
  }
}

// build/dev/javascript/squared_away/squared_away/squared_away_lang/typechecker.mjs
function typecheck(env, expr) {
  if (expr instanceof Empty2) {
    return new Ok(new Empty4(new TNil()));
  } else if (expr instanceof BuiltinSum) {
    let key = expr.key;
    if (!(key instanceof Some)) {
      throw makeError(
        "let_assert",
        "squared_away/squared_away_lang/typechecker",
        20,
        "typecheck",
        "Pattern match failed, no pattern matched the value.",
        { value: key }
      );
    }
    let key$1 = key[0];
    let items_above_sum_call = (() => {
      let _pipe = to_list3(env);
      let _pipe$1 = filter(
        _pipe,
        (i) => {
          let gk = i[0];
          return col(gk) === col(key$1) && row(gk) < row(
            key$1
          );
        }
      );
      let _pipe$2 = sort(
        _pipe$1,
        (i1, i2) => {
          let gk1 = i1[0];
          let gk2 = i2[0];
          return compare(row(gk1), row(gk2));
        }
      );
      let _pipe$3 = reverse(_pipe$2);
      return take_while(
        _pipe$3,
        (i) => {
          let item = i[1];
          if (item.isOk() && item[0] instanceof LabelDef) {
            return false;
          } else {
            return true;
          }
        }
      );
    })();
    let keys2 = map2(items_above_sum_call, (i) => {
      return i[0];
    });
    let items_above_sum_call$1 = map2(
      items_above_sum_call,
      (i) => {
        return i[1];
      }
    );
    return guard(
      any(items_above_sum_call$1, is_error),
      new Error(
        new TypeError2(
          new TypeError("Cell above sum expression has error")
        )
      ),
      () => {
        let types = (() => {
          let _pipe = map2(
            items_above_sum_call$1,
            (i) => {
              if (!i.isOk()) {
                throw makeError(
                  "let_assert",
                  "squared_away/squared_away_lang/typechecker",
                  57,
                  "",
                  "Pattern match failed, no pattern matched the value.",
                  { value: i }
                );
              }
              let item = i[0];
              return item;
            }
          );
          let _pipe$1 = unique(_pipe);
          return map2(_pipe$1, (e) => {
            return typecheck(env, e);
          });
        })();
        return guard(
          any(types, is_error),
          new Error(
            new TypeError2(
              new TypeError(
                "Cell above sum expression has type error"
              )
            )
          ),
          () => {
            let types$1 = (() => {
              let _pipe = map2(
                types,
                (t2) => {
                  if (!t2.isOk()) {
                    throw makeError(
                      "let_assert",
                      "squared_away/squared_away_lang/typechecker",
                      73,
                      "",
                      "Pattern match failed, no pattern matched the value.",
                      { value: t2 }
                    );
                  }
                  let texpr = t2[0];
                  return texpr.type_;
                }
              );
              return unique(_pipe);
            })();
            if (types$1.hasLength(1) && types$1.head instanceof TFloat) {
              return new Ok(new BuiltinSum2(new TFloat(), keys2));
            } else if (types$1.hasLength(1) && types$1.head instanceof TInt) {
              return new Ok(new BuiltinSum2(new TInt(), keys2));
            } else {
              return new Error(
                new TypeError2(
                  new TypeError(
                    "sum function used on cells of multiple types"
                  )
                )
              );
            }
          }
        );
      }
    );
  } else if (expr instanceof LabelDef) {
    let txt = expr.txt;
    let defs = fold4(
      env,
      0,
      (count, _, expr2) => {
        if (expr2.isOk() && expr2[0] instanceof LabelDef && txt === expr2[0].txt) {
          let t2 = expr2[0].txt;
          return count + 1;
        } else {
          return count;
        }
      }
    );
    if (defs === 1) {
      return new Ok(new LabelDef2(new TNil(), txt));
    } else if (defs > 1) {
      return new Error(
        new TypeError2(new TypeError("Duplicate Label"))
      );
    } else {
      return new Error(
        new TypeError2(
          new TypeError("This is an internal compiler error.")
        )
      );
    }
  } else if (expr instanceof Label) {
    let txt = expr.txt;
    let key = (() => {
      let _pipe = env;
      let _pipe$1 = to_list3(_pipe);
      let _pipe$2 = fold_until(
        _pipe$1,
        new None(),
        (_, i) => {
          if (i[1].isOk() && i[1][0] instanceof LabelDef && i[1][0].txt === txt) {
            let cell_ref = i[0];
            let label_txt = i[1][0].txt;
            return new Stop(new Some(cell_ref));
          } else {
            return new Continue(new None());
          }
        }
      );
      let _pipe$3 = map(
        _pipe$2,
        (_capture) => {
          return cell_to_the_right(env, _capture);
        }
      );
      let _pipe$4 = map(_pipe$3, from_result);
      return flatten(_pipe$4);
    })();
    if (key instanceof None) {
      return new Ok(new Label2(new TNil(), txt));
    } else {
      let key$1 = key[0];
      let x = get4(env, key$1);
      if (!x.isOk()) {
        let e = x[0];
        return new Error(e);
      } else if (x.isOk() && x[0] instanceof Label && x[0].txt === txt) {
        let ltxt = x[0].txt;
        return new Error(
          new TypeError2(
            new TypeError("Label points to itself")
          )
        );
      } else {
        let expr$1 = x[0];
        let $ = typecheck(env, expr$1);
        if ($.isOk()) {
          let te = $[0];
          return new Ok(new Label2(te.type_, txt));
        } else {
          let e = $[0];
          return new Error(e);
        }
      }
    }
  } else if (expr instanceof CrossLabel) {
    let row2 = expr.row;
    let col2 = expr.col;
    let col_cell = (() => {
      let _pipe = env;
      let _pipe$1 = to_list3(_pipe);
      return fold_until(
        _pipe$1,
        new None(),
        (_, cell) => {
          if (cell[1].isOk() && cell[1][0] instanceof LabelDef && cell[1][0].txt === col2) {
            let cell_ref = cell[0];
            let label_txt = cell[1][0].txt;
            return new Stop(new Some(cell_ref));
          } else {
            return new Continue(new None());
          }
        }
      );
    })();
    if (col_cell instanceof None) {
      return new Error(
        new TypeError2(
          new TypeError("No label called: " + col2)
        )
      );
    } else {
      let col_cell$1 = col_cell[0];
      let row_cell = (() => {
        let _pipe = env;
        let _pipe$1 = to_list3(_pipe);
        return fold_until(
          _pipe$1,
          new None(),
          (_, i) => {
            if (i[1].isOk() && i[1][0] instanceof LabelDef && i[1][0].txt === row2) {
              let cell_ref = i[0];
              let label_txt = i[1][0].txt;
              return new Stop(new Some(cell_ref));
            } else {
              return new Continue(new None());
            }
          }
        );
      })();
      if (row_cell instanceof None) {
        return new Error(
          new TypeError2(
            new TypeError("No label called: " + row2)
          )
        );
      } else {
        let row_cell$1 = row_cell[0];
        let new_key = intersect(row_cell$1, col_cell$1);
        if (!new_key.isOk()) {
          return new Error(
            new TypeError2(
              new TypeError(
                "Labels " + row2 + " and " + col2 + " do not intersect"
              )
            )
          );
        } else {
          let nk = new_key[0];
          let x = get4(env, nk);
          if (!x.isOk()) {
            let e = x[0];
            return new Error(e);
          } else {
            let expr$1 = x[0];
            let $ = typecheck(env, expr$1);
            if ($.isOk()) {
              let te = $[0];
              return new Ok(new CrossLabel2(te.type_, nk, row2, col2));
            } else {
              let e = $[0];
              return new Error(e);
            }
          }
        }
      }
    }
  } else if (expr instanceof BooleanLiteral) {
    let b = expr.val;
    return new Ok(new BooleanLiteral2(new TBool(), b));
  } else if (expr instanceof FloatLiteral) {
    let f = expr.f;
    return new Ok(new FloatLiteral2(new TFloat(), f));
  } else if (expr instanceof IntegerLiteral) {
    let n = expr.n;
    return new Ok(new IntegerLiteral2(new TInt(), n));
  } else if (expr instanceof Group) {
    let inner = expr.inner;
    return try$(
      typecheck(env, inner),
      (expr2) => {
        return new Ok(new Group2(expr2.type_, expr2));
      }
    );
  } else if (expr instanceof UnaryOp) {
    let op = expr.op;
    let expr$1 = expr.expr;
    return try$(
      typecheck(env, expr$1),
      (expr2) => {
        let $ = expr2.type_;
        if (op instanceof Negate && $ instanceof TInt) {
          return new Ok(new UnaryOp2(expr2.type_, op, expr2));
        } else if (op instanceof Negate && $ instanceof TFloat) {
          return new Ok(new UnaryOp2(expr2.type_, op, expr2));
        } else if (op instanceof Not && $ instanceof TBool) {
          return new Ok(new UnaryOp2(expr2.type_, op, expr2));
        } else {
          return new Error(
            new TypeError2(
              new TypeError(
                "Unexpected type and operator combination"
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
      typecheck(env, lhs),
      (lhs2) => {
        return try$(
          typecheck(env, rhs),
          (rhs2) => {
            let $ = lhs2.type_;
            let $1 = rhs2.type_;
            if ($ instanceof TFloat && op instanceof Add && $1 instanceof TFloat) {
              return new Ok(
                new BinaryOp2(new TFloat(), lhs2, op, rhs2)
              );
            } else if ($ instanceof TInt && op instanceof Add && $1 instanceof TInt) {
              return new Ok(
                new BinaryOp2(new TInt(), lhs2, op, rhs2)
              );
            } else if (op instanceof Add) {
              return new Error(
                new TypeError2(
                  new IncorrectTypesForBinaryOp(
                    lhs2.type_,
                    rhs2.type_,
                    op
                  )
                )
              );
            } else if ($ instanceof TFloat && op instanceof Subtract && $1 instanceof TFloat) {
              return new Ok(
                new BinaryOp2(new TFloat(), lhs2, op, rhs2)
              );
            } else if ($ instanceof TInt && op instanceof Subtract && $1 instanceof TInt) {
              return new Ok(
                new BinaryOp2(new TInt(), lhs2, op, rhs2)
              );
            } else if ($ instanceof TFloat && op instanceof Multiply && $1 instanceof TFloat) {
              return new Ok(
                new BinaryOp2(new TFloat(), lhs2, op, rhs2)
              );
            } else if ($ instanceof TInt && op instanceof Multiply && $1 instanceof TInt) {
              return new Ok(
                new BinaryOp2(new TInt(), lhs2, op, rhs2)
              );
            } else if ($ instanceof TFloat && op instanceof Divide && $1 instanceof TFloat) {
              return new Ok(
                new BinaryOp2(new TFloat(), lhs2, op, rhs2)
              );
            } else if ($ instanceof TInt && op instanceof Divide && $1 instanceof TInt) {
              return new Ok(
                new BinaryOp2(new TInt(), lhs2, op, rhs2)
              );
            } else if ($ instanceof TFloat && op instanceof Power && $1 instanceof TFloat) {
              return new Ok(
                new BinaryOp2(new TFloat(), lhs2, op, rhs2)
              );
            } else if ($ instanceof TInt && op instanceof Power && $1 instanceof TFloat) {
              return new Ok(
                new BinaryOp2(new TFloat(), lhs2, op, rhs2)
              );
            } else if (op instanceof EqualCheck && isEqual($, $1)) {
              let t1 = $;
              let t2 = $1;
              return new Ok(
                new BinaryOp2(new TBool(), lhs2, op, rhs2)
              );
            } else if (op instanceof NotEqualCheck && isEqual($, $1)) {
              let t1 = $;
              let t2 = $1;
              return new Ok(
                new BinaryOp2(new TBool(), lhs2, op, rhs2)
              );
            } else if ($ instanceof TFloat && op instanceof LessThanCheck && $1 instanceof TFloat) {
              return new Ok(
                new BinaryOp2(new TBool(), lhs2, op, rhs2)
              );
            } else if ($ instanceof TFloat && op instanceof LessThanOrEqualCheck && $1 instanceof TFloat) {
              return new Ok(
                new BinaryOp2(new TBool(), lhs2, op, rhs2)
              );
            } else if ($ instanceof TFloat && op instanceof GreaterThanOrEqualCheck && $1 instanceof TFloat) {
              return new Ok(
                new BinaryOp2(new TBool(), lhs2, op, rhs2)
              );
            } else if ($ instanceof TFloat && op instanceof GreaterThanCheck && $1 instanceof TFloat) {
              return new Ok(
                new BinaryOp2(new TBool(), lhs2, op, rhs2)
              );
            } else if ($ instanceof TInt && op instanceof LessThanCheck && $1 instanceof TInt) {
              return new Ok(
                new BinaryOp2(new TBool(), lhs2, op, rhs2)
              );
            } else if ($ instanceof TInt && op instanceof LessThanOrEqualCheck && $1 instanceof TInt) {
              return new Ok(
                new BinaryOp2(new TBool(), lhs2, op, rhs2)
              );
            } else if ($ instanceof TInt && op instanceof GreaterThanOrEqualCheck && $1 instanceof TInt) {
              return new Ok(
                new BinaryOp2(new TBool(), lhs2, op, rhs2)
              );
            } else if ($ instanceof TInt && op instanceof GreaterThanCheck && $1 instanceof TInt) {
              return new Ok(
                new BinaryOp2(new TBool(), lhs2, op, rhs2)
              );
            } else if ($ instanceof TString && op instanceof LessThanCheck && $1 instanceof TString) {
              return new Ok(
                new BinaryOp2(new TBool(), lhs2, op, rhs2)
              );
            } else if ($ instanceof TString && op instanceof LessThanOrEqualCheck && $1 instanceof TString) {
              return new Ok(
                new BinaryOp2(new TBool(), lhs2, op, rhs2)
              );
            } else if ($ instanceof TString && op instanceof GreaterThanOrEqualCheck && $1 instanceof TString) {
              return new Ok(
                new BinaryOp2(new TBool(), lhs2, op, rhs2)
              );
            } else if ($ instanceof TString && op instanceof GreaterThanCheck && $1 instanceof TString) {
              return new Ok(
                new BinaryOp2(new TBool(), lhs2, op, rhs2)
              );
            } else if ($ instanceof TBool && op instanceof And && $1 instanceof TBool) {
              return new Ok(
                new BinaryOp2(new TBool(), lhs2, op, rhs2)
              );
            } else if ($ instanceof TBool && op instanceof Or && $1 instanceof TBool) {
              return new Ok(
                new BinaryOp2(new TBool(), lhs2, op, rhs2)
              );
            } else if (op instanceof And) {
              return new Error(
                new TypeError2(
                  new IncorrectTypesForBinaryOp(
                    lhs2.type_,
                    rhs2.type_,
                    op
                  )
                )
              );
            } else if (op instanceof MustBe && isEqual($1, $)) {
              let lht = $;
              let rht = $1;
              return new Ok(
                new BinaryOp2(rht, lhs2, new MustBe(), rhs2)
              );
            } else {
              let lht = $;
              let binary_op = op;
              let rht = $1;
              return new Error(
                new TypeError2(
                  new IncorrectTypesForBinaryOp(lht, rht, binary_op)
                )
              );
            }
          }
        );
      }
    );
  }
}

// build/dev/javascript/squared_away/squared_away/squared_away_lang.mjs
function interpret_grid(input2) {
  return map_values2(
    input2,
    (_, typed_expr) => {
      if (!typed_expr.isOk()) {
        let e = typed_expr[0];
        return new Error(e);
      } else {
        let typed_expr$1 = typed_expr[0];
        return interpret(input2, typed_expr$1);
      }
    }
  );
}
function typecheck_grid(input2) {
  return map_values2(
    input2,
    (_, expr) => {
      if (!expr.isOk()) {
        let e = expr[0];
        return new Error(e);
      } else {
        let expr$1 = expr[0];
        return typecheck(input2, expr$1);
      }
    }
  );
}
function parse_grid(input2) {
  return map_values2(
    input2,
    (key, toks) => {
      if (!toks.isOk()) {
        let e = toks[0];
        return new Error(e);
      } else {
        let toks$1 = toks[0];
        let toks$2 = (() => {
          let _pipe2 = toks$1;
          return map2(
            _pipe2,
            (t2) => {
              if (t2 instanceof BuiltinSum3 && t2.key instanceof None) {
                return new BuiltinSum3(new Some(key));
              } else {
                return t2;
              }
            }
          );
        })();
        let expr = parse3(toks$2);
        let _pipe = expr;
        return map_error(
          _pipe,
          (var0) => {
            return new ParseError2(var0);
          }
        );
      }
    }
  );
}
function scan_grid(input2) {
  return map_values2(
    input2,
    (_, src) => {
      let _pipe = scan(src);
      return map_error(
        _pipe,
        (var0) => {
          return new ScanError2(var0);
        }
      );
    }
  );
}

// build/dev/javascript/squared_away/squared_away_ffi.js
function focus(id2) {
  const input2 = document.getElementById(id2);
  input2.focus();
  const length4 = input2.value.length;
  setTimeout(() => {
    input2.setSelectionRange(length4, length4);
  }, 0);
}
function saveFile(content, filename) {
  const blob = new Blob([content], { type: "text/csv" });
  const url = URL.createObjectURL(blob);
  const link = document.createElement("a");
  link.href = url;
  link.download = filename;
  document.body.appendChild(link);
  link.click();
  document.body.removeChild(link);
  URL.revokeObjectURL(url);
}
function uploadFile() {
  const fileInput = document.getElementById("csvupload");
  const file = fileInput.files[0];
  return file.text();
}

// build/dev/javascript/squared_away/squared_away.mjs
var Model2 = class extends CustomType {
  constructor(holding_shift, grid_width, grid_height, display_mode, display_coords, active_cell, src_grid, value_grid, errors_to_display) {
    super();
    this.holding_shift = holding_shift;
    this.grid_width = grid_width;
    this.grid_height = grid_height;
    this.display_mode = display_mode;
    this.display_coords = display_coords;
    this.active_cell = active_cell;
    this.src_grid = src_grid;
    this.value_grid = value_grid;
    this.errors_to_display = errors_to_display;
  }
};
var DisplayValues = class extends CustomType {
};
var DisplayFormulas = class extends CustomType {
};
var Noop = class extends CustomType {
};
var UserToggledDisplayMode = class extends CustomType {
  constructor(to) {
    super();
    this.to = to;
  }
};
var UserToggledDisplayCoords = class extends CustomType {
  constructor(to) {
    super();
    this.to = to;
  }
};
var UserSetCellValue = class extends CustomType {
  constructor(key, val) {
    super();
    this.key = key;
    this.val = val;
  }
};
var UserFocusedOnCell = class extends CustomType {
  constructor(key) {
    super();
    this.key = key;
  }
};
var UserFocusedOffCell = class extends CustomType {
};
var UserHitKeyInCell = class extends CustomType {
  constructor(key, keyboard_key) {
    super();
    this.key = key;
    this.keyboard_key = keyboard_key;
  }
};
var UserReleasedKeyInCell = class extends CustomType {
  constructor(keyboard_key) {
    super();
    this.keyboard_key = keyboard_key;
  }
};
var UserClickedSaveBtn = class extends CustomType {
};
var UserUploadedFile = class extends CustomType {
  constructor(path) {
    super();
    this.path = path;
  }
};
var FileUploadComplete = class extends CustomType {
  constructor(file_content) {
    super();
    this.file_content = file_content;
  }
};
function update_grid(model) {
  let scanned = scan_grid(model.src_grid);
  let parsed = parse_grid(scanned);
  let typechecked = typecheck_grid(parsed);
  let value_grid = interpret_grid(typechecked);
  let errors_to_display = fold4(
    value_grid,
    toList([]),
    (acc, key, val) => {
      if (!val.isOk()) {
        let err = val[0];
        return prepend([key, err], acc);
      } else {
        return acc;
      }
    }
  );
  return model.withFields({
    value_grid,
    errors_to_display
  });
}
function set_active_cell_to(model, key) {
  if (!key.isOk()) {
    return [model, none()];
  } else {
    let key$1 = key[0];
    let id2 = to_string7(key$1);
    focus(id2);
    return [model.withFields({ active_cell: new Some(key$1) }), none()];
  }
}
function t(input2) {
  return toList([text2(input2)]);
}
function error_view(re) {
  return div(
    toList([
      style(
        toList([
          ["background-color", "#ffe6e6"],
          ["color", "#b30000"],
          ["padding", "20px"],
          ["border-radius", "20px"]
        ])
      )
    ]),
    prepend(
      h4(toList([]), t(re.title)),
      prepend(
        p(toList([]), t(re.info)),
        (() => {
          let $ = re.hint;
          if ($ instanceof None) {
            return toList([]);
          } else {
            let hint = $[0];
            return toList([p(toList([]), t(hint))]);
          }
        })()
      )
    )
  );
}
function view(model) {
  let error_to_display = (() => {
    let _pipe = find_map(
      model.errors_to_display,
      (e) => {
        let $ = isEqual(new Some(e[0]), model.active_cell);
        if (!$) {
          return new Error(void 0);
        } else {
          return new Ok(
            error_view(
              (() => {
                let _pipe2 = e[1];
                return to_renderable_error2(_pipe2);
              })()
            )
          );
        }
      }
    );
    return unwrap(_pipe, div(toList([]), toList([])));
  })();
  let rows = (() => {
    let _pipe = model.src_grid.cells;
    let _pipe$1 = group(_pipe, row);
    let _pipe$2 = map_values(
      _pipe$1,
      (_, keys2) => {
        let cells = (() => {
          let _pipe$22 = sort(
            keys2,
            (k1, k2) => {
              return compare(
                (() => {
                  let _pipe$23 = k1;
                  return col(_pipe$23);
                })(),
                (() => {
                  let _pipe$23 = k2;
                  return col(_pipe$23);
                })()
              );
            }
          );
          return map2(
            _pipe$22,
            (key) => {
              let on_keydown2 = on_keydown(
                (_capture) => {
                  return new UserHitKeyInCell(key, _capture);
                }
              );
              let on_keyup2 = on_keyup(
                (var0) => {
                  return new UserReleasedKeyInCell(var0);
                }
              );
              let on_input2 = on_input(
                (_capture) => {
                  return new UserSetCellValue(key, _capture);
                }
              );
              let out_of_focus = on_blur(new UserFocusedOffCell());
              let on_focus2 = on_focus(new UserFocusedOnCell(key));
              let id2 = id(to_string7(key));
              let value3 = (() => {
                let _pipe$32 = (() => {
                  let $2 = model.display_mode;
                  let $12 = isEqual(model.active_cell, new Some(key));
                  if ($2 instanceof DisplayFormulas) {
                    return get4(model.src_grid, key);
                  } else if ($2 instanceof DisplayValues && $12) {
                    return get4(model.src_grid, key);
                  } else {
                    let $22 = get4(model.value_grid, key);
                    if (!$22.isOk()) {
                      return get4(model.src_grid, key);
                    } else {
                      let v = $22[0];
                      return value_to_string(v);
                    }
                  }
                })();
                return value(_pipe$32);
              })();
              let cell_is_errored = any(
                model.errors_to_display,
                (i) => {
                  return isEqual(i[0], key);
                }
              );
              let error_class = (() => {
                if (!cell_is_errored) {
                  return none2();
                } else {
                  return class$("errorcell");
                }
              })();
              let $ = (() => {
                let $12 = get4(model.value_grid, key);
                if (!$12.isOk()) {
                  return ["#b30000", "#ffe6e6"];
                } else {
                  let v = $12[0];
                  if (v instanceof Text2) {
                    return ["#4a4a4a", "#f2f2f2"];
                  } else if (v instanceof TestPass) {
                    return ["#006400", "#e6ffe6"];
                  } else if (v instanceof TestFail) {
                    return ["#b30000", "#ffe6e6"];
                  } else {
                    return ["black", "white"];
                  }
                }
              })();
              let color = $[0];
              let background_color = $[1];
              let input2 = input(
                toList([
                  on_input2,
                  on_focus2,
                  out_of_focus,
                  value3,
                  on_keydown2,
                  on_keyup2,
                  id2,
                  type_("text"),
                  error_class,
                  style(
                    toList([
                      ["background-color", background_color],
                      ["color", color]
                    ])
                  )
                ])
              );
              let $1 = model.display_coords;
              if (!$1) {
                return td(toList([]), toList([input2]));
              } else {
                return td(
                  toList([]),
                  toList([
                    label(toList([]), t(to_string7(key) + ": ")),
                    input2
                  ])
                );
              }
            }
          );
        })();
        return tr(toList([]), cells);
      }
    );
    let _pipe$3 = map_to_list(_pipe$2);
    let _pipe$4 = sort(
      _pipe$3,
      (r1, r2) => {
        return compare(r1[0], r2[0]);
      }
    );
    return map2(_pipe$4, (e) => {
      return e[1];
    });
  })();
  let grid = div(
    toList([class$("table-container")]),
    toList([
      table(
        toList([class$("tg")]),
        toList([tbody(toList([]), rows)])
      )
    ])
  );
  let formula_mode_toggle = input(
    toList([
      type_("radio"),
      name("display_mode"),
      id("formula_mode"),
      on_check(
        (_) => {
          return new UserToggledDisplayMode(new DisplayFormulas());
        }
      )
    ])
  );
  let formula_mode_toggle_label = label(
    toList([for$("formula_mode")]),
    t("Show formulas")
  );
  let grid_mode_toggle = input(
    toList([
      type_("checkbox"),
      id("grid_mode"),
      on_check((var0) => {
        return new UserToggledDisplayCoords(var0);
      })
    ])
  );
  let grid_mode_toggle_label = label(
    toList([for$("grid_mode")]),
    t("Show grid coordinates")
  );
  let value_mode_toggle = input(
    toList([
      type_("radio"),
      name("display_mode"),
      id("value_mode"),
      on_check(
        (_) => {
          return new UserToggledDisplayMode(new DisplayValues());
        }
      )
    ])
  );
  let value_mode_toggle_label = label(
    toList([for$("value_mode")]),
    t("Show evaluated values")
  );
  let save_button = button(
    toList([on_click(new UserClickedSaveBtn())]),
    t("Save")
  );
  let load_label = label(toList([]), t("Load file"));
  let load_button = input(
    toList([
      type_("file"),
      id("csvupload"),
      on_input((var0) => {
        return new UserUploadedFile(var0);
      })
    ])
  );
  return div(
    toList([]),
    toList([
      value_mode_toggle,
      value_mode_toggle_label,
      formula_mode_toggle,
      formula_mode_toggle_label,
      grid_mode_toggle,
      grid_mode_toggle_label,
      save_button,
      load_label,
      load_button,
      br(toList([])),
      br(toList([])),
      grid,
      br(toList([])),
      error_to_display
    ])
  );
}
var initial_grid_width = 7;
var initial_grid_height = 20;
function init2(_) {
  let src_grid = new$4(initial_grid_width, initial_grid_height, "");
  let value_grid = new$4(
    initial_grid_width,
    initial_grid_height,
    new Ok(new Empty3())
  );
  let model = (() => {
    let _pipe = new Model2(
      false,
      initial_grid_width,
      initial_grid_height,
      new DisplayValues(),
      false,
      new None(),
      src_grid,
      value_grid,
      toList([])
    );
    return update_grid(_pipe);
  })();
  return [model, none()];
}
function update(model, msg) {
  if (msg instanceof Noop) {
    return [model, none()];
  } else if (msg instanceof UserSetCellValue) {
    let key = msg.key;
    let val = msg.val;
    let model$1 = model.withFields({
      src_grid: insert4(model.src_grid, key, val)
    });
    return [update_grid(model$1), none()];
  } else if (msg instanceof UserToggledDisplayMode) {
    let display_mode = msg.to;
    return [model.withFields({ display_mode }), none()];
  } else if (msg instanceof UserToggledDisplayCoords) {
    let display_coords = msg.to;
    return [
      model.withFields({ display_coords }),
      none()
    ];
  } else if (msg instanceof UserFocusedOnCell) {
    let key = msg.key;
    return [model.withFields({ active_cell: new Some(key) }), none()];
  } else if (msg instanceof UserFocusedOffCell) {
    return [model.withFields({ active_cell: new None() }), none()];
  } else if (msg instanceof UserHitKeyInCell) {
    let key = msg.key;
    let keyboard_key = msg.keyboard_key;
    let $ = model.holding_shift;
    if (keyboard_key === "Shift" && !$) {
      return [model.withFields({ holding_shift: true }), none()];
    } else if (keyboard_key === "ArrowUp") {
      return set_active_cell_to(model, cell_above(model.src_grid, key));
    } else if (keyboard_key === "ArrowLeft") {
      return set_active_cell_to(
        model,
        cell_to_the_left(model.src_grid, key)
      );
    } else if (keyboard_key === "Enter") {
      return set_active_cell_to(
        model,
        cell_underneath(model.src_grid, key)
      );
    } else if (keyboard_key === "ArrowDown" && !$) {
      return set_active_cell_to(
        model,
        cell_underneath(model.src_grid, key)
      );
    } else if (keyboard_key === "ArrowRight" && !$) {
      return set_active_cell_to(
        model,
        cell_to_the_right(model.src_grid, key)
      );
    } else if (keyboard_key === "ArrowRight" && $) {
      let maybe_cell_to_right = cell_to_the_right(model.src_grid, key);
      if (!maybe_cell_to_right.isOk() && !maybe_cell_to_right[0]) {
        return [model, none()];
      } else {
        let cell_to_right = maybe_cell_to_right[0];
        let scanned = scan_grid(model.src_grid);
        let parsed = parse_grid(scanned);
        let typechecked = typecheck_grid(parsed);
        let maybe_expr = get4(typechecked, key);
        return guard(
          (() => {
            let _pipe = maybe_expr;
            return is_error(_pipe);
          })(),
          [model, none()],
          () => {
            if (!maybe_expr.isOk()) {
              throw makeError(
                "let_assert",
                "squared_away",
                167,
                "",
                "Pattern match failed, no pattern matched the value.",
                { value: maybe_expr }
              );
            }
            let expr = maybe_expr[0];
            let expr_with_labels_updated = visit_cross_labels(
              expr,
              (key2, row_label, col_label) => {
                let $1 = find3(model.src_grid, col_label);
                if (!$1.isOk()) {
                  throw makeError(
                    "let_assert",
                    "squared_away",
                    177,
                    "",
                    "Pattern match failed, no pattern matched the value.",
                    { value: $1 }
                  );
                }
                let key_for_col = $1[0];
                return try$(
                  cell_to_the_right(model.src_grid, key_for_col),
                  (key_for_new_col) => {
                    let maybe_new_label = get4(
                      typechecked,
                      key_for_new_col
                    );
                    let get_new_label = (l) => {
                      if (l.isOk() && l[0] instanceof LabelDef2) {
                        let txt = l[0].txt;
                        return new Ok(txt);
                      } else {
                        return new Error(void 0);
                      }
                    };
                    return try$(
                      get_new_label(maybe_new_label),
                      (new_label) => {
                        let $2 = cell_to_the_right(model.src_grid, key2);
                        if (!$2.isOk()) {
                          throw makeError(
                            "let_assert",
                            "squared_away",
                            198,
                            "",
                            "Pattern match failed, no pattern matched the value.",
                            { value: $2 }
                          );
                        }
                        let new_key = $2[0];
                        return new Ok(
                          new CrossLabel2(
                            expr.type_,
                            new_key,
                            row_label,
                            new_label
                          )
                        );
                      }
                    );
                  }
                );
              }
            );
            if (!expr_with_labels_updated.isOk()) {
              return [model, none()];
            } else {
              let new_expr = expr_with_labels_updated[0];
              let formula = "=" + to_string9(new_expr);
              let src_grid = insert4(
                model.src_grid,
                cell_to_right,
                formula
              );
              let id2 = to_string7(cell_to_right);
              focus(id2);
              let new_model = model.withFields({
                src_grid,
                active_cell: new Some(cell_to_right)
              });
              return [update_grid(new_model), none()];
            }
          }
        );
      }
    } else if (keyboard_key === "ArrowDown" && $) {
      let maybe_cell_below = cell_underneath(model.src_grid, key);
      if (!maybe_cell_below.isOk() && !maybe_cell_below[0]) {
        return [model, none()];
      } else {
        let cell_below = maybe_cell_below[0];
        let scanned = scan_grid(model.src_grid);
        let parsed = parse_grid(scanned);
        let typechecked = typecheck_grid(parsed);
        let maybe_expr = get4(typechecked, key);
        return guard(
          (() => {
            let _pipe = maybe_expr;
            return is_error(_pipe);
          })(),
          [model, none()],
          () => {
            if (!maybe_expr.isOk()) {
              throw makeError(
                "let_assert",
                "squared_away",
                241,
                "",
                "Pattern match failed, no pattern matched the value.",
                { value: maybe_expr }
              );
            }
            let expr = maybe_expr[0];
            let expr_with_labels_updated = visit_cross_labels(
              expr,
              (key2, row_label, col_label) => {
                let $1 = find3(model.src_grid, row_label);
                if (!$1.isOk()) {
                  throw makeError(
                    "let_assert",
                    "squared_away",
                    248,
                    "",
                    "Pattern match failed, no pattern matched the value.",
                    { value: $1 }
                  );
                }
                let key_for_row = $1[0];
                return try$(
                  cell_underneath(model.src_grid, key_for_row),
                  (key_for_new_row) => {
                    let maybe_new_label = get4(
                      typechecked,
                      key_for_new_row
                    );
                    let get_new_label = (l) => {
                      if (l.isOk() && l[0] instanceof LabelDef2) {
                        let txt = l[0].txt;
                        return new Ok(txt);
                      } else {
                        return new Error(void 0);
                      }
                    };
                    return try$(
                      get_new_label(maybe_new_label),
                      (new_label) => {
                        let $2 = cell_underneath(model.src_grid, key2);
                        if (!$2.isOk()) {
                          throw makeError(
                            "let_assert",
                            "squared_away",
                            268,
                            "",
                            "Pattern match failed, no pattern matched the value.",
                            { value: $2 }
                          );
                        }
                        let new_key = $2[0];
                        return new Ok(
                          new CrossLabel2(
                            expr.type_,
                            new_key,
                            new_label,
                            col_label
                          )
                        );
                      }
                    );
                  }
                );
              }
            );
            if (!expr_with_labels_updated.isOk()) {
              return [model, none()];
            } else {
              let new_expr = expr_with_labels_updated[0];
              let formula = "=" + to_string9(new_expr);
              let src_grid = insert4(model.src_grid, cell_below, formula);
              let id2 = to_string7(cell_below);
              focus(id2);
              let new_model = model.withFields({
                src_grid,
                active_cell: new Some(cell_below)
              });
              return [update_grid(new_model), none()];
            }
          }
        );
      }
    } else {
      return [model, none()];
    }
  } else if (msg instanceof UserReleasedKeyInCell) {
    let keyboard_key = msg.keyboard_key;
    if (keyboard_key === "Shift") {
      return [model.withFields({ holding_shift: false }), none()];
    } else {
      return [model, none()];
    }
  } else if (msg instanceof UserClickedSaveBtn) {
    let content = src_csv(model.src_grid);
    saveFile(content, "myspreadsheet.csv");
    return [model, none()];
  } else if (msg instanceof UserUploadedFile) {
    let get_file_contents_effect = from(
      (dispatch) => {
        then_await(
          uploadFile(),
          (file_content) => {
            return newPromise(
              (resolve2) => {
                dispatch(new FileUploadComplete(file_content));
                return resolve2(void 0);
              }
            );
          }
        );
        return void 0;
      }
    );
    return [model, get_file_contents_effect];
  } else {
    let file_content = msg.file_content;
    let src_grid = (() => {
      let _pipe = file_content;
      return from_src_csv(_pipe, initial_grid_width, initial_grid_height);
    })();
    let new_model = (() => {
      let _pipe = model.withFields({ src_grid });
      return update_grid(_pipe);
    })();
    return [new_model, none()];
  }
}
function main() {
  let app = application(init2, update, view);
  let $ = start2(app, "#app", void 0);
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "squared_away",
      27,
      "main",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    );
  }
  return void 0;
}

// build/.lustre/entry.mjs
main();
