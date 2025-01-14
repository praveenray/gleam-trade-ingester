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
  // @internal
  countLength() {
    let length4 = 0;
    for (let _ of this)
      length4++;
    return length4;
  }
};
function prepend(element3, tail) {
  return new NonEmpty(element3, tail);
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
  constructor(value4) {
    this.value = value4;
  }
};
function byteArrayToInt(byteArray, start3, end, isBigEndian, isSigned) {
  let value4 = 0;
  if (isBigEndian) {
    for (let i = start3; i < end; i++) {
      value4 = value4 * 256 + byteArray[i];
    }
  } else {
    for (let i = end - 1; i >= start3; i--) {
      value4 = value4 * 256 + byteArray[i];
    }
  }
  if (isSigned) {
    const byteSize = end - start3;
    const highBit = 2 ** (byteSize * 8 - 1);
    if (value4 >= highBit) {
      value4 -= highBit * 2;
    }
  }
  return value4;
}
function byteArrayToFloat(byteArray, start3, end, isBigEndian) {
  const view3 = new DataView(byteArray.buffer);
  const byteSize = end - start3;
  if (byteSize === 8) {
    return view3.getFloat64(start3, !isBigEndian);
  } else if (byteSize === 4) {
    return view3.getFloat32(start3, !isBigEndian);
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
  constructor(value4) {
    super();
    this[0] = value4;
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
    let a2 = values.pop();
    let b = values.pop();
    if (a2 === b)
      continue;
    if (!isObject(a2) || !isObject(b))
      return false;
    let unequal = !structurallyCompatibleObjects(a2, b) || unequalDates(a2, b) || unequalBuffers(a2, b) || unequalArrays(a2, b) || unequalMaps(a2, b) || unequalSets(a2, b) || unequalRegExps(a2, b);
    if (unequal)
      return false;
    const proto = Object.getPrototypeOf(a2);
    if (proto !== null && typeof proto.equals === "function") {
      try {
        if (a2.equals(b))
          continue;
        else
          return false;
      } catch {
      }
    }
    let [keys2, get2] = getters(a2);
    for (let k of keys2(a2)) {
      values.push(get2(a2, k), get2(b, k));
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
function unequalDates(a2, b) {
  return a2 instanceof Date && (a2 > b || a2 < b);
}
function unequalBuffers(a2, b) {
  return a2.buffer instanceof ArrayBuffer && a2.BYTES_PER_ELEMENT && !(a2.byteLength === b.byteLength && a2.every((n, i) => n === b[i]));
}
function unequalArrays(a2, b) {
  return Array.isArray(a2) && a2.length !== b.length;
}
function unequalMaps(a2, b) {
  return a2 instanceof Map && a2.size !== b.size;
}
function unequalSets(a2, b) {
  return a2 instanceof Set && (a2.size != b.size || [...a2].some((e) => !b.has(e)));
}
function unequalRegExps(a2, b) {
  return a2 instanceof RegExp && (a2.source !== b.source || a2.flags !== b.flags);
}
function isObject(a2) {
  return typeof a2 === "object" && a2 !== null;
}
function structurallyCompatibleObjects(a2, b) {
  if (typeof a2 !== "object" && typeof b !== "object" && (!a2 || !b))
    return false;
  let nonstructural = [Promise, WeakSet, WeakMap, Function];
  if (nonstructural.some((c) => a2 instanceof c))
    return false;
  return a2.constructor === b.constructor;
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
function to_result(option2, e) {
  if (option2 instanceof Some) {
    let a2 = option2[0];
    return new Ok(a2);
  } else {
    return new Error(e);
  }
}
function from_result(result) {
  if (result.isOk()) {
    let a2 = result[0];
    return new Some(a2);
  } else {
    return new None();
  }
}
function unwrap(option2, default$) {
  if (option2 instanceof Some) {
    let x = option2[0];
    return x;
  } else {
    return default$;
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
function hashMerge(a2, b) {
  return a2 ^ b + 2654435769 + (a2 << 6) + (a2 >> 2) | 0;
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
function cloneAndSet(arr, at2, val) {
  const len = arr.length;
  const out = new Array(len);
  for (let i = 0; i < len; ++i) {
    out[i] = arr[i];
  }
  out[at2] = val;
  return out;
}
function spliceIn(arr, at2, val) {
  const len = arr.length;
  const out = new Array(len + 1);
  let i = 0;
  let g = 0;
  while (i < at2) {
    out[g++] = arr[i++];
  }
  out[g++] = val;
  while (i < len) {
    out[g++] = arr[i++];
  }
  return out;
}
function spliceOut(arr, at2) {
  const len = arr.length;
  const out = new Array(len - 1);
  let i = 0;
  let g = 0;
  while (i < at2) {
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
      const newArray = spliceIn(root.array, idx, {
        type: ENTRY,
        k: key,
        v: val
      });
      addedLeaf.val = true;
      return {
        type: INDEX_NODE,
        bitmap: root.bitmap | bit,
        array: newArray
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
    const size2 = root.array.length;
    addedLeaf.val = true;
    return {
      type: COLLISION_NODE,
      hash,
      array: cloneAndSet(root.array, size2, { type: ENTRY, k: key, v: val })
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
  const size2 = root.array.length;
  for (let i = 0; i < size2; i++) {
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
  const size2 = items.length;
  for (let i = 0; i < size2; i++) {
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
  constructor(root, size2) {
    this.root = root;
    this.size = size2;
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
    try {
      this.forEach((v, k) => {
        if (!isEqual(o.get(k, !v), v)) {
          throw unequalDictSymbol;
        }
      });
      return true;
    } catch (e) {
      if (e === unequalDictSymbol) {
        return false;
      }
      throw e;
    }
  }
};
var unequalDictSymbol = Symbol();

// build/dev/javascript/gleam_stdlib/gleam_stdlib.mjs
var Nil = void 0;
var NOT_FOUND = {};
function identity(x) {
  return x;
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
function pop_grapheme(string3) {
  let first5;
  const iterator = graphemes_iterator(string3);
  if (iterator) {
    first5 = iterator.next().value?.segment;
  } else {
    first5 = string3.match(/./su)?.[0];
  }
  if (first5) {
    return new Ok([first5, string3.slice(first5.length)]);
  } else {
    return new Error(Nil);
  }
}
function pop_codeunit(str) {
  return [str.charCodeAt(0) | 0, str.slice(1)];
}
function lowercase(string3) {
  return string3.toLowerCase();
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
function string_codeunit_slice(str, from2, length4) {
  return str.slice(from2, from2 + length4);
}
function contains_string(haystack, needle) {
  return haystack.indexOf(needle) >= 0;
}
function starts_with(haystack, needle) {
  return haystack.startsWith(needle);
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
var trim_start_regex = new RegExp(`^[${unicode_whitespaces}]*`);
var trim_end_regex = new RegExp(`[${unicode_whitespaces}]*$`);
function trim_start(string3) {
  return string3.replace(trim_start_regex, "");
}
function trim_end(string3) {
  return string3.replace(trim_end_regex, "");
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
function new_map() {
  return Dict.new();
}
function map_to_list(map6) {
  return List.fromArray(map6.entries());
}
function map_get(map6, key) {
  const value4 = map6.get(key, NOT_FOUND);
  if (value4 === NOT_FOUND) {
    return new Error(Nil);
  }
  return new Ok(value4);
}
function map_insert(key, value4, map6) {
  return map6.set(key, value4);
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
function decode_list(data) {
  if (Array.isArray(data)) {
    return new Ok(List.fromArray(data));
  }
  return data instanceof List ? new Ok(data) : decoder_error("List", data);
}
function decode_field(value4, name4) {
  const not_a_map_error = () => decoder_error("Dict", value4);
  if (value4 instanceof Dict || value4 instanceof WeakMap || value4 instanceof Map) {
    const entry = map_get(value4, name4);
    return new Ok(entry.isOk() ? new Some(entry[0]) : new None());
  } else if (value4 === null) {
    return not_a_map_error();
  } else if (Object.getPrototypeOf(value4) == Object.prototype) {
    return try_get_field(value4, name4, () => new Ok(new None()));
  } else {
    return try_get_field(value4, name4, not_a_map_error);
  }
}
function try_get_field(value4, field2, or_else) {
  try {
    return field2 in value4 ? new Ok(new Some(value4[field2])) : or_else();
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
  if (t === "bigint" || Number.isInteger(v))
    return v.toString();
  if (t === "number")
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
  let first5 = true;
  map6.forEach((value4, key) => {
    if (!first5)
      body = body + ", ";
    body = body + "#(" + inspect(key) + ", " + inspect(value4) + ")";
    first5 = false;
  });
  return body + "])";
}
function inspectObject(v) {
  const name4 = Object.getPrototypeOf(v)?.constructor?.name || "Object";
  const props = [];
  for (const k of Object.keys(v)) {
    props.push(`${inspect(k)}: ${inspect(v[k])}`);
  }
  const body = props.length ? " " + props.join(", ") + " " : "";
  const head = name4 === "Object" ? "" : name4 + " ";
  return `//js(${head}{${body}})`;
}
function inspectCustomType(record) {
  const props = Object.keys(record).map((label2) => {
    const value4 = inspect(record[label2]);
    return isNaN(parseInt(label2)) ? `${label2}: ${value4}` : value4;
  }).join(", ");
  return props ? `${record.constructor.name}(${props})` : record.constructor.name;
}
function inspectList(list3) {
  return `[${list3.toArray().map(inspect).join(", ")}]`;
}
function inspectBitArray(bits) {
  return `<<${Array.from(bits.buffer).join(", ")}>>`;
}
function inspectUtfCodepoint(codepoint2) {
  return `//utfcodepoint(${String.fromCodePoint(codepoint2.value)})`;
}

// build/dev/javascript/gleam_stdlib/gleam/int.mjs
function compare(a2, b) {
  let $ = a2 === b;
  if ($) {
    return new Eq();
  } else {
    let $1 = a2 < b;
    if ($1) {
      return new Lt();
    } else {
      return new Gt();
    }
  }
}
function add(a2, b) {
  return a2 + b;
}
function subtract(a2, b) {
  return a2 - b;
}

// build/dev/javascript/gleam_stdlib/gleam/dict.mjs
function insert(dict2, key, value4) {
  return map_insert(key, value4, dict2);
}
function from_list_loop(loop$list, loop$initial) {
  while (true) {
    let list3 = loop$list;
    let initial = loop$initial;
    if (list3.hasLength(0)) {
      return initial;
    } else {
      let x = list3.head;
      let rest2 = list3.tail;
      loop$list = rest2;
      loop$initial = insert(initial, x[0], x[1]);
    }
  }
}
function from_list(list3) {
  return from_list_loop(list3, new_map());
}
function reverse_and_concat(loop$remaining, loop$accumulator) {
  while (true) {
    let remaining = loop$remaining;
    let accumulator = loop$accumulator;
    if (remaining.hasLength(0)) {
      return accumulator;
    } else {
      let item = remaining.head;
      let rest2 = remaining.tail;
      loop$remaining = rest2;
      loop$accumulator = prepend(item, accumulator);
    }
  }
}
function do_keys_loop(loop$list, loop$acc) {
  while (true) {
    let list3 = loop$list;
    let acc = loop$acc;
    if (list3.hasLength(0)) {
      return reverse_and_concat(acc, toList([]));
    } else {
      let first5 = list3.head;
      let rest2 = list3.tail;
      loop$list = rest2;
      loop$acc = prepend(first5[0], acc);
    }
  }
}
function keys(dict2) {
  let list_of_pairs = map_to_list(dict2);
  return do_keys_loop(list_of_pairs, toList([]));
}

// build/dev/javascript/gleam_stdlib/gleam/pair.mjs
function first(pair) {
  let a2 = pair[0];
  return a2;
}
function second(pair) {
  let a2 = pair[1];
  return a2;
}
function map_second(pair, fun) {
  let a2 = pair[0];
  let b = pair[1];
  return [a2, fun(b)];
}

// build/dev/javascript/gleam_stdlib/gleam/list.mjs
var Ascending = class extends CustomType {
};
var Descending = class extends CustomType {
};
function reverse_loop(loop$remaining, loop$accumulator) {
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
function reverse(list3) {
  return reverse_loop(list3, toList([]));
}
function first2(list3) {
  if (list3.hasLength(0)) {
    return new Error(void 0);
  } else {
    let x = list3.head;
    return new Ok(x);
  }
}
function filter_loop(loop$list, loop$fun, loop$acc) {
  while (true) {
    let list3 = loop$list;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list3.hasLength(0)) {
      return reverse(acc);
    } else {
      let first$1 = list3.head;
      let rest$1 = list3.tail;
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
function filter(list3, predicate) {
  return filter_loop(list3, predicate, toList([]));
}
function map_loop(loop$list, loop$fun, loop$acc) {
  while (true) {
    let list3 = loop$list;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list3.hasLength(0)) {
      return reverse(acc);
    } else {
      let first$1 = list3.head;
      let rest$1 = list3.tail;
      loop$list = rest$1;
      loop$fun = fun;
      loop$acc = prepend(fun(first$1), acc);
    }
  }
}
function map(list3, fun) {
  return map_loop(list3, fun, toList([]));
}
function try_map_loop(loop$list, loop$fun, loop$acc) {
  while (true) {
    let list3 = loop$list;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list3.hasLength(0)) {
      return new Ok(reverse(acc));
    } else {
      let first$1 = list3.head;
      let rest$1 = list3.tail;
      let $ = fun(first$1);
      if ($.isOk()) {
        let first$2 = $[0];
        loop$list = rest$1;
        loop$fun = fun;
        loop$acc = prepend(first$2, acc);
      } else {
        let error = $[0];
        return new Error(error);
      }
    }
  }
}
function try_map(list3, fun) {
  return try_map_loop(list3, fun, toList([]));
}
function append_loop(loop$first, loop$second) {
  while (true) {
    let first5 = loop$first;
    let second2 = loop$second;
    if (first5.hasLength(0)) {
      return second2;
    } else {
      let item = first5.head;
      let rest$1 = first5.tail;
      loop$first = rest$1;
      loop$second = prepend(item, second2);
    }
  }
}
function append(first5, second2) {
  return append_loop(reverse(first5), second2);
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
function concat_loop(loop$lists, loop$acc) {
  while (true) {
    let lists = loop$lists;
    let acc = loop$acc;
    if (lists.hasLength(0)) {
      return reverse(acc);
    } else {
      let list3 = lists.head;
      let further_lists = lists.tail;
      loop$lists = further_lists;
      loop$acc = reverse_and_prepend(list3, acc);
    }
  }
}
function flatten(lists) {
  return concat_loop(lists, toList([]));
}
function fold(loop$list, loop$initial, loop$fun) {
  while (true) {
    let list3 = loop$list;
    let initial = loop$initial;
    let fun = loop$fun;
    if (list3.hasLength(0)) {
      return initial;
    } else {
      let x = list3.head;
      let rest$1 = list3.tail;
      loop$list = rest$1;
      loop$initial = fun(initial, x);
      loop$fun = fun;
    }
  }
}
function fold_right(list3, initial, fun) {
  if (list3.hasLength(0)) {
    return initial;
  } else {
    let x = list3.head;
    let rest$1 = list3.tail;
    return fun(fold_right(rest$1, initial, fun), x);
  }
}
function index_fold_loop(loop$over, loop$acc, loop$with, loop$index) {
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
function index_fold(list3, initial, fun) {
  return index_fold_loop(list3, initial, fun, 0);
}
function sequences(loop$list, loop$compare, loop$growing, loop$direction, loop$prev, loop$acc) {
  while (true) {
    let list3 = loop$list;
    let compare4 = loop$compare;
    let growing = loop$growing;
    let direction = loop$direction;
    let prev = loop$prev;
    let acc = loop$acc;
    let growing$1 = prepend(prev, growing);
    if (list3.hasLength(0)) {
      if (direction instanceof Ascending) {
        return prepend(reverse_loop(growing$1, toList([])), acc);
      } else {
        return prepend(growing$1, acc);
      }
    } else {
      let new$1 = list3.head;
      let rest$1 = list3.tail;
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
            return prepend(reverse_loop(growing$1, toList([])), acc);
          } else {
            return prepend(growing$1, acc);
          }
        })();
        if (rest$1.hasLength(0)) {
          return prepend(toList([new$1]), acc$1);
        } else {
          let next2 = rest$1.head;
          let rest$2 = rest$1.tail;
          let direction$1 = (() => {
            let $1 = compare4(new$1, next2);
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
          loop$prev = next2;
          loop$acc = acc$1;
        }
      } else if ($ instanceof Lt && direction instanceof Descending) {
        let acc$1 = (() => {
          if (direction instanceof Ascending) {
            return prepend(reverse_loop(growing$1, toList([])), acc);
          } else {
            return prepend(growing$1, acc);
          }
        })();
        if (rest$1.hasLength(0)) {
          return prepend(toList([new$1]), acc$1);
        } else {
          let next2 = rest$1.head;
          let rest$2 = rest$1.tail;
          let direction$1 = (() => {
            let $1 = compare4(new$1, next2);
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
          loop$prev = next2;
          loop$acc = acc$1;
        }
      } else {
        let acc$1 = (() => {
          if (direction instanceof Ascending) {
            return prepend(reverse_loop(growing$1, toList([])), acc);
          } else {
            return prepend(growing$1, acc);
          }
        })();
        if (rest$1.hasLength(0)) {
          return prepend(toList([new$1]), acc$1);
        } else {
          let next2 = rest$1.head;
          let rest$2 = rest$1.tail;
          let direction$1 = (() => {
            let $1 = compare4(new$1, next2);
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
          loop$prev = next2;
          loop$acc = acc$1;
        }
      }
    }
  }
}
function merge_ascendings(loop$list1, loop$list2, loop$compare, loop$acc) {
  while (true) {
    let list1 = loop$list1;
    let list22 = loop$list2;
    let compare4 = loop$compare;
    let acc = loop$acc;
    if (list1.hasLength(0)) {
      let list3 = list22;
      return reverse_loop(list3, acc);
    } else if (list22.hasLength(0)) {
      let list3 = list1;
      return reverse_loop(list3, acc);
    } else {
      let first1 = list1.head;
      let rest1 = list1.tail;
      let first22 = list22.head;
      let rest2 = list22.tail;
      let $ = compare4(first1, first22);
      if ($ instanceof Lt) {
        loop$list1 = rest1;
        loop$list2 = list22;
        loop$compare = compare4;
        loop$acc = prepend(first1, acc);
      } else if ($ instanceof Gt) {
        loop$list1 = list1;
        loop$list2 = rest2;
        loop$compare = compare4;
        loop$acc = prepend(first22, acc);
      } else {
        loop$list1 = list1;
        loop$list2 = rest2;
        loop$compare = compare4;
        loop$acc = prepend(first22, acc);
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
      return reverse_loop(acc, toList([]));
    } else if (sequences2.hasLength(1)) {
      let sequence = sequences2.head;
      return reverse_loop(
        prepend(reverse_loop(sequence, toList([])), acc),
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
    let list22 = loop$list2;
    let compare4 = loop$compare;
    let acc = loop$acc;
    if (list1.hasLength(0)) {
      let list3 = list22;
      return reverse_loop(list3, acc);
    } else if (list22.hasLength(0)) {
      let list3 = list1;
      return reverse_loop(list3, acc);
    } else {
      let first1 = list1.head;
      let rest1 = list1.tail;
      let first22 = list22.head;
      let rest2 = list22.tail;
      let $ = compare4(first1, first22);
      if ($ instanceof Lt) {
        loop$list1 = list1;
        loop$list2 = rest2;
        loop$compare = compare4;
        loop$acc = prepend(first22, acc);
      } else if ($ instanceof Gt) {
        loop$list1 = rest1;
        loop$list2 = list22;
        loop$compare = compare4;
        loop$acc = prepend(first1, acc);
      } else {
        loop$list1 = rest1;
        loop$list2 = list22;
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
      return reverse_loop(acc, toList([]));
    } else if (sequences2.hasLength(1)) {
      let sequence = sequences2.head;
      return reverse_loop(
        prepend(reverse_loop(sequence, toList([])), acc),
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
      return reverse_loop(sequence, toList([]));
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
function sort(list3, compare4) {
  if (list3.hasLength(0)) {
    return toList([]);
  } else if (list3.hasLength(1)) {
    let x = list3.head;
    return toList([x]);
  } else {
    let x = list3.head;
    let y = list3.tail.head;
    let rest$1 = list3.tail.tail;
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
function key_set(list3, key, value4) {
  if (list3.hasLength(0)) {
    return toList([[key, value4]]);
  } else if (list3.atLeastLength(1) && isEqual(list3.head[0], key)) {
    let k = list3.head[0];
    let rest$1 = list3.tail;
    return prepend([key, value4], rest$1);
  } else {
    let first$1 = list3.head;
    let rest$1 = list3.tail;
    return prepend(first$1, key_set(rest$1, key, value4));
  }
}
function each(loop$list, loop$f) {
  while (true) {
    let list3 = loop$list;
    let f = loop$f;
    if (list3.hasLength(0)) {
      return void 0;
    } else {
      let first$1 = list3.head;
      let rest$1 = list3.tail;
      f(first$1);
      loop$list = rest$1;
      loop$f = f;
    }
  }
}

// build/dev/javascript/gleam_stdlib/gleam/string.mjs
function concat2(strings) {
  let _pipe = strings;
  let _pipe$1 = concat(_pipe);
  return identity(_pipe$1);
}
function trim(string3) {
  let _pipe = string3;
  let _pipe$1 = trim_start(_pipe);
  return trim_end(_pipe$1);
}
function drop_start(loop$string, loop$num_graphemes) {
  while (true) {
    let string3 = loop$string;
    let num_graphemes = loop$num_graphemes;
    let $ = num_graphemes > 0;
    if (!$) {
      return string3;
    } else {
      let $1 = pop_grapheme(string3);
      if ($1.isOk()) {
        let string$1 = $1[0][1];
        loop$string = string$1;
        loop$num_graphemes = num_graphemes - 1;
      } else {
        return string3;
      }
    }
  }
}
function inspect2(term) {
  let _pipe = inspect(term);
  return identity(_pipe);
}

// build/dev/javascript/gleam_stdlib/gleam/result.mjs
function is_ok(result) {
  if (!result.isOk()) {
    return false;
  } else {
    return true;
  }
}
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
function then$(result, fun) {
  return try$(result, fun);
}
function unwrap2(result, default$) {
  if (result.isOk()) {
    let v = result[0];
    return v;
  } else {
    return default$;
  }
}
function or(first5, second2) {
  if (first5.isOk()) {
    return first5;
  } else {
    return second2;
  }
}

// build/dev/javascript/gleam_stdlib/gleam/dynamic.mjs
var DecodeError = class extends CustomType {
  constructor(expected, found, path2) {
    super();
    this.expected = expected;
    this.found = found;
    this.path = path2;
  }
};
function dynamic(value4) {
  return new Ok(value4);
}
function int(data) {
  return decode_int(data);
}
function shallow_list(value4) {
  return decode_list(value4);
}
function any(decoders) {
  return (data) => {
    if (decoders.hasLength(0)) {
      return new Error(
        toList([new DecodeError("another type", classify_dynamic(data), toList([]))])
      );
    } else {
      let decoder2 = decoders.head;
      let decoders$1 = decoders.tail;
      let $ = decoder2(data);
      if ($.isOk()) {
        let decoded = $[0];
        return new Ok(decoded);
      } else {
        return any(decoders$1)(data);
      }
    }
  };
}
function all_errors(result) {
  if (result.isOk()) {
    return toList([]);
  } else {
    let errors = result[0];
    return errors;
  }
}
function push_path(error, name4) {
  let name$1 = identity(name4);
  let decoder2 = any(
    toList([string, (x) => {
      return map2(int(x), to_string);
    }])
  );
  let name$2 = (() => {
    let $ = decoder2(name$1);
    if ($.isOk()) {
      let name$22 = $[0];
      return name$22;
    } else {
      let _pipe = toList(["<", classify_dynamic(name$1), ">"]);
      let _pipe$1 = concat(_pipe);
      return identity(_pipe$1);
    }
  })();
  return error.withFields({ path: prepend(name$2, error.path) });
}
function list(decoder_type) {
  return (dynamic2) => {
    return try$(
      shallow_list(dynamic2),
      (list3) => {
        let _pipe = list3;
        let _pipe$1 = try_map(_pipe, decoder_type);
        return map_errors(
          _pipe$1,
          (_capture) => {
            return push_path(_capture, "*");
          }
        );
      }
    );
  };
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
function field(name4, inner_type) {
  return (value4) => {
    let missing_field_error = new DecodeError("field", "nothing", toList([]));
    return try$(
      decode_field(value4, name4),
      (maybe_inner) => {
        let _pipe = maybe_inner;
        let _pipe$1 = to_result(_pipe, toList([missing_field_error]));
        let _pipe$2 = try$(_pipe$1, inner_type);
        return map_errors(
          _pipe$2,
          (_capture) => {
            return push_path(_capture, name4);
          }
        );
      }
    );
  };
}
function decode3(constructor, t1, t2, t3) {
  return (value4) => {
    let $ = t1(value4);
    let $1 = t2(value4);
    let $2 = t3(value4);
    if ($.isOk() && $1.isOk() && $2.isOk()) {
      let a2 = $[0];
      let b = $1[0];
      let c = $2[0];
      return new Ok(constructor(a2, b, c));
    } else {
      let a2 = $;
      let b = $1;
      let c = $2;
      return new Error(
        flatten(toList([all_errors(a2), all_errors(b), all_errors(c)]))
      );
    }
  };
}

// build/dev/javascript/gleam_stdlib/gleam/io.mjs
function debug(term) {
  let _pipe = term;
  let _pipe$1 = inspect2(_pipe);
  print_debug(_pipe$1);
  return term;
}

// build/dev/javascript/gleam_stdlib/gleam/bool.mjs
function to_string2(bool3) {
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

// build/dev/javascript/gleam_json/gleam_json_ffi.mjs
function json_to_string(json) {
  return JSON.stringify(json);
}
function object(entries) {
  return Object.fromEntries(entries);
}
function identity2(x) {
  return x;
}
function do_null() {
  return null;
}
function decode(string3) {
  try {
    const result = JSON.parse(string3);
    return new Ok(result);
  } catch (err) {
    return new Error(getJsonDecodeError(err, string3));
  }
}
function getJsonDecodeError(stdErr, json) {
  if (isUnexpectedEndOfInput(stdErr))
    return new UnexpectedEndOfInput();
  return toUnexpectedByteError(stdErr, json);
}
function isUnexpectedEndOfInput(err) {
  const unexpectedEndOfInputRegex = /((unexpected (end|eof))|(end of data)|(unterminated string)|(json( parse error|\.parse)\: expected '(\:|\}|\])'))/i;
  return unexpectedEndOfInputRegex.test(err.message);
}
function toUnexpectedByteError(err, json) {
  let converters = [
    v8UnexpectedByteError,
    oldV8UnexpectedByteError,
    jsCoreUnexpectedByteError,
    spidermonkeyUnexpectedByteError
  ];
  for (let converter of converters) {
    let result = converter(err, json);
    if (result)
      return result;
  }
  return new UnexpectedByte("", 0);
}
function v8UnexpectedByteError(err) {
  const regex = /unexpected token '(.)', ".+" is not valid JSON/i;
  const match = regex.exec(err.message);
  if (!match)
    return null;
  const byte = toHex(match[1]);
  return new UnexpectedByte(byte, -1);
}
function oldV8UnexpectedByteError(err) {
  const regex = /unexpected token (.) in JSON at position (\d+)/i;
  const match = regex.exec(err.message);
  if (!match)
    return null;
  const byte = toHex(match[1]);
  const position = Number(match[2]);
  return new UnexpectedByte(byte, position);
}
function spidermonkeyUnexpectedByteError(err, json) {
  const regex = /(unexpected character|expected .*) at line (\d+) column (\d+)/i;
  const match = regex.exec(err.message);
  if (!match)
    return null;
  const line = Number(match[2]);
  const column = Number(match[3]);
  const position = getPositionFromMultiline(line, column, json);
  const byte = toHex(json[position]);
  return new UnexpectedByte(byte, position);
}
function jsCoreUnexpectedByteError(err) {
  const regex = /unexpected (identifier|token) "(.)"/i;
  const match = regex.exec(err.message);
  if (!match)
    return null;
  const byte = toHex(match[2]);
  return new UnexpectedByte(byte, 0);
}
function toHex(char) {
  return "0x" + char.charCodeAt(0).toString(16).toUpperCase();
}
function getPositionFromMultiline(line, column, string3) {
  if (line === 1)
    return column - 1;
  let currentLn = 1;
  let position = 0;
  string3.split("").find((char, idx) => {
    if (char === "\n")
      currentLn += 1;
    if (currentLn === line) {
      position = idx + column;
      return true;
    }
    return false;
  });
  return position;
}

// build/dev/javascript/gleam_json/gleam/json.mjs
var UnexpectedEndOfInput = class extends CustomType {
};
var UnexpectedByte = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var UnexpectedFormat = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
function do_decode(json, decoder2) {
  return then$(
    decode(json),
    (dynamic_value) => {
      let _pipe = decoder2(dynamic_value);
      return map_error(
        _pipe,
        (var0) => {
          return new UnexpectedFormat(var0);
        }
      );
    }
  );
}
function decode2(json, decoder2) {
  return do_decode(json, decoder2);
}
function to_string3(json) {
  return json_to_string(json);
}
function string2(input3) {
  return identity2(input3);
}
function bool(input3) {
  return identity2(input3);
}
function null$() {
  return do_null();
}
function object2(entries) {
  return object(entries);
}

// build/dev/javascript/lustre/lustre/effect.mjs
var Effect = class extends CustomType {
  constructor(all2) {
    super();
    this.all = all2;
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
function event(name4, data) {
  return custom((_, emit3, _1, _2) => {
    return emit3(name4, data);
  });
}
function none() {
  return new Effect(toList([]));
}
function batch(effects) {
  return new Effect(
    fold(
      effects,
      toList([]),
      (b, _use1) => {
        let a2 = _use1.all;
        return append(b, a2);
      }
    )
  );
}

// build/dev/javascript/lustre/lustre/internals/vdom.mjs
var Text = class extends CustomType {
  constructor(content2) {
    super();
    this.content = content2;
  }
};
var Element = class extends CustomType {
  constructor(key, namespace2, tag, attrs, children2, self_closing, void$) {
    super();
    this.key = key;
    this.namespace = namespace2;
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
    let name4 = attribute2[0];
    let handler = attribute2[1];
    let name$1 = drop_start(name4, 2);
    return new Ok([name$1, handler]);
  }
}
function do_element_list_handlers(elements2, handlers2, key) {
  return index_fold(
    elements2,
    handlers2,
    (handlers3, element3, index3) => {
      let key$1 = key + "-" + to_string(index3);
      return do_handlers(element3, handlers3, key$1);
    }
  );
}
function do_handlers(loop$element, loop$handlers, loop$key) {
  while (true) {
    let element3 = loop$element;
    let handlers2 = loop$handlers;
    let key = loop$key;
    if (element3 instanceof Text) {
      return handlers2;
    } else if (element3 instanceof Map2) {
      let subtree = element3.subtree;
      loop$element = subtree();
      loop$handlers = handlers2;
      loop$key = key;
    } else {
      let attrs = element3.attrs;
      let children2 = element3.children;
      let handlers$1 = fold(
        attrs,
        handlers2,
        (handlers3, attr) => {
          let $ = attribute_to_event_handler(attr);
          if ($.isOk()) {
            let name4 = $[0][0];
            let handler = $[0][1];
            return insert(handlers3, key + "-" + name4, handler);
          } else {
            return handlers3;
          }
        }
      );
      return do_element_list_handlers(children2, handlers$1, key);
    }
  }
}
function handlers(element3) {
  return do_handlers(element3, new_map(), "0");
}

// build/dev/javascript/lustre/lustre/attribute.mjs
function attribute(name4, value4) {
  return new Attribute(name4, identity(value4), false);
}
function property(name4, value4) {
  return new Attribute(name4, identity(value4), true);
}
function on(name4, handler) {
  return new Event("on" + name4, handler);
}
function style(properties) {
  return attribute(
    "style",
    fold(
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
function class$(name4) {
  return attribute("class", name4);
}
function role(name4) {
  return attribute("role", name4);
}
function value(val) {
  return attribute("value", val);
}
function autocomplete(name4) {
  return attribute("autocomplete", name4);
}
function disabled(is_disabled) {
  return property("disabled", is_disabled);
}
function name(name4) {
  return attribute("name", name4);
}
function href(uri) {
  return attribute("href", uri);
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
function do_keyed(el, key) {
  if (el instanceof Element) {
    let namespace2 = el.namespace;
    let tag = el.tag;
    let attrs = el.attrs;
    let children2 = el.children;
    let self_closing = el.self_closing;
    let void$ = el.void;
    return new Element(
      key,
      namespace2,
      tag,
      attrs,
      children2,
      self_closing,
      void$
    );
  } else if (el instanceof Map2) {
    let subtree = el.subtree;
    return new Map2(() => {
      return do_keyed(subtree(), key);
    });
  } else {
    return el;
  }
}
function keyed(el, children2) {
  return el(
    map(
      children2,
      (_use0) => {
        let key = _use0[0];
        let child = _use0[1];
        return do_keyed(child, key);
      }
    )
  );
}
function namespaced(namespace2, tag, attrs, children2) {
  return new Element("", namespace2, tag, attrs, children2, false, false);
}
function text(content2) {
  return new Text(content2);
}
function fragment(elements2) {
  return element(
    "lustre-fragment",
    toList([style(toList([["display", "contents"]]))]),
    elements2
  );
}
function get_root(effect) {
  return custom(
    (dispatch, _, _1, root) => {
      return effect(dispatch, root);
    }
  );
}

// build/dev/javascript/gleam_stdlib/gleam/set.mjs
var Set2 = class extends CustomType {
  constructor(dict2) {
    super();
    this.dict = dict2;
  }
};
function new$2() {
  return new Set2(new_map());
}
function contains(set2, member) {
  let _pipe = set2.dict;
  let _pipe$1 = map_get(_pipe, member);
  return is_ok(_pipe$1);
}
var token = void 0;
function insert2(set2, member) {
  return new Set2(insert(set2.dict, member, token));
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
  return isEqual(diff2.created, new_map()) && isEqual(
    diff2.removed,
    new$2()
  ) && isEqual(diff2.updated, new_map());
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
if (globalThis.customElements && !globalThis.customElements.get("lustre-fragment")) {
  globalThis.customElements.define(
    "lustre-fragment",
    class LustreFragment extends HTMLElement {
      constructor() {
        super();
      }
    }
  );
}
function morph(prev, next2, dispatch) {
  let out;
  let stack = [{ prev, next: next2, parent: prev.parentNode }];
  while (stack.length) {
    let { prev: prev2, next: next3, parent } = stack.pop();
    while (next3.subtree !== void 0)
      next3 = next3.subtree();
    if (next3.content !== void 0) {
      if (!prev2) {
        const created = document.createTextNode(next3.content);
        parent.appendChild(created);
        out ??= created;
      } else if (prev2.nodeType === Node.TEXT_NODE) {
        if (prev2.textContent !== next3.content)
          prev2.textContent = next3.content;
        out ??= prev2;
      } else {
        const created = document.createTextNode(next3.content);
        parent.replaceChild(created, prev2);
        out ??= created;
      }
    } else if (next3.tag !== void 0) {
      const created = createElementNode({
        prev: prev2,
        next: next3,
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
function createElementNode({ prev, next: next2, dispatch, stack }) {
  const namespace2 = next2.namespace || "http://www.w3.org/1999/xhtml";
  const canMorph = prev && prev.nodeType === Node.ELEMENT_NODE && prev.localName === next2.tag && prev.namespaceURI === (next2.namespace || "http://www.w3.org/1999/xhtml");
  const el = canMorph ? prev : namespace2 ? document.createElementNS(namespace2, next2.tag) : document.createElement(next2.tag);
  let handlersForEl;
  if (!registeredHandlers.has(el)) {
    const emptyHandlers = /* @__PURE__ */ new Map();
    registeredHandlers.set(el, emptyHandlers);
    handlersForEl = emptyHandlers;
  } else {
    handlersForEl = registeredHandlers.get(el);
  }
  const prevHandlers = canMorph ? new Set(handlersForEl.keys()) : null;
  const prevAttributes = canMorph ? new Set(Array.from(prev.attributes, (a2) => a2.name)) : null;
  let className = null;
  let style3 = null;
  let innerHTML = null;
  if (canMorph && next2.tag === "textarea") {
    const innertText = next2.children[Symbol.iterator]().next().value?.content;
    if (innertText !== void 0)
      el.value = innertText;
  }
  const delegated = [];
  for (const attr of next2.attrs) {
    const name4 = attr[0];
    const value4 = attr[1];
    if (attr.as_property) {
      if (el[name4] !== value4)
        el[name4] = value4;
      if (canMorph)
        prevAttributes.delete(name4);
    } else if (name4.startsWith("on")) {
      const eventName = name4.slice(2);
      const callback = dispatch(value4, eventName === "input");
      if (!handlersForEl.has(eventName)) {
        el.addEventListener(eventName, lustreGenericEventHandler);
      }
      handlersForEl.set(eventName, callback);
      if (canMorph)
        prevHandlers.delete(eventName);
    } else if (name4.startsWith("data-lustre-on-")) {
      const eventName = name4.slice(15);
      const callback = dispatch(lustreServerEventHandler);
      if (!handlersForEl.has(eventName)) {
        el.addEventListener(eventName, lustreGenericEventHandler);
      }
      handlersForEl.set(eventName, callback);
      el.setAttribute(name4, value4);
      if (canMorph) {
        prevHandlers.delete(eventName);
        prevAttributes.delete(name4);
      }
    } else if (name4.startsWith("delegate:data-") || name4.startsWith("delegate:aria-")) {
      el.setAttribute(name4, value4);
      delegated.push([name4.slice(10), value4]);
    } else if (name4 === "class") {
      className = className === null ? value4 : className + " " + value4;
    } else if (name4 === "style") {
      style3 = style3 === null ? value4 : style3 + value4;
    } else if (name4 === "dangerous-unescaped-html") {
      innerHTML = value4;
    } else {
      if (el.getAttribute(name4) !== value4)
        el.setAttribute(name4, value4);
      if (name4 === "value" || name4 === "selected")
        el[name4] = value4;
      if (canMorph)
        prevAttributes.delete(name4);
    }
  }
  if (className !== null) {
    el.setAttribute("class", className);
    if (canMorph)
      prevAttributes.delete("class");
  }
  if (style3 !== null) {
    el.setAttribute("style", style3);
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
  if (next2.tag === "slot") {
    window.queueMicrotask(() => {
      for (const child of el.assignedElements()) {
        for (const [name4, value4] of delegated) {
          if (!child.hasAttribute(name4)) {
            child.setAttribute(name4, value4);
          }
        }
      }
    });
  }
  if (next2.key !== void 0 && next2.key !== "") {
    el.setAttribute("data-lustre-key", next2.key);
  } else if (innerHTML !== null) {
    el.innerHTML = innerHTML;
    return el;
  }
  let prevChild = el.firstChild;
  let seenKeys = null;
  let keyedChildren = null;
  let incomingKeyedChildren = null;
  let firstChild = children(next2).next().value;
  if (canMorph && firstChild !== void 0 && // Explicit checks are more verbose but truthy checks force a bunch of comparisons
  // we don't care about: it's never gonna be a number etc.
  firstChild.key !== void 0 && firstChild.key !== "") {
    seenKeys = /* @__PURE__ */ new Set();
    keyedChildren = getKeyedChildren(prev);
    incomingKeyedChildren = getKeyedChildren(next2);
    for (const child of children(next2)) {
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
    for (const child of children(next2)) {
      stack.unshift({ prev: prevChild, next: child, parent: el });
      prevChild = prevChild?.nextSibling;
    }
  }
  while (prevChild) {
    const next3 = prevChild.nextSibling;
    el.removeChild(prevChild);
    prevChild = next3;
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
      (data2, property2) => {
        const path2 = property2.split(".");
        for (let i = 0, o = data2, e = event2; i < path2.length; i++) {
          if (i === path2.length - 1) {
            o[path2[i]] = e[path2[i]];
          } else {
            o[path2[i]] ??= {};
            e = e[path2[i]];
            o = o[path2[i]];
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
function* children(element3) {
  for (const child of element3.children) {
    yield* forceChild(child);
  }
}
function* forceChild(element3) {
  if (element3.subtree !== void 0) {
    yield* forceChild(element3.subtree());
  } else {
    yield element3;
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
  static start({ init: init5, update: update4, view: view3 }, selector, flags) {
    if (!is_browser())
      return new Error(new NotABrowser());
    const root = selector instanceof HTMLElement ? selector : document.querySelector(selector);
    if (!root)
      return new Error(new ElementNotFound(selector));
    const app = new _LustreClientApplication(root, init5(flags), update4, view3);
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
  constructor(root, [init5, effects], update4, view3) {
    this.root = root;
    this.#model = init5;
    this.#update = update4;
    this.#view = view3;
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
   */
  #tick(effects = []) {
    this.#tickScheduled = void 0;
    this.#flush(effects);
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
  #flush(effects = []) {
    while (this.#queue.length > 0) {
      const msg = this.#queue.shift();
      const [next2, effect] = this.#update(this.#model, msg);
      effects = effects.concat(effect.all.toArray());
      this.#model = next2;
    }
    while (effects.length > 0) {
      const effect = effects.shift();
      const dispatch = (msg) => this.send(new Dispatch(msg));
      const emit3 = (event2, data) => this.root.dispatchEvent(
        new CustomEvent(event2, {
          detail: data,
          bubbles: true,
          composed: true
        })
      );
      const select = () => {
      };
      const root = this.root;
      effect({ dispatch, emit: emit3, select, root });
    }
    if (this.#queue.length > 0) {
      this.#flush(effects);
    }
  }
};
var start = LustreClientApplication.start;
var make_lustre_client_component = ({ init: init5, update: update4, view: view3, on_attribute_change: on_attribute_change3 }, name4) => {
  if (!is_browser())
    return new Error(new NotABrowser());
  if (!name4.includes("-"))
    return new Error(new BadComponentName(name4));
  if (window.customElements.get(name4)) {
    return new Error(new ComponentAlreadyRegistered(name4));
  }
  const [model, effects] = init5(void 0);
  const hasAttributes = on_attribute_change3 instanceof Some;
  const component2 = class LustreClientComponent extends HTMLElement {
    /**
     * @returns {string[]}
     */
    static get observedAttributes() {
      if (hasAttributes) {
        return on_attribute_change3[0].entries().map(([name5]) => name5);
      } else {
        return [];
      }
    }
    /**
     * @returns {LustreClientComponent}
     */
    constructor() {
      super();
      this.attachShadow({ mode: "open" });
      this.internals = this.attachInternals();
      if (hasAttributes) {
        on_attribute_change3[0].forEach((decoder2, name5) => {
          Object.defineProperty(this, name5, {
            get() {
              return this[`__mirrored__${name5}`];
            },
            set(value4) {
              const prev = this[`__mirrored__${name5}`];
              if (this.#connected && isEqual(prev, value4))
                return;
              this[`__mirrorred__${name5}`] = value4;
              const decoded = decoder2(value4);
              if (decoded instanceof Error)
                return;
              this.#queue.push(decoded[0]);
              if (this.#connected && !this.#tickScheduled) {
                this.#tickScheduled = window.requestAnimationFrame(
                  () => this.#tick()
                );
              }
            }
          });
        });
      }
    }
    /**
     *
     */
    connectedCallback() {
      this.#adoptStyleSheets().finally(() => {
        this.#tick(effects.all.toArray(), true);
        this.#connected = true;
      });
    }
    /**
     * @param {string} key
     * @param {string} prev
     * @param {string} next
     */
    attributeChangedCallback(key, prev, next2) {
      if (prev !== next2)
        this[key] = next2;
    }
    /**
     *
     */
    disconnectedCallback() {
      this.#model = null;
      this.#queue = [];
      this.#tickScheduled = window.cancelAnimationFrame(this.#tickScheduled);
      this.#connected = false;
    }
    /**
     * @param {Lustre.Action<Msg, Lustre.ClientSpa>} action
     */
    send(action) {
      if (action instanceof Debug) {
        if (action[0] instanceof ForceModel) {
          this.#tickScheduled = window.cancelAnimationFrame(
            this.#tickScheduled
          );
          this.#queue = [];
          this.#model = action[0][0];
          const vdom = view3(this.#model);
          const dispatch = (handler, immediate = false) => (event2) => {
            const result = handler(event2);
            if (result instanceof Ok) {
              this.send(new Dispatch(result[0], immediate));
            }
          };
          const prev = this.shadowRoot.childNodes[this.#adoptedStyleElements.length] ?? this.shadowRoot.appendChild(document.createTextNode(""));
          morph(prev, vdom, dispatch);
        }
      } else if (action instanceof Dispatch) {
        const msg = action[0];
        const immediate = action[1] ?? false;
        this.#queue.push(msg);
        if (immediate) {
          this.#tickScheduled = window.cancelAnimationFrame(
            this.#tickScheduled
          );
          this.#tick();
        } else if (!this.#tickScheduled) {
          this.#tickScheduled = window.requestAnimationFrame(
            () => this.#tick()
          );
        }
      } else if (action instanceof Emit2) {
        const event2 = action[0];
        const data = action[1];
        this.dispatchEvent(
          new CustomEvent(event2, {
            detail: data,
            bubbles: true,
            composed: true
          })
        );
      }
    }
    /** @type {Element[]} */
    #adoptedStyleElements = [];
    /** @type {Model} */
    #model = model;
    /** @type {Array<Msg>} */
    #queue = [];
    /** @type {number | undefined} */
    #tickScheduled;
    /** @type {boolean} */
    #connected = true;
    #tick(effects2 = []) {
      if (!this.#connected)
        return;
      this.#tickScheduled = void 0;
      this.#flush(effects2);
      const vdom = view3(this.#model);
      const dispatch = (handler, immediate = false) => (event2) => {
        const result = handler(event2);
        if (result instanceof Ok) {
          this.send(new Dispatch(result[0], immediate));
        }
      };
      const prev = this.shadowRoot.childNodes[this.#adoptedStyleElements.length] ?? this.shadowRoot.appendChild(document.createTextNode(""));
      morph(prev, vdom, dispatch);
    }
    #flush(effects2 = []) {
      while (this.#queue.length > 0) {
        const msg = this.#queue.shift();
        const [next2, effect] = update4(this.#model, msg);
        effects2 = effects2.concat(effect.all.toArray());
        this.#model = next2;
      }
      while (effects2.length > 0) {
        const effect = effects2.shift();
        const dispatch = (msg) => this.send(new Dispatch(msg));
        const emit3 = (event2, data) => this.dispatchEvent(
          new CustomEvent(event2, {
            detail: data,
            bubbles: true,
            composed: true
          })
        );
        const select = () => {
        };
        const root = this.shadowRoot;
        effect({ dispatch, emit: emit3, select, root });
      }
      if (this.#queue.length > 0) {
        this.#flush(effects2);
      }
    }
    async #adoptStyleSheets() {
      const pendingParentStylesheets = [];
      for (const link of document.querySelectorAll("link[rel=stylesheet]")) {
        if (link.sheet)
          continue;
        pendingParentStylesheets.push(
          new Promise((resolve2, reject) => {
            link.addEventListener("load", resolve2);
            link.addEventListener("error", reject);
          })
        );
      }
      await Promise.allSettled(pendingParentStylesheets);
      while (this.#adoptedStyleElements.length) {
        this.#adoptedStyleElements.shift().remove();
        this.shadowRoot.firstChild.remove();
      }
      this.shadowRoot.adoptedStyleSheets = this.getRootNode().adoptedStyleSheets;
      const pending = [];
      for (const sheet of document.styleSheets) {
        try {
          this.shadowRoot.adoptedStyleSheets.push(sheet);
        } catch {
          try {
            const adoptedSheet = new CSSStyleSheet();
            for (const rule of sheet.cssRules) {
              adoptedSheet.insertRule(
                rule.cssText,
                adoptedSheet.cssRules.length
              );
            }
            this.shadowRoot.adoptedStyleSheets.push(adoptedSheet);
          } catch {
            const node = sheet.ownerNode.cloneNode();
            this.shadowRoot.prepend(node);
            this.#adoptedStyleElements.push(node);
            pending.push(
              new Promise((resolve2, reject) => {
                node.onload = resolve2;
                node.onerror = reject;
              })
            );
          }
        }
      }
      return Promise.allSettled(pending);
    }
  };
  window.customElements.define(name4, component2);
  return new Ok(void 0);
};
var LustreServerApplication = class _LustreServerApplication {
  static start({ init: init5, update: update4, view: view3, on_attribute_change: on_attribute_change3 }, flags) {
    const app = new _LustreServerApplication(
      init5(flags),
      update4,
      view3,
      on_attribute_change3
    );
    return new Ok((action) => app.send(action));
  }
  constructor([model, effects], update4, view3, on_attribute_change3) {
    this.#model = model;
    this.#update = update4;
    this.#view = view3;
    this.#html = view3(model);
    this.#onAttributeChange = on_attribute_change3;
    this.#renderers = /* @__PURE__ */ new Map();
    this.#handlers = handlers(this.#html);
    this.#tick(effects.all.toArray());
  }
  send(action) {
    if (action instanceof Attrs) {
      for (const attr of action[0]) {
        const decoder2 = this.#onAttributeChange.get(attr[0]);
        if (!decoder2)
          continue;
        const msg = decoder2(attr[1]);
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
    this.#flush(effects);
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
  #flush(effects = []) {
    while (this.#queue.length > 0) {
      const msg = this.#queue.shift();
      const [next2, effect] = this.#update(this.#model, msg);
      effects = effects.concat(effect.all.toArray());
      this.#model = next2;
    }
    while (effects.length > 0) {
      const effect = effects.shift();
      const dispatch = (msg) => this.send(new Dispatch(msg));
      const emit3 = (event2, data) => this.root.dispatchEvent(
        new CustomEvent(event2, {
          detail: data,
          bubbles: true,
          composed: true
        })
      );
      const select = () => {
      };
      const root = null;
      effect({ dispatch, emit: emit3, select, root });
    }
    if (this.#queue.length > 0) {
      this.#flush(effects);
    }
  }
};
var start_server_application = LustreServerApplication.start;
var is_browser = () => globalThis.window && window.document;
var prevent_default = (event2) => event2.preventDefault();

// build/dev/javascript/lustre/lustre.mjs
var App = class extends CustomType {
  constructor(init5, update4, view3, on_attribute_change3) {
    super();
    this.init = init5;
    this.update = update4;
    this.view = view3;
    this.on_attribute_change = on_attribute_change3;
  }
};
var BadComponentName = class extends CustomType {
  constructor(name4) {
    super();
    this.name = name4;
  }
};
var ComponentAlreadyRegistered = class extends CustomType {
  constructor(name4) {
    super();
    this.name = name4;
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
function application(init5, update4, view3) {
  return new App(init5, update4, view3, new None());
}
function component(init5, update4, view3, on_attribute_change3) {
  return new App(init5, update4, view3, new Some(on_attribute_change3));
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
function text2(content2) {
  return text(content2);
}
function article(attrs, children2) {
  return element("article", attrs, children2);
}
function header(attrs, children2) {
  return element("header", attrs, children2);
}
function h4(attrs, children2) {
  return element("h4", attrs, children2);
}
function main(attrs, children2) {
  return element("main", attrs, children2);
}
function div(attrs, children2) {
  return element("div", attrs, children2);
}
function li(attrs, children2) {
  return element("li", attrs, children2);
}
function ul(attrs, children2) {
  return element("ul", attrs, children2);
}
function a(attrs, children2) {
  return element("a", attrs, children2);
}
function span(attrs, children2) {
  return element("span", attrs, children2);
}
function svg(attrs, children2) {
  return namespaced("http://www.w3.org/2000/svg", "svg", attrs, children2);
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
function slot(attrs) {
  return element("slot", attrs, toList([]));
}

// build/dev/javascript/lustre/lustre/event.mjs
function emit2(event2, data) {
  return event(event2, data);
}
function on2(name4, handler) {
  return on(name4, handler);
}
function on_click(msg) {
  return on2("click", (_) => {
    return new Ok(msg);
  });
}
function on_mouse_down(msg) {
  return on2("mousedown", (_) => {
    return new Ok(msg);
  });
}
function on_mouse_over(msg) {
  return on2("mouseover", (_) => {
    return new Ok(msg);
  });
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
      return map2(_pipe, msg);
    }
  );
}

// build/dev/javascript/gleam_stdlib/gleam/uri.mjs
var Uri = class extends CustomType {
  constructor(scheme, userinfo, host, port, path2, query, fragment2) {
    super();
    this.scheme = scheme;
    this.userinfo = userinfo;
    this.host = host;
    this.port = port;
    this.path = path2;
    this.query = query;
    this.fragment = fragment2;
  }
};
function is_valid_host_within_brackets_char(char) {
  return 48 >= char && char <= 57 || 65 >= char && char <= 90 || 97 >= char && char <= 122 || char === 58 || char === 46;
}
function parse_fragment(rest2, pieces) {
  return new Ok(pieces.withFields({ fragment: new Some(rest2) }));
}
function parse_query_with_question_mark_loop(loop$original, loop$uri_string, loop$pieces, loop$size) {
  while (true) {
    let original = loop$original;
    let uri_string = loop$uri_string;
    let pieces = loop$pieces;
    let size2 = loop$size;
    if (uri_string.startsWith("#") && size2 === 0) {
      let rest2 = uri_string.slice(1);
      return parse_fragment(rest2, pieces);
    } else if (uri_string.startsWith("#")) {
      let rest2 = uri_string.slice(1);
      let query = string_codeunit_slice(original, 0, size2);
      let pieces$1 = pieces.withFields({ query: new Some(query) });
      return parse_fragment(rest2, pieces$1);
    } else if (uri_string === "") {
      return new Ok(pieces.withFields({ query: new Some(original) }));
    } else {
      let $ = pop_codeunit(uri_string);
      let rest2 = $[1];
      loop$original = original;
      loop$uri_string = rest2;
      loop$pieces = pieces;
      loop$size = size2 + 1;
    }
  }
}
function parse_query_with_question_mark(uri_string, pieces) {
  return parse_query_with_question_mark_loop(uri_string, uri_string, pieces, 0);
}
function parse_path_loop(loop$original, loop$uri_string, loop$pieces, loop$size) {
  while (true) {
    let original = loop$original;
    let uri_string = loop$uri_string;
    let pieces = loop$pieces;
    let size2 = loop$size;
    if (uri_string.startsWith("?")) {
      let rest2 = uri_string.slice(1);
      let path2 = string_codeunit_slice(original, 0, size2);
      let pieces$1 = pieces.withFields({ path: path2 });
      return parse_query_with_question_mark(rest2, pieces$1);
    } else if (uri_string.startsWith("#")) {
      let rest2 = uri_string.slice(1);
      let path2 = string_codeunit_slice(original, 0, size2);
      let pieces$1 = pieces.withFields({ path: path2 });
      return parse_fragment(rest2, pieces$1);
    } else if (uri_string === "") {
      return new Ok(pieces.withFields({ path: original }));
    } else {
      let $ = pop_codeunit(uri_string);
      let rest2 = $[1];
      loop$original = original;
      loop$uri_string = rest2;
      loop$pieces = pieces;
      loop$size = size2 + 1;
    }
  }
}
function parse_path(uri_string, pieces) {
  return parse_path_loop(uri_string, uri_string, pieces, 0);
}
function parse_port_loop(loop$uri_string, loop$pieces, loop$port) {
  while (true) {
    let uri_string = loop$uri_string;
    let pieces = loop$pieces;
    let port = loop$port;
    if (uri_string.startsWith("0")) {
      let rest2 = uri_string.slice(1);
      loop$uri_string = rest2;
      loop$pieces = pieces;
      loop$port = port * 10;
    } else if (uri_string.startsWith("1")) {
      let rest2 = uri_string.slice(1);
      loop$uri_string = rest2;
      loop$pieces = pieces;
      loop$port = port * 10 + 1;
    } else if (uri_string.startsWith("2")) {
      let rest2 = uri_string.slice(1);
      loop$uri_string = rest2;
      loop$pieces = pieces;
      loop$port = port * 10 + 2;
    } else if (uri_string.startsWith("3")) {
      let rest2 = uri_string.slice(1);
      loop$uri_string = rest2;
      loop$pieces = pieces;
      loop$port = port * 10 + 3;
    } else if (uri_string.startsWith("4")) {
      let rest2 = uri_string.slice(1);
      loop$uri_string = rest2;
      loop$pieces = pieces;
      loop$port = port * 10 + 4;
    } else if (uri_string.startsWith("5")) {
      let rest2 = uri_string.slice(1);
      loop$uri_string = rest2;
      loop$pieces = pieces;
      loop$port = port * 10 + 5;
    } else if (uri_string.startsWith("6")) {
      let rest2 = uri_string.slice(1);
      loop$uri_string = rest2;
      loop$pieces = pieces;
      loop$port = port * 10 + 6;
    } else if (uri_string.startsWith("7")) {
      let rest2 = uri_string.slice(1);
      loop$uri_string = rest2;
      loop$pieces = pieces;
      loop$port = port * 10 + 7;
    } else if (uri_string.startsWith("8")) {
      let rest2 = uri_string.slice(1);
      loop$uri_string = rest2;
      loop$pieces = pieces;
      loop$port = port * 10 + 8;
    } else if (uri_string.startsWith("9")) {
      let rest2 = uri_string.slice(1);
      loop$uri_string = rest2;
      loop$pieces = pieces;
      loop$port = port * 10 + 9;
    } else if (uri_string.startsWith("?")) {
      let rest2 = uri_string.slice(1);
      let pieces$1 = pieces.withFields({ port: new Some(port) });
      return parse_query_with_question_mark(rest2, pieces$1);
    } else if (uri_string.startsWith("#")) {
      let rest2 = uri_string.slice(1);
      let pieces$1 = pieces.withFields({ port: new Some(port) });
      return parse_fragment(rest2, pieces$1);
    } else if (uri_string.startsWith("/")) {
      let pieces$1 = pieces.withFields({ port: new Some(port) });
      return parse_path(uri_string, pieces$1);
    } else if (uri_string === "") {
      return new Ok(pieces.withFields({ port: new Some(port) }));
    } else {
      return new Error(void 0);
    }
  }
}
function parse_port(uri_string, pieces) {
  if (uri_string.startsWith(":0")) {
    let rest2 = uri_string.slice(2);
    return parse_port_loop(rest2, pieces, 0);
  } else if (uri_string.startsWith(":1")) {
    let rest2 = uri_string.slice(2);
    return parse_port_loop(rest2, pieces, 1);
  } else if (uri_string.startsWith(":2")) {
    let rest2 = uri_string.slice(2);
    return parse_port_loop(rest2, pieces, 2);
  } else if (uri_string.startsWith(":3")) {
    let rest2 = uri_string.slice(2);
    return parse_port_loop(rest2, pieces, 3);
  } else if (uri_string.startsWith(":4")) {
    let rest2 = uri_string.slice(2);
    return parse_port_loop(rest2, pieces, 4);
  } else if (uri_string.startsWith(":5")) {
    let rest2 = uri_string.slice(2);
    return parse_port_loop(rest2, pieces, 5);
  } else if (uri_string.startsWith(":6")) {
    let rest2 = uri_string.slice(2);
    return parse_port_loop(rest2, pieces, 6);
  } else if (uri_string.startsWith(":7")) {
    let rest2 = uri_string.slice(2);
    return parse_port_loop(rest2, pieces, 7);
  } else if (uri_string.startsWith(":8")) {
    let rest2 = uri_string.slice(2);
    return parse_port_loop(rest2, pieces, 8);
  } else if (uri_string.startsWith(":9")) {
    let rest2 = uri_string.slice(2);
    return parse_port_loop(rest2, pieces, 9);
  } else if (uri_string.startsWith(":")) {
    return new Error(void 0);
  } else if (uri_string.startsWith("?")) {
    let rest2 = uri_string.slice(1);
    return parse_query_with_question_mark(rest2, pieces);
  } else if (uri_string.startsWith("#")) {
    let rest2 = uri_string.slice(1);
    return parse_fragment(rest2, pieces);
  } else if (uri_string.startsWith("/")) {
    return parse_path(uri_string, pieces);
  } else if (uri_string === "") {
    return new Ok(pieces);
  } else {
    return new Error(void 0);
  }
}
function parse_host_outside_of_brackets_loop(loop$original, loop$uri_string, loop$pieces, loop$size) {
  while (true) {
    let original = loop$original;
    let uri_string = loop$uri_string;
    let pieces = loop$pieces;
    let size2 = loop$size;
    if (uri_string === "") {
      return new Ok(pieces.withFields({ host: new Some(original) }));
    } else if (uri_string.startsWith(":")) {
      let host = string_codeunit_slice(original, 0, size2);
      let pieces$1 = pieces.withFields({ host: new Some(host) });
      return parse_port(uri_string, pieces$1);
    } else if (uri_string.startsWith("/")) {
      let host = string_codeunit_slice(original, 0, size2);
      let pieces$1 = pieces.withFields({ host: new Some(host) });
      return parse_path(uri_string, pieces$1);
    } else if (uri_string.startsWith("?")) {
      let rest2 = uri_string.slice(1);
      let host = string_codeunit_slice(original, 0, size2);
      let pieces$1 = pieces.withFields({ host: new Some(host) });
      return parse_query_with_question_mark(rest2, pieces$1);
    } else if (uri_string.startsWith("#")) {
      let rest2 = uri_string.slice(1);
      let host = string_codeunit_slice(original, 0, size2);
      let pieces$1 = pieces.withFields({ host: new Some(host) });
      return parse_fragment(rest2, pieces$1);
    } else {
      let $ = pop_codeunit(uri_string);
      let rest2 = $[1];
      loop$original = original;
      loop$uri_string = rest2;
      loop$pieces = pieces;
      loop$size = size2 + 1;
    }
  }
}
function parse_host_within_brackets_loop(loop$original, loop$uri_string, loop$pieces, loop$size) {
  while (true) {
    let original = loop$original;
    let uri_string = loop$uri_string;
    let pieces = loop$pieces;
    let size2 = loop$size;
    if (uri_string === "") {
      return new Ok(pieces.withFields({ host: new Some(uri_string) }));
    } else if (uri_string.startsWith("]") && size2 === 0) {
      let rest2 = uri_string.slice(1);
      return parse_port(rest2, pieces);
    } else if (uri_string.startsWith("]")) {
      let rest2 = uri_string.slice(1);
      let host = string_codeunit_slice(original, 0, size2 + 1);
      let pieces$1 = pieces.withFields({ host: new Some(host) });
      return parse_port(rest2, pieces$1);
    } else if (uri_string.startsWith("/") && size2 === 0) {
      return parse_path(uri_string, pieces);
    } else if (uri_string.startsWith("/")) {
      let host = string_codeunit_slice(original, 0, size2);
      let pieces$1 = pieces.withFields({ host: new Some(host) });
      return parse_path(uri_string, pieces$1);
    } else if (uri_string.startsWith("?") && size2 === 0) {
      let rest2 = uri_string.slice(1);
      return parse_query_with_question_mark(rest2, pieces);
    } else if (uri_string.startsWith("?")) {
      let rest2 = uri_string.slice(1);
      let host = string_codeunit_slice(original, 0, size2);
      let pieces$1 = pieces.withFields({ host: new Some(host) });
      return parse_query_with_question_mark(rest2, pieces$1);
    } else if (uri_string.startsWith("#") && size2 === 0) {
      let rest2 = uri_string.slice(1);
      return parse_fragment(rest2, pieces);
    } else if (uri_string.startsWith("#")) {
      let rest2 = uri_string.slice(1);
      let host = string_codeunit_slice(original, 0, size2);
      let pieces$1 = pieces.withFields({ host: new Some(host) });
      return parse_fragment(rest2, pieces$1);
    } else {
      let $ = pop_codeunit(uri_string);
      let char = $[0];
      let rest2 = $[1];
      let $1 = is_valid_host_within_brackets_char(char);
      if ($1) {
        loop$original = original;
        loop$uri_string = rest2;
        loop$pieces = pieces;
        loop$size = size2 + 1;
      } else {
        return parse_host_outside_of_brackets_loop(
          original,
          original,
          pieces,
          0
        );
      }
    }
  }
}
function parse_host_within_brackets(uri_string, pieces) {
  return parse_host_within_brackets_loop(uri_string, uri_string, pieces, 0);
}
function parse_host_outside_of_brackets(uri_string, pieces) {
  return parse_host_outside_of_brackets_loop(uri_string, uri_string, pieces, 0);
}
function parse_host(uri_string, pieces) {
  if (uri_string.startsWith("[")) {
    return parse_host_within_brackets(uri_string, pieces);
  } else if (uri_string.startsWith(":")) {
    let pieces$1 = pieces.withFields({ host: new Some("") });
    return parse_port(uri_string, pieces$1);
  } else if (uri_string === "") {
    return new Ok(pieces.withFields({ host: new Some("") }));
  } else {
    return parse_host_outside_of_brackets(uri_string, pieces);
  }
}
function parse_userinfo_loop(loop$original, loop$uri_string, loop$pieces, loop$size) {
  while (true) {
    let original = loop$original;
    let uri_string = loop$uri_string;
    let pieces = loop$pieces;
    let size2 = loop$size;
    if (uri_string.startsWith("@") && size2 === 0) {
      let rest2 = uri_string.slice(1);
      return parse_host(rest2, pieces);
    } else if (uri_string.startsWith("@")) {
      let rest2 = uri_string.slice(1);
      let userinfo = string_codeunit_slice(original, 0, size2);
      let pieces$1 = pieces.withFields({ userinfo: new Some(userinfo) });
      return parse_host(rest2, pieces$1);
    } else if (uri_string === "") {
      return parse_host(original, pieces);
    } else if (uri_string.startsWith("/")) {
      return parse_host(original, pieces);
    } else if (uri_string.startsWith("?")) {
      return parse_host(original, pieces);
    } else if (uri_string.startsWith("#")) {
      return parse_host(original, pieces);
    } else {
      let $ = pop_codeunit(uri_string);
      let rest2 = $[1];
      loop$original = original;
      loop$uri_string = rest2;
      loop$pieces = pieces;
      loop$size = size2 + 1;
    }
  }
}
function parse_authority_pieces(string3, pieces) {
  return parse_userinfo_loop(string3, string3, pieces, 0);
}
function parse_authority_with_slashes(uri_string, pieces) {
  if (uri_string === "//") {
    return new Ok(pieces.withFields({ host: new Some("") }));
  } else if (uri_string.startsWith("//")) {
    let rest2 = uri_string.slice(2);
    return parse_authority_pieces(rest2, pieces);
  } else {
    return parse_path(uri_string, pieces);
  }
}
function parse_scheme_loop(loop$original, loop$uri_string, loop$pieces, loop$size) {
  while (true) {
    let original = loop$original;
    let uri_string = loop$uri_string;
    let pieces = loop$pieces;
    let size2 = loop$size;
    if (uri_string.startsWith("/") && size2 === 0) {
      return parse_authority_with_slashes(uri_string, pieces);
    } else if (uri_string.startsWith("/")) {
      let scheme = string_codeunit_slice(original, 0, size2);
      let pieces$1 = pieces.withFields({
        scheme: new Some(lowercase(scheme))
      });
      return parse_authority_with_slashes(uri_string, pieces$1);
    } else if (uri_string.startsWith("?") && size2 === 0) {
      let rest2 = uri_string.slice(1);
      return parse_query_with_question_mark(rest2, pieces);
    } else if (uri_string.startsWith("?")) {
      let rest2 = uri_string.slice(1);
      let scheme = string_codeunit_slice(original, 0, size2);
      let pieces$1 = pieces.withFields({
        scheme: new Some(lowercase(scheme))
      });
      return parse_query_with_question_mark(rest2, pieces$1);
    } else if (uri_string.startsWith("#") && size2 === 0) {
      let rest2 = uri_string.slice(1);
      return parse_fragment(rest2, pieces);
    } else if (uri_string.startsWith("#")) {
      let rest2 = uri_string.slice(1);
      let scheme = string_codeunit_slice(original, 0, size2);
      let pieces$1 = pieces.withFields({
        scheme: new Some(lowercase(scheme))
      });
      return parse_fragment(rest2, pieces$1);
    } else if (uri_string.startsWith(":") && size2 === 0) {
      return new Error(void 0);
    } else if (uri_string.startsWith(":")) {
      let rest2 = uri_string.slice(1);
      let scheme = string_codeunit_slice(original, 0, size2);
      let pieces$1 = pieces.withFields({
        scheme: new Some(lowercase(scheme))
      });
      return parse_authority_with_slashes(rest2, pieces$1);
    } else if (uri_string === "") {
      return new Ok(pieces.withFields({ path: original }));
    } else {
      let $ = pop_codeunit(uri_string);
      let rest2 = $[1];
      loop$original = original;
      loop$uri_string = rest2;
      loop$pieces = pieces;
      loop$size = size2 + 1;
    }
  }
}
function parse(uri_string) {
  let default_pieces = new Uri(
    new None(),
    new None(),
    new None(),
    new None(),
    "",
    new None(),
    new None()
  );
  return parse_scheme_loop(uri_string, uri_string, default_pieces, 0);
}
function to_string4(uri) {
  let parts = (() => {
    let $ = uri.fragment;
    if ($ instanceof Some) {
      let fragment2 = $[0];
      return toList(["#", fragment2]);
    } else {
      return toList([]);
    }
  })();
  let parts$1 = (() => {
    let $ = uri.query;
    if ($ instanceof Some) {
      let query = $[0];
      return prepend("?", prepend(query, parts));
    } else {
      return parts;
    }
  })();
  let parts$2 = prepend(uri.path, parts$1);
  let parts$3 = (() => {
    let $ = uri.host;
    let $1 = starts_with(uri.path, "/");
    if ($ instanceof Some && !$1 && $[0] !== "") {
      let host = $[0];
      return prepend("/", parts$2);
    } else {
      return parts$2;
    }
  })();
  let parts$4 = (() => {
    let $ = uri.host;
    let $1 = uri.port;
    if ($ instanceof Some && $1 instanceof Some) {
      let port = $1[0];
      return prepend(":", prepend(to_string(port), parts$3));
    } else {
      return parts$3;
    }
  })();
  let parts$5 = (() => {
    let $ = uri.scheme;
    let $1 = uri.userinfo;
    let $2 = uri.host;
    if ($ instanceof Some && $1 instanceof Some && $2 instanceof Some) {
      let s = $[0];
      let u = $1[0];
      let h = $2[0];
      return prepend(
        s,
        prepend(
          "://",
          prepend(u, prepend("@", prepend(h, parts$4)))
        )
      );
    } else if ($ instanceof Some && $1 instanceof None && $2 instanceof Some) {
      let s = $[0];
      let h = $2[0];
      return prepend(s, prepend("://", prepend(h, parts$4)));
    } else if ($ instanceof Some && $1 instanceof Some && $2 instanceof None) {
      let s = $[0];
      return prepend(s, prepend(":", parts$4));
    } else if ($ instanceof Some && $1 instanceof None && $2 instanceof None) {
      let s = $[0];
      return prepend(s, prepend(":", parts$4));
    } else if ($ instanceof None && $1 instanceof None && $2 instanceof Some) {
      let h = $2[0];
      return prepend("//", prepend(h, parts$4));
    } else {
      return parts$4;
    }
  })();
  return concat2(parts$5);
}

// build/dev/javascript/gleam_http/gleam/http.mjs
var Get = class extends CustomType {
};
var Post = class extends CustomType {
};
var Head = class extends CustomType {
};
var Put = class extends CustomType {
};
var Delete = class extends CustomType {
};
var Trace = class extends CustomType {
};
var Connect = class extends CustomType {
};
var Options = class extends CustomType {
};
var Patch = class extends CustomType {
};
var Http = class extends CustomType {
};
var Https = class extends CustomType {
};
function method_to_string(method) {
  if (method instanceof Connect) {
    return "connect";
  } else if (method instanceof Delete) {
    return "delete";
  } else if (method instanceof Get) {
    return "get";
  } else if (method instanceof Head) {
    return "head";
  } else if (method instanceof Options) {
    return "options";
  } else if (method instanceof Patch) {
    return "patch";
  } else if (method instanceof Post) {
    return "post";
  } else if (method instanceof Put) {
    return "put";
  } else if (method instanceof Trace) {
    return "trace";
  } else {
    let s = method[0];
    return s;
  }
}
function scheme_to_string(scheme) {
  if (scheme instanceof Http) {
    return "http";
  } else {
    return "https";
  }
}
function scheme_from_string(scheme) {
  let $ = lowercase(scheme);
  if ($ === "http") {
    return new Ok(new Http());
  } else if ($ === "https") {
    return new Ok(new Https());
  } else {
    return new Error(void 0);
  }
}

// build/dev/javascript/gleam_http/gleam/http/request.mjs
var Request = class extends CustomType {
  constructor(method, headers, body, scheme, host, port, path2, query) {
    super();
    this.method = method;
    this.headers = headers;
    this.body = body;
    this.scheme = scheme;
    this.host = host;
    this.port = port;
    this.path = path2;
    this.query = query;
  }
};
function to_uri(request) {
  return new Uri(
    new Some(scheme_to_string(request.scheme)),
    new None(),
    new Some(request.host),
    request.port,
    request.path,
    request.query,
    new None()
  );
}
function from_uri(uri) {
  return then$(
    (() => {
      let _pipe = uri.scheme;
      let _pipe$1 = unwrap(_pipe, "");
      return scheme_from_string(_pipe$1);
    })(),
    (scheme) => {
      return then$(
        (() => {
          let _pipe = uri.host;
          return to_result(_pipe, void 0);
        })(),
        (host) => {
          let req = new Request(
            new Get(),
            toList([]),
            "",
            scheme,
            host,
            uri.port,
            uri.path,
            uri.query
          );
          return new Ok(req);
        }
      );
    }
  );
}
function set_header(request, key, value4) {
  let headers = key_set(request.headers, lowercase(key), value4);
  return request.withFields({ headers });
}
function set_body(req, body) {
  let method = req.method;
  let headers = req.headers;
  let scheme = req.scheme;
  let host = req.host;
  let port = req.port;
  let path2 = req.path;
  let query = req.query;
  return new Request(method, headers, body, scheme, host, port, path2, query);
}
function set_method(req, method) {
  return req.withFields({ method });
}
function to(url) {
  let _pipe = url;
  let _pipe$1 = parse(_pipe);
  return then$(_pipe$1, from_uri);
}

// build/dev/javascript/gleam_http/gleam/http/response.mjs
var Response = class extends CustomType {
  constructor(status, headers, body) {
    super();
    this.status = status;
    this.headers = headers;
    this.body = body;
  }
};

// build/dev/javascript/gleam_javascript/gleam_javascript_ffi.mjs
var PromiseLayer = class _PromiseLayer {
  constructor(promise) {
    this.promise = promise;
  }
  static wrap(value4) {
    return value4 instanceof Promise ? new _PromiseLayer(value4) : value4;
  }
  static unwrap(value4) {
    return value4 instanceof _PromiseLayer ? value4.promise : value4;
  }
};
function resolve(value4) {
  return Promise.resolve(PromiseLayer.wrap(value4));
}
function then_await(promise, fn) {
  return promise.then((value4) => fn(PromiseLayer.unwrap(value4)));
}
function map_promise(promise, fn) {
  return promise.then(
    (value4) => PromiseLayer.wrap(fn(PromiseLayer.unwrap(value4)))
  );
}
function rescue(promise, fn) {
  return promise.catch((error) => fn(error));
}

// build/dev/javascript/gleam_javascript/gleam/javascript/promise.mjs
function tap(promise, callback) {
  let _pipe = promise;
  return map_promise(
    _pipe,
    (a2) => {
      callback(a2);
      return a2;
    }
  );
}
function try_await(promise, callback) {
  let _pipe = promise;
  return then_await(
    _pipe,
    (result) => {
      if (result.isOk()) {
        let a2 = result[0];
        return callback(a2);
      } else {
        let e = result[0];
        return resolve(new Error(e));
      }
    }
  );
}

// build/dev/javascript/gleam_fetch/ffi.mjs
async function raw_send(request) {
  try {
    return new Ok(await fetch(request));
  } catch (error) {
    return new Error(new NetworkError(error.toString()));
  }
}
function from_fetch_response(response) {
  return new Response(
    response.status,
    List.fromArray([...response.headers]),
    response
  );
}
function to_fetch_request(request) {
  let url = to_string4(to_uri(request));
  let method = method_to_string(request.method).toUpperCase();
  let options = {
    headers: make_headers(request.headers),
    method
  };
  if (method !== "GET" && method !== "HEAD")
    options.body = request.body;
  return new globalThis.Request(url, options);
}
function make_headers(headersList) {
  let headers = new globalThis.Headers();
  for (let [k, v] of headersList)
    headers.append(k.toLowerCase(), v);
  return headers;
}
async function read_text_body(response) {
  let body;
  try {
    body = await response.body.text();
  } catch (error) {
    return new Error(new UnableToReadBody());
  }
  return new Ok(response.withFields({ body }));
}

// build/dev/javascript/gleam_fetch/gleam/fetch.mjs
var NetworkError = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var UnableToReadBody = class extends CustomType {
};
function send(request) {
  let _pipe = request;
  let _pipe$1 = to_fetch_request(_pipe);
  let _pipe$2 = raw_send(_pipe$1);
  return try_await(
    _pipe$2,
    (resp) => {
      return resolve(new Ok(from_fetch_response(resp)));
    }
  );
}

// build/dev/javascript/lustre_http/lustre_http.mjs
var BadUrl = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var InternalServerError = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var JsonError = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var NetworkError2 = class extends CustomType {
};
var NotFound = class extends CustomType {
};
var OtherError = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};
var Unauthorized = class extends CustomType {
};
var ExpectTextResponse = class extends CustomType {
  constructor(run) {
    super();
    this.run = run;
  }
};
function do_send(req, expect, dispatch) {
  let _pipe = send(req);
  let _pipe$1 = try_await(_pipe, read_text_body);
  let _pipe$2 = map_promise(
    _pipe$1,
    (response) => {
      if (response.isOk()) {
        let res = response[0];
        return expect.run(new Ok(res));
      } else {
        return expect.run(new Error(new NetworkError2()));
      }
    }
  );
  let _pipe$3 = rescue(
    _pipe$2,
    (_) => {
      return expect.run(new Error(new NetworkError2()));
    }
  );
  tap(_pipe$3, dispatch);
  return void 0;
}
function post(url, body, expect) {
  return from(
    (dispatch) => {
      let $ = to(url);
      if ($.isOk()) {
        let req = $[0];
        let _pipe = req;
        let _pipe$1 = set_method(_pipe, new Post());
        let _pipe$2 = set_header(
          _pipe$1,
          "Content-Type",
          "application/json"
        );
        let _pipe$3 = set_body(_pipe$2, to_string3(body));
        return do_send(_pipe$3, expect, dispatch);
      } else {
        return dispatch(expect.run(new Error(new BadUrl(url))));
      }
    }
  );
}
function response_to_result(response) {
  if (response instanceof Response && (200 <= response.status && response.status <= 299)) {
    let status = response.status;
    let body = response.body;
    return new Ok(body);
  } else if (response instanceof Response && response.status === 401) {
    return new Error(new Unauthorized());
  } else if (response instanceof Response && response.status === 404) {
    return new Error(new NotFound());
  } else if (response instanceof Response && response.status === 500) {
    let body = response.body;
    return new Error(new InternalServerError(body));
  } else {
    let code = response.status;
    let body = response.body;
    return new Error(new OtherError(code, body));
  }
}
function expect_json(decoder2, to_msg2) {
  return new ExpectTextResponse(
    (response) => {
      let _pipe = response;
      let _pipe$1 = then$(_pipe, response_to_result);
      let _pipe$2 = then$(
        _pipe$1,
        (body) => {
          let $ = decode2(body, decoder2);
          if ($.isOk()) {
            let json = $[0];
            return new Ok(json);
          } else {
            let json_error = $[0];
            return new Error(new JsonError(json_error));
          }
        }
      );
      return to_msg2(_pipe$2);
    }
  );
}

// build/dev/javascript/lustre_ui/lustre/ui/button.mjs
function of(element3, attributes, children2) {
  return element3(
    prepend(
      class$("lustre-ui-button"),
      prepend(role("button"), attributes)
    ),
    children2
  );
}
function button2(attributes, children2) {
  return of(
    button,
    prepend(attribute("tabindex", "0"), attributes),
    children2
  );
}

// build/dev/javascript/lustre_ui/lustre/ui/card.mjs
function of2(element3, attributes, children2) {
  return element3(
    prepend(class$("lustre-ui-card"), attributes),
    children2
  );
}
function card(attributes, children2) {
  return of2(article, attributes, children2);
}
function header2(attributes, children2) {
  return header(
    prepend(class$("card-header"), attributes),
    children2
  );
}
function content(attributes, children2) {
  return main(
    prepend(class$("card-content"), attributes),
    children2
  );
}

// build/dev/javascript/lustre_ui/dom.ffi.mjs
var assigned_elements = (slot2) => {
  if (slot2 instanceof HTMLSlotElement) {
    return new Ok(List.fromArray(slot2.assignedElements()));
  }
  return new Error(List.fromArray([]));
};
var get_attribute = (name4) => (element3) => {
  if (!(element3 instanceof HTMLElement)) {
    return new Error(List.fromArray([]));
  }
  if (element3.hasAttribute(name4)) {
    return new Ok(element3.getAttribute(name4));
  } else {
    return new Error(List.fromArray([]));
  }
};
var get_element = (selector) => (element3) => {
  if (!("querySelector" in element3)) {
    return new Error(List.fromArray([]));
  }
  const result = element3.querySelector(selector);
  if (result) {
    return new Ok(result);
  } else {
    return new Error(List.fromArray([]));
  }
};
var get_root2 = (element3) => {
  if (!(element3 instanceof HTMLElement)) {
    return new Error(List.fromArray([]));
  }
  return new Ok(element3.getRootNode());
};
var focus = (element3) => {
  element3.focus();
};
var set_state = (value4, shadow_root) => {
  if (!(shadow_root instanceof ShadowRoot))
    return;
  if (!(shadow_root.host.internals instanceof ElementInternals))
    return;
  shadow_root.host.internals.states.add(value4);
};
var remove_state = (value4, shadow_root) => {
  if (!(shadow_root instanceof ShadowRoot))
    return;
  if (!(shadow_root.host.internals instanceof ElementInternals))
    return;
  shadow_root.host.internals.states.delete(value4);
};

// build/dev/javascript/lustre_ui/lustre/ui/data/bidict.mjs
function new$3() {
  return [new_map(), new_map()];
}
function get(bidict, key) {
  return map_get(bidict[0], key);
}
function get_inverse(bidict, key) {
  return map_get(bidict[1], key);
}
function min_inverse(bidict, compare4) {
  let _pipe = map_to_list(bidict[1]);
  let _pipe$1 = sort(_pipe, (a2, b) => {
    return compare4(a2[0], b[0]);
  });
  let _pipe$2 = first2(_pipe$1);
  return map2(_pipe$2, second);
}
function max_inverse(bidict, compare4) {
  let _pipe = map_to_list(bidict[1]);
  let _pipe$1 = sort(_pipe, (a2, b) => {
    return compare4(b[0], a2[0]);
  });
  let _pipe$2 = first2(_pipe$1);
  return map2(_pipe$2, second);
}
function next(bidict, key, increment) {
  let _pipe = get(bidict, key);
  let _pipe$1 = map2(_pipe, increment);
  return then$(
    _pipe$1,
    (_capture) => {
      return get_inverse(bidict, _capture);
    }
  );
}
function set(bidict, key, value4) {
  return [
    insert(bidict[0], key, value4),
    insert(bidict[1], value4, key)
  ];
}
function from_list2(entries) {
  return fold(
    entries,
    new$3(),
    (bidict, entry) => {
      return set(bidict, entry[0], entry[1]);
    }
  );
}
function indexed(values) {
  return index_fold(
    values,
    new$3(),
    (bidict, value4, index3) => {
      return set(bidict, value4, index3);
    }
  );
}

// build/dev/javascript/lustre_ui/lustre/ui/input.mjs
function input2(attributes) {
  return input(
    prepend(class$("lustre-ui-input"), attributes)
  );
}
function container(attributes, children2) {
  return div(
    prepend(class$("lustre-ui-input-container"), attributes),
    children2
  );
}

// build/dev/javascript/lustre/lustre/element/svg.mjs
var namespace = "http://www.w3.org/2000/svg";
function path(attrs) {
  return namespaced(namespace, "path", attrs, toList([]));
}

// build/dev/javascript/lustre_ui/lustre/ui/primitives/icon.mjs
function icon(attrs, path2) {
  return svg(
    prepend(
      attribute("viewBox", "0 0 15 15"),
      prepend(
        attribute("fill", "none"),
        prepend(class$("lustre-ui-icon"), attrs)
      )
    ),
    toList([
      path(
        toList([
          attribute("d", path2),
          attribute("fill", "currentColor"),
          attribute("fill-rule", "evenodd"),
          attribute("clip-rule", "evenodd")
        ])
      )
    ])
  );
}
function chevron_down(attrs) {
  return icon(
    attrs,
    "M3.13523 6.15803C3.3241 5.95657 3.64052 5.94637 3.84197 6.13523L7.5 9.56464L11.158 6.13523C11.3595 5.94637 11.6759 5.95657 11.8648 6.15803C12.0536 6.35949 12.0434 6.67591 11.842 6.86477L7.84197 10.6148C7.64964 10.7951 7.35036 10.7951 7.15803 10.6148L3.15803 6.86477C2.95657 6.67591 2.94637 6.35949 3.13523 6.15803Z"
  );
}
function check(attrs) {
  return icon(
    attrs,
    "M11.4669 3.72684C11.7558 3.91574 11.8369 4.30308 11.648 4.59198L7.39799 11.092C7.29783 11.2452 7.13556 11.3467 6.95402 11.3699C6.77247 11.3931 6.58989 11.3355 6.45446 11.2124L3.70446 8.71241C3.44905 8.48022 3.43023 8.08494 3.66242 7.82953C3.89461 7.57412 4.28989 7.55529 4.5453 7.78749L6.75292 9.79441L10.6018 3.90792C10.7907 3.61902 11.178 3.53795 11.4669 3.72684Z"
  );
}
function magnifying_glass(attrs) {
  return icon(
    attrs,
    "M10 6.5C10 8.433 8.433 10 6.5 10C4.567 10 3 8.433 3 6.5C3 4.567 4.567 3 6.5 3C8.433 3 10 4.567 10 6.5ZM9.30884 10.0159C8.53901 10.6318 7.56251 11 6.5 11C4.01472 11 2 8.98528 2 6.5C2 4.01472 4.01472 2 6.5 2C8.98528 2 11 4.01472 11 6.5C11 7.56251 10.6318 8.53901 10.0159 9.30884L12.8536 12.1464C13.0488 12.3417 13.0488 12.6583 12.8536 12.8536C12.6583 13.0488 12.3417 13.0488 12.1464 12.8536L9.30884 10.0159Z"
  );
}

// build/dev/javascript/decipher/decipher.mjs
function tagged_union(tag, variants) {
  let switch$ = from_list(variants);
  return (dynamic2) => {
    return try$(
      tag(dynamic2),
      (kind) => {
        let $ = map_get(switch$, kind);
        if ($.isOk()) {
          let decoder2 = $[0];
          return decoder2(dynamic2);
        } else {
          let tags = (() => {
            let _pipe = keys(switch$);
            let _pipe$1 = map(_pipe, inspect2);
            return join(_pipe$1, " | ");
          })();
          let path2 = (() => {
            let $1 = tag(identity(void 0));
            if (!$1.isOk() && $1[0].atLeastLength(1) && $1[0].head instanceof DecodeError) {
              let path3 = $1[0].head.path;
              return path3;
            } else {
              return toList([]);
            }
          })();
          return new Error(
            toList([new DecodeError(tags, inspect2(kind), path2)])
          );
        }
      }
    );
  };
}
function enum$(variants) {
  return tagged_union(
    string,
    map(
      variants,
      (_capture) => {
        return map_second(
          _capture,
          (variant) => {
            return (_) => {
              return new Ok(variant);
            };
          }
        );
      }
    )
  );
}
function bool_string(dynamic2) {
  return enum$(
    toList([
      ["true", true],
      ["True", true],
      ["on", true],
      ["On", true],
      ["yes", true],
      ["Yes", true],
      ["false", false],
      ["False", false],
      ["off", false],
      ["Off", false],
      ["no", false],
      ["No", false]
    ])
  )(dynamic2);
}

// build/dev/javascript/lustre_ui/scheduler.ffi.mjs
var after_paint = (k) => {
  window.requestAnimationFrame(k);
};

// build/dev/javascript/lustre_ui/lustre/ui/primitives/popover.mjs
var TopLeft = class extends CustomType {
};
var TopMiddle = class extends CustomType {
};
var TopRight = class extends CustomType {
};
var RightTop = class extends CustomType {
};
var RightMiddle = class extends CustomType {
};
var RightBottom = class extends CustomType {
};
var BottomLeft = class extends CustomType {
};
var BottomMiddle = class extends CustomType {
};
var BottomRight = class extends CustomType {
};
var LeftTop = class extends CustomType {
};
var LeftMiddle = class extends CustomType {
};
var WillExpand = class extends CustomType {
};
var Expanded = class extends CustomType {
};
var WillCollapse = class extends CustomType {
};
var Collapsing = class extends CustomType {
};
var Collapsed = class extends CustomType {
};
var ParentSetOpen = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var SchedulerDidTick = class extends CustomType {
};
var TransitionDidEnd = class extends CustomType {
};
var UserPressedTrigger = class extends CustomType {
};
function open(is_open) {
  return attribute(
    "aria-expanded",
    (() => {
      let _pipe = to_string2(is_open);
      return lowercase(_pipe);
    })()
  );
}
function anchor(direction) {
  return attribute(
    "anchor",
    (() => {
      if (direction instanceof TopLeft) {
        return "top-left";
      } else if (direction instanceof TopMiddle) {
        return "top-middle";
      } else if (direction instanceof TopRight) {
        return "top-right";
      } else if (direction instanceof RightTop) {
        return "right-top";
      } else if (direction instanceof RightMiddle) {
        return "right-middle";
      } else if (direction instanceof RightBottom) {
        return "right-bottom";
      } else if (direction instanceof BottomLeft) {
        return "bottom-left";
      } else if (direction instanceof BottomMiddle) {
        return "bottom-middle";
      } else if (direction instanceof BottomRight) {
        return "bottom-right";
      } else if (direction instanceof LeftTop) {
        return "left-top";
      } else if (direction instanceof LeftMiddle) {
        return "left-middle";
      } else {
        return "left-bottom";
      }
    })()
  );
}
function equal_width() {
  return attribute("equal-width", "");
}
function gap(value4) {
  return style(toList([["--gap", value4]]));
}
function on_open(handler) {
  return on2("open", (_) => {
    return new Ok(handler);
  });
}
function on_close(handler) {
  return on2("close", (_) => {
    return new Ok(handler);
  });
}
function on_attribute_change() {
  return from_list(
    toList([
      [
        "aria-expanded",
        (value4) => {
          let _pipe = value4;
          let _pipe$1 = bool_string(_pipe);
          return map2(
            _pipe$1,
            (var0) => {
              return new ParentSetOpen(var0);
            }
          );
        }
      ]
    ])
  );
}
function set_state2(value4) {
  return get_root(
    (_, root) => {
      return each(
        toList([
          "will-expand",
          "expanded",
          "will-collapse",
          "collapsing",
          "collapsed"
        ]),
        (state) => {
          let $ = state === value4;
          if ($) {
            return set_state(value4, root);
          } else {
            return remove_state(state, root);
          }
        }
      );
    }
  );
}
function init2(_) {
  let model = new Collapsed();
  let effect = batch(toList([set_state2("collapsed")]));
  return [model, effect];
}
function tick() {
  return from(
    (dispatch) => {
      return after_paint(() => {
        return dispatch(new SchedulerDidTick());
      });
    }
  );
}
function update(model, msg) {
  if (msg instanceof ParentSetOpen && msg[0] && model instanceof WillCollapse) {
    return [
      new WillExpand(),
      batch(toList([tick(), set_state2("will-expand")]))
    ];
  } else if (msg instanceof ParentSetOpen && msg[0] && model instanceof Collapsed) {
    return [
      new WillExpand(),
      batch(toList([tick(), set_state2("will-expand")]))
    ];
  } else if (msg instanceof ParentSetOpen && msg[0]) {
    return [model, none()];
  } else if (msg instanceof ParentSetOpen && !msg[0] && model instanceof WillExpand) {
    return [
      new WillCollapse(),
      batch(toList([tick(), set_state2("will-collapse")]))
    ];
  } else if (msg instanceof ParentSetOpen && !msg[0] && model instanceof Expanded) {
    return [
      new WillCollapse(),
      batch(toList([tick(), set_state2("will-collapse")]))
    ];
  } else if (msg instanceof ParentSetOpen && !msg[0]) {
    return [model, none()];
  } else if (msg instanceof SchedulerDidTick && model instanceof WillExpand) {
    return [new Expanded(), set_state2("expanded")];
  } else if (msg instanceof SchedulerDidTick && model instanceof WillCollapse) {
    return [new Collapsing(), set_state2("collapsing")];
  } else if (msg instanceof SchedulerDidTick) {
    return [model, none()];
  } else if (msg instanceof TransitionDidEnd && model instanceof Collapsing) {
    return [new Collapsed(), set_state2("collapsed")];
  } else if (msg instanceof TransitionDidEnd) {
    return [model, none()];
  } else if (msg instanceof UserPressedTrigger && model instanceof WillExpand) {
    return [
      model,
      batch(
        toList([
          emit2("close", null$()),
          emit2(
            "change",
            object2(toList([["open", bool(false)]]))
          )
        ])
      )
    ];
  } else if (msg instanceof UserPressedTrigger && model instanceof Expanded) {
    return [
      model,
      batch(
        toList([
          emit2("close", null$()),
          emit2(
            "change",
            object2(toList([["open", bool(false)]]))
          )
        ])
      )
    ];
  } else if (msg instanceof UserPressedTrigger && model instanceof WillCollapse) {
    return [
      model,
      batch(
        toList([
          emit2("open", null$()),
          emit2(
            "change",
            object2(toList([["open", bool(true)]]))
          )
        ])
      )
    ];
  } else if (msg instanceof UserPressedTrigger && model instanceof Collapsing) {
    return [
      model,
      batch(
        toList([
          emit2("open", null$()),
          emit2(
            "change",
            object2(toList([["open", bool(true)]]))
          )
        ])
      )
    ];
  } else {
    return [
      model,
      batch(
        toList([
          emit2("open", null$()),
          emit2(
            "change",
            object2(toList([["open", bool(true)]]))
          )
        ])
      )
    ];
  }
}
function handle_keydown(event2) {
  return try$(
    field("key", string)(event2),
    (key) => {
      if (key === "Enter") {
        prevent_default(event2);
        return new Ok(new UserPressedTrigger());
      } else if (key === " ") {
        prevent_default(event2);
        return new Ok(new UserPressedTrigger());
      } else {
        return new Error(toList([]));
      }
    }
  );
}
function view_trigger() {
  return slot(
    toList([
      name("trigger"),
      on_click(new UserPressedTrigger()),
      on2("keydown", handle_keydown)
    ])
  );
}
function handle_transitionend(_) {
  return new Ok(new TransitionDidEnd());
}
function view_popover(model) {
  return guard(
    isEqual(model, new Collapsed()),
    text2(""),
    () => {
      return div(
        toList([
          attribute("part", "popover-content"),
          on2("transitionend", handle_transitionend)
        ]),
        toList([slot(toList([name("popover")]))])
      );
    }
  );
}
function view(model) {
  return div(
    toList([style(toList([["position", "relative"]]))]),
    toList([view_trigger(), view_popover(model)])
  );
}
var name2 = "lustre-ui-popover";
function register() {
  let app = component(init2, update, view, on_attribute_change());
  return make_lustre_client_component(app, name2);
}
function popover(attributes, trigger, content2) {
  return element(
    name2,
    attributes,
    toList([
      div(toList([attribute("slot", "trigger")]), toList([trigger])),
      div(toList([attribute("slot", "popover")]), toList([content2]))
    ])
  );
}

// build/dev/javascript/lustre_ui/lustre/ui/combobox.mjs
var Item = class extends CustomType {
  constructor(value4, label2) {
    super();
    this.value = value4;
    this.label = label2;
  }
};
var Model2 = class extends CustomType {
  constructor(expanded, value4, placeholder, query, intent, intent_strategy, options) {
    super();
    this.expanded = expanded;
    this.value = value4;
    this.placeholder = placeholder;
    this.query = query;
    this.intent = intent;
    this.intent_strategy = intent_strategy;
    this.options = options;
  }
};
var ByIndex = class extends CustomType {
};
var ByLength = class extends CustomType {
};
var Options2 = class extends CustomType {
  constructor(all2, filtered, lookup_label, lookup_index) {
    super();
    this.all = all2;
    this.filtered = filtered;
    this.lookup_label = lookup_label;
    this.lookup_index = lookup_index;
  }
};
var DomBlurredTrigger = class extends CustomType {
};
var DomFocusedTrigger = class extends CustomType {
};
var ParentChangedChildren = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var ParentSetPlaceholder = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var ParentSetStrategy = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var ParentSetValue = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var UserChangedQuery = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var UserClosedMenu = class extends CustomType {
};
var UserHoveredOption = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var UserOpenedMenu = class extends CustomType {
};
var UserPressedDown = class extends CustomType {
};
var UserPressedEnd = class extends CustomType {
};
var UserPressedEnter = class extends CustomType {
};
var UserPressedEscape = class extends CustomType {
};
var UserPressedHome = class extends CustomType {
};
var UserPressedUp = class extends CustomType {
};
var UserSelectedOption = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
function option(value4, label2) {
  return new Item(value4, label2);
}
function value3(value4) {
  return value(value4);
}
function on_change(handler) {
  return on2(
    "change",
    (event2) => {
      return try$(
        field("detail", dynamic)(event2),
        (detail) => {
          return try$(
            field("value", string)(detail),
            (value4) => {
              return new Ok(handler(value4));
            }
          );
        }
      );
    }
  );
}
function intent_from_query(query, strategy, options) {
  return guard(
    query === "",
    new None(),
    () => {
      let query$1 = lowercase(query);
      let matches = filter(
        options.all,
        (option2) => {
          let _pipe2 = option2.label;
          let _pipe$12 = lowercase(_pipe2);
          return contains_string(_pipe$12, query$1);
        }
      );
      let sorted = sort(
        matches,
        (a2, b) => {
          let a_label = lowercase(a2.label);
          let b_label = lowercase(b.label);
          let a_starts_with_query = starts_with(a_label, query$1);
          let b_starts_with_query = starts_with(b_label, query$1);
          let $ = get(options.lookup_index, a2.value);
          if (!$.isOk()) {
            throw makeError(
              "let_assert",
              "lustre/ui/combobox",
              340,
              "",
              "Pattern match failed, no pattern matched the value.",
              { value: $ }
            );
          }
          let a_index = $[0];
          let $1 = get(options.lookup_index, b.value);
          if (!$1.isOk()) {
            throw makeError(
              "let_assert",
              "lustre/ui/combobox",
              341,
              "",
              "Pattern match failed, no pattern matched the value.",
              { value: $1 }
            );
          }
          let b_index = $1[0];
          let a_length = string_length(a_label);
          let b_length = string_length(b_label);
          if (a_starts_with_query && !b_starts_with_query) {
            return new Lt();
          } else if (!a_starts_with_query && b_starts_with_query) {
            return new Gt();
          } else if (strategy instanceof ByIndex) {
            return compare(a_index, b_index);
          } else {
            return compare(a_length, b_length);
          }
        }
      );
      let _pipe = sorted;
      let _pipe$1 = first2(_pipe);
      let _pipe$2 = map2(_pipe$1, (option2) => {
        return option2.value;
      });
      return from_result(_pipe$2);
    }
  );
}
function on_attribute_change2() {
  return from_list(
    toList([
      [
        "value",
        (value4) => {
          let _pipe = value4;
          let _pipe$1 = string(_pipe);
          return map2(
            _pipe$1,
            (var0) => {
              return new ParentSetValue(var0);
            }
          );
        }
      ],
      [
        "placeholder",
        (value4) => {
          let _pipe = value4;
          let _pipe$1 = string(_pipe);
          return map2(
            _pipe$1,
            (var0) => {
              return new ParentSetPlaceholder(var0);
            }
          );
        }
      ],
      [
        "strategy",
        (value4) => {
          let $ = string(value4);
          if ($.isOk() && $[0] === "by-index") {
            return new Ok(new ParentSetStrategy(new ByIndex()));
          } else if ($.isOk() && $[0] === "by-length") {
            return new Ok(new ParentSetStrategy(new ByLength()));
          } else {
            return new Error(toList([]));
          }
        }
      ]
    ])
  );
}
function set_state3(value4) {
  return get_root((_, root) => {
    return set_state(value4, root);
  });
}
function init3(_) {
  let model = new Model2(
    false,
    "",
    "Select an option...",
    "",
    new None(),
    new ByIndex(),
    new Options2(toList([]), toList([]), new$3(), new$3())
  );
  let effect = batch(toList([set_state3("empty")]));
  return [model, effect];
}
function remove_state2(value4) {
  return get_root(
    (_, root) => {
      return remove_state(value4, root);
    }
  );
}
function update2(model, msg) {
  if (msg instanceof DomBlurredTrigger) {
    return [model, remove_state2("trigger-focus")];
  } else if (msg instanceof DomFocusedTrigger) {
    return [model, set_state3("trigger-focus")];
  } else if (msg instanceof ParentChangedChildren) {
    let all2 = msg[0];
    let lookup_label = from_list2(
      map(all2, (item) => {
        return [item.value, item.label];
      })
    );
    let lookup_index = indexed(
      map(all2, (item) => {
        return item.value;
      })
    );
    let filtered = filter(
      all2,
      (option2) => {
        let _pipe = lowercase(option2.label);
        return contains_string(_pipe, lowercase(model.query));
      }
    );
    let options = new Options2(all2, filtered, lookup_label, lookup_index);
    let intent = new None();
    let model$1 = model.withFields({ options, intent });
    let effect = none();
    return [model$1, effect];
  } else if (msg instanceof ParentSetPlaceholder) {
    let placeholder$1 = msg[0];
    return [model.withFields({ placeholder: placeholder$1 }), none()];
  } else if (msg instanceof ParentSetStrategy) {
    let strategy = msg[0];
    return [model.withFields({ intent_strategy: strategy }), none()];
  } else if (msg instanceof ParentSetValue) {
    let value$1 = msg[0];
    let model$1 = model.withFields({
      value: value$1,
      intent: new Some(value$1)
    });
    let effect = (() => {
      if (value$1 === "") {
        return set_state3("empty");
      } else {
        return remove_state2("empty");
      }
    })();
    return [model$1, effect];
  } else if (msg instanceof UserChangedQuery) {
    let query = msg[0];
    let filtered = filter(
      model.options.all,
      (option2) => {
        let _pipe = lowercase(option2.label);
        return contains_string(_pipe, lowercase(query));
      }
    );
    let options = model.options.withFields({ filtered });
    let intent = intent_from_query(query, model.intent_strategy, model.options);
    let model$1 = model.withFields({
      query,
      intent,
      options
    });
    let effect = none();
    return [model$1, effect];
  } else if (msg instanceof UserClosedMenu) {
    let model$1 = model.withFields({
      expanded: false,
      intent: new None()
    });
    let effect = remove_state2("expanded");
    return [model$1, effect];
  } else if (msg instanceof UserHoveredOption) {
    let intent = msg[0];
    return [
      model.withFields({ intent: new Some(intent) }),
      none()
    ];
  } else if (msg instanceof UserOpenedMenu) {
    let model$1 = model.withFields({ expanded: true });
    let effect = set_state3("expanded");
    return [model$1, effect];
  } else if (msg instanceof UserPressedDown) {
    let intent = (() => {
      let $ = model.intent;
      if ($ instanceof Some) {
        let intent2 = $[0];
        let _pipe = next(
          model.options.lookup_index,
          intent2,
          (_capture) => {
            return add(_capture, 1);
          }
        );
        let _pipe$1 = or(_pipe, new Ok(intent2));
        return from_result(_pipe$1);
      } else {
        let _pipe = min_inverse(
          model.options.lookup_index,
          compare
        );
        return from_result(_pipe);
      }
    })();
    let model$1 = model.withFields({ intent });
    let effect = none();
    return [model$1, effect];
  } else if (msg instanceof UserPressedEnd) {
    let intent = (() => {
      let _pipe = model.options.lookup_index;
      let _pipe$1 = max_inverse(_pipe, compare);
      return from_result(_pipe$1);
    })();
    let model$1 = model.withFields({ intent });
    let effect = none();
    return [model$1, effect];
  } else if (msg instanceof UserPressedEnter) {
    let effect = (() => {
      let $ = model.intent;
      if ($ instanceof Some) {
        let value$1 = $[0];
        return emit2(
          "change",
          object2(toList([["value", string2(value$1)]]))
        );
      } else {
        return none();
      }
    })();
    return [model, effect];
  } else if (msg instanceof UserPressedEscape) {
    let model$1 = model.withFields({
      expanded: false,
      intent: new None()
    });
    let effect = remove_state2("expanded");
    return [model$1, effect];
  } else if (msg instanceof UserPressedHome) {
    let intent = (() => {
      let _pipe = model.options.lookup_index;
      let _pipe$1 = min_inverse(_pipe, compare);
      return from_result(_pipe$1);
    })();
    let model$1 = model.withFields({ intent });
    let effect = none();
    return [model$1, effect];
  } else if (msg instanceof UserPressedUp) {
    let intent = (() => {
      let $ = model.intent;
      if ($ instanceof Some) {
        let intent2 = $[0];
        let _pipe = next(
          model.options.lookup_index,
          intent2,
          (_capture) => {
            return subtract(_capture, 1);
          }
        );
        let _pipe$1 = or(_pipe, new Ok(intent2));
        return from_result(_pipe$1);
      } else {
        let _pipe = max_inverse(
          model.options.lookup_index,
          compare
        );
        return from_result(_pipe);
      }
    })();
    let model$1 = model.withFields({ intent });
    let effect = none();
    return [model$1, effect];
  } else {
    let value$1 = msg[0];
    let intent = (() => {
      let _pipe = value$1;
      let _pipe$1 = ((_capture) => {
        return get(model.options.lookup_label, _capture);
      })(_pipe);
      return from_result(_pipe$1);
    })();
    let model$1 = model.withFields({ intent });
    let effect = emit2(
      "change",
      object2(toList([["value", string2(value$1)]]))
    );
    return [model$1, effect];
  }
}
function handle_slot_change(event2) {
  return try$(
    field("target", assigned_elements)(event2),
    (children2) => {
      return try$(
        list(
          (el) => {
            return decode3(
              (tag, value4, label2) => {
                return [tag, value4, label2];
              },
              field("tagName", string),
              get_attribute("value"),
              field("textContent", string)
            )(el);
          }
        )(children2),
        (options) => {
          let _pipe = options;
          let _pipe$1 = fold_right(
            _pipe,
            [toList([]), new$2()],
            (acc, option2) => {
              let tag = option2[0];
              let value$1 = option2[1];
              let label2 = option2[2];
              return guard(
                tag !== "LUSTRE-UI-COMBOBOX-OPTION",
                acc,
                () => {
                  return guard(
                    contains(acc[1], value$1),
                    acc,
                    () => {
                      let seen = insert2(acc[1], value$1);
                      let options$1 = prepend(
                        new Item(value$1, label2),
                        acc[0]
                      );
                      return [options$1, seen];
                    }
                  );
                }
              );
            }
          );
          let _pipe$2 = first(_pipe$1);
          let _pipe$3 = new ParentChangedChildren(_pipe$2);
          return new Ok(_pipe$3);
        }
      );
    }
  );
}
function handle_popover_click(event2, will_open) {
  return try$(
    field("currentTarget", dynamic)(event2),
    (target) => {
      return try$(
        get_element("input")(target),
        (input3) => {
          if (will_open) {
            after_paint(() => {
              return focus(input3);
            });
          } else {
          }
          return new Error(toList([]));
        }
      );
    }
  );
}
function handle_popover_keydown(event2, will_open) {
  return try$(
    field("key", string)(event2),
    (key) => {
      return try$(
        field("currentTarget", dynamic)(event2),
        (target) => {
          return try$(
            get_element("input")(target),
            (input3) => {
              if (key === "Enter" && will_open) {
                after_paint(() => {
                  return focus(input3);
                });
                return new Error(toList([]));
              } else if (key === " " && will_open) {
                after_paint(() => {
                  return focus(input3);
                });
                return new Error(toList([]));
              } else {
                return new Error(toList([]));
              }
            }
          );
        }
      );
    }
  );
}
function view_trigger2(value4, placeholder, options) {
  let label2 = (() => {
    let _pipe = value4;
    let _pipe$1 = ((_capture) => {
      return get(options.lookup_label, _capture);
    })(_pipe);
    return unwrap2(_pipe$1, placeholder);
  })();
  return button(
    toList([
      attribute("part", "combobox-trigger"),
      attribute("tabindex", "0"),
      on_focus(new DomFocusedTrigger()),
      on_blur(new DomBlurredTrigger())
    ]),
    toList([
      span(
        toList([
          attribute("part", "combobox-trigger-label"),
          class$(
            (() => {
              if (label2 === "") {
                return "empty";
              } else {
                return "";
              }
            })()
          )
        ]),
        toList([text2(label2)])
      ),
      chevron_down(toList([attribute("part", "combobox-trigger-icon")]))
    ])
  );
}
function handle_input_keydown(event2) {
  return try$(
    field("key", string)(event2),
    (key) => {
      if (key === "ArrowDown") {
        prevent_default(event2);
        return new Ok(new UserPressedDown());
      } else if (key === "ArrowEnd") {
        prevent_default(event2);
        return new Ok(new UserPressedEnd());
      } else if (key === "Enter") {
        prevent_default(event2);
        return new Ok(new UserPressedEnter());
      } else if (key === "Escape") {
        return try$(
          field("target", get_root2)(event2),
          (root) => {
            return try$(
              get_element("button")(root),
              (trigger) => {
                after_paint(() => {
                  return focus(trigger);
                });
                prevent_default(event2);
                return new Ok(new UserPressedEscape());
              }
            );
          }
        );
      } else if (key === "Home") {
        prevent_default(event2);
        return new Ok(new UserPressedHome());
      } else if (key === "ArrowUp") {
        prevent_default(event2);
        return new Ok(new UserPressedUp());
      } else if (key === "Tab") {
        return new Ok(new UserClosedMenu());
      } else {
        return new Error(toList([]));
      }
    }
  );
}
function view_input(query) {
  return container(
    toList([attribute("part", "combobox-input")]),
    toList([
      magnifying_glass(toList([])),
      input2(
        toList([
          style(
            toList([
              ["width", "100%"],
              ["border-bottom-left-radius", "0px"],
              ["border-bottom-right-radius", "0px"]
            ])
          ),
          autocomplete("off"),
          on_input((var0) => {
            return new UserChangedQuery(var0);
          }),
          on2("keydown", handle_input_keydown),
          value(query)
        ])
      )
    ])
  );
}
function view_option(option2, value4, intent, last) {
  let is_selected = option2.value === value4;
  let is_intent = isEqual(new Some(option2.value), intent);
  let icon2 = (() => {
    if (is_selected) {
      return (_capture) => {
        return check(_capture);
      };
    } else {
      return (_capture) => {
        return span(_capture, toList([]));
      };
    }
  })();
  let parts = toList([
    "combobox-option",
    (() => {
      if (is_intent) {
        return "intent";
      } else {
        return "";
      }
    })(),
    (() => {
      if (last) {
        return "last";
      } else {
        return "";
      }
    })()
  ]);
  return li(
    toList([
      attribute("part", join(parts, " ")),
      attribute("value", option2.value),
      on_mouse_over(new UserHoveredOption(option2.value)),
      on_mouse_down(new UserSelectedOption(option2.value))
    ]),
    toList([
      icon2(
        toList([
          style(toList([["height", "1rem"], ["width", "1rem"]]))
        ])
      ),
      span(
        toList([style(toList([["flex", "1 1 0%"]]))]),
        toList([
          element(
            "slot",
            toList([name("option-" + option2.value)]),
            toList([text2(option2.label)])
          )
        ])
      )
    ])
  );
}
function do_view_options(options, value4, intent) {
  if (options.hasLength(0)) {
    return toList([]);
  } else if (options.hasLength(1)) {
    let option$1 = options.head;
    return toList([[option$1.value, view_option(option$1, value4, intent, true)]]);
  } else {
    let option$1 = options.head;
    let rest2 = options.tail;
    return prepend(
      [option$1.label, view_option(option$1, value4, intent, false)],
      do_view_options(rest2, value4, intent)
    );
  }
}
function view_options(options, value4, intent) {
  return keyed(
    (_capture) => {
      return ul(toList([]), _capture);
    },
    do_view_options(options.filtered, value4, intent)
  );
}
function view2(model) {
  return fragment(
    toList([
      slot(
        toList([
          style(toList([["display", "none"]])),
          on2("slotchange", handle_slot_change)
        ])
      ),
      popover(
        toList([
          anchor(new BottomMiddle()),
          equal_width(),
          gap("var(--padding-y)"),
          on_close(new UserClosedMenu()),
          on_open(new UserOpenedMenu()),
          open(model.expanded),
          on2(
            "click",
            (_capture) => {
              return handle_popover_click(_capture, !model.expanded);
            }
          ),
          on2(
            "keydown",
            (_capture) => {
              return handle_popover_keydown(_capture, !model.expanded);
            }
          )
        ]),
        view_trigger2(model.value, model.placeholder, model.options),
        div(
          toList([attribute("part", "combobox-options")]),
          toList([
            view_input(model.query),
            view_options(model.options, model.value, model.intent)
          ])
        )
      )
    ])
  );
}
var name3 = "lustre-ui-combobox";
function register2() {
  let $ = register();
  if ($.isOk() && !$[0]) {
    let app = component(init3, update2, view2, on_attribute_change2());
    return make_lustre_client_component(app, name3);
  } else if (!$.isOk() && $[0] instanceof ComponentAlreadyRegistered) {
    let app = component(init3, update2, view2, on_attribute_change2());
    return make_lustre_client_component(app, name3);
  } else {
    let error = $;
    return error;
  }
}
function combobox(attributes, children2) {
  return keyed(
    (_capture) => {
      return element(name3, attributes, _capture);
    },
    map(
      children2,
      (item) => {
        let el = element(
          "lustre-ui-combobox-option",
          toList([value(item.value)]),
          toList([text2(item.label)])
        );
        return [item.value, el];
      }
    )
  );
}

// build/dev/javascript/shared/shared/data_types.mjs
var RawTrade = class extends CustomType {
  constructor(ticker, ticker_error, date, date_error, price, price_error, quantity, quantity_error, action, action_error, error, success) {
    super();
    this.ticker = ticker;
    this.ticker_error = ticker_error;
    this.date = date;
    this.date_error = date_error;
    this.price = price;
    this.price_error = price_error;
    this.quantity = quantity;
    this.quantity_error = quantity_error;
    this.action = action;
    this.action_error = action_error;
    this.error = error;
    this.success = success;
  }
};
function new_raw_trade() {
  return new RawTrade(
    "",
    new None(),
    "",
    new None(),
    "",
    new None(),
    "",
    new None(),
    "",
    new None(),
    new None(),
    new None()
  );
}
function rawtrade_to_json(state) {
  return object2(
    toList([
      ["ticker", string2(state.ticker)],
      ["ticker_error", string2(unwrap(state.ticker_error, ""))],
      ["price", string2(state.price)],
      ["price_error", string2(unwrap(state.price_error, ""))],
      ["quantity", string2(state.quantity)],
      ["quantity_error", string2(unwrap(state.quantity_error, ""))],
      ["date", string2(state.date)],
      ["date_error", string2(unwrap(state.date_error, ""))],
      ["action", string2(state.action)],
      ["action_error", string2(unwrap(state.action_error, ""))],
      ["error", string2(unwrap(state.error, ""))],
      ["success", string2(unwrap(state.success, ""))]
    ])
  );
}
function reset_errors(trade) {
  return trade.withFields({
    ticker_error: new None(),
    price_error: new None(),
    quantity_error: new None(),
    date_error: new None(),
    action_error: new None(),
    error: new None(),
    success: new None()
  });
}

// build/dev/javascript/shared/js_utils.mjs
function full_url(path2) {
  path2 = path2.replace("^/", path2);
  return window.location.origin + "/" + path2;
}

// build/dev/javascript/shared/shared.mjs
function state_from_dynamic_best_case(root, client_state) {
  let extract_string_field = (field_name, default$) => {
    let _pipe = field(field_name, string)(root);
    let _pipe$1 = unwrap2(_pipe, default$);
    return trim(_pipe$1);
  };
  let to_option = (str) => {
    if (str === "") {
      return new None();
    } else {
      let s = str;
      return new Some(s);
    }
  };
  return new RawTrade(
    extract_string_field("ticker", client_state.ticker),
    (() => {
      let _pipe = extract_string_field("ticker_error", "");
      return to_option(_pipe);
    })(),
    extract_string_field("date", client_state.date),
    (() => {
      let _pipe = extract_string_field("date_error", "");
      return to_option(_pipe);
    })(),
    extract_string_field("price", client_state.price),
    (() => {
      let _pipe = extract_string_field("price_error", "");
      return to_option(_pipe);
    })(),
    extract_string_field("quantity", client_state.quantity),
    (() => {
      let _pipe = extract_string_field("quantity_error", "");
      return to_option(_pipe);
    })(),
    extract_string_field("action", client_state.action),
    (() => {
      let _pipe = extract_string_field("action_error", "");
      return to_option(_pipe);
    })(),
    (() => {
      let _pipe = extract_string_field("error", "");
      return to_option(_pipe);
    })(),
    (() => {
      let _pipe = extract_string_field("success", "");
      return to_option(_pipe);
    })()
  );
}
function state_from_string_best_case(resp, client_state) {
  let st = decode2(
    resp,
    (root) => {
      let _pipe = state_from_dynamic_best_case(root, client_state);
      return new Ok(_pipe);
    }
  );
  if (st.isOk()) {
    let state = st[0];
    return state;
  } else {
    return client_state.withFields({
      error: new Some("error parsing response into types.RawTrade Object")
    });
  }
}

// build/dev/javascript/trades_client/pages/manual_trade/manual_trade.mjs
var TickerUpdated = class extends CustomType {
  constructor(value4) {
    super();
    this.value = value4;
  }
};
var QuantityUpdated = class extends CustomType {
  constructor(value4) {
    super();
    this.value = value4;
  }
};
var PriceUpdated = class extends CustomType {
  constructor(value4) {
    super();
    this.value = value4;
  }
};
var ActionUpdated = class extends CustomType {
  constructor(value4) {
    super();
    this.value = value4;
  }
};
var DateUpdated = class extends CustomType {
  constructor(value4) {
    super();
    this.value = value4;
  }
};
var TradeValuesEntered = class extends CustomType {
};
var TradeValuesResponseReceived = class extends CustomType {
  constructor(response_state) {
    super();
    this.response_state = response_state;
  }
};
var State = class extends CustomType {
  constructor(raw_trade, effect, waiting) {
    super();
    this.raw_trade = raw_trade;
    this.effect = effect;
    this.waiting = waiting;
  }
};
function init4(_) {
  let state = new State(new_raw_trade(), none(), false);
  return [state, state.effect];
}
function populate_state_with_error_response(result, client_state) {
  if (result.isOk()) {
    return new_raw_trade().withFields({
      success: new Some("Trade Saved Successfully")
    });
  } else {
    let err = result[0];
    if (err instanceof OtherError) {
      let err_msg = err[1];
      return state_from_string_best_case(err_msg, client_state);
    } else if (err instanceof JsonError) {
      return client_state.withFields({
        error: new Some("Error parsing response JSON")
      });
    } else if (err instanceof Unauthorized) {
      return client_state.withFields({ error: new Some("Unauthorized Request") });
    } else if (err instanceof NetworkError2) {
      return client_state.withFields({ error: new Some("Network Error") });
    } else if (err instanceof BadUrl) {
      return client_state.withFields({ error: new Some("Bad URL") });
    } else if (err instanceof NotFound) {
      return client_state.withFields({ error: new Some("Not Found") });
    } else {
      return client_state.withFields({
        error: new Some("Server Error Saving Trade")
      });
    }
  }
}
function populate_state_with_response(target) {
  let _pipe = state_from_dynamic_best_case(
    target,
    new_raw_trade()
  );
  return new Ok(_pipe);
}
function form_control_style(styles) {
  return style(
    append(styles, toList([["display", "flex"], ["width", "650px"]]))
  );
}
function control_label(label2) {
  return label(
    toList([style(toList([["width", "25%"]]))]),
    toList([text2(label2 + ": ")])
  );
}
function control_with_error_styles() {
  return toList([
    ["display", "flex"],
    ["flex-direction", "column"],
    ["flex-grow", "1"]
  ]);
}
function submit_button(state) {
  let $ = state.waiting;
  if ($) {
    return button2(
      toList([disabled(true)]),
      toList([text2("Waiting")])
    );
  } else {
    return button2(
      toList([on_click(new TradeValuesEntered())]),
      toList([text2("Save")])
    );
  }
}
function to_msg(event2, msg) {
  return try$(
    field("target", dynamic)(event2),
    (target) => {
      debug("Target " + inspect2(target));
      return try$(
        field("value", string)(target),
        (value4) => {
          debug("Called " + value4);
          return new Ok(msg(value4));
        }
      );
    }
  );
}
function show_page_error(error) {
  if (error instanceof Some) {
    let msg = error[0];
    return span(
      toList([style(toList([["color", "red"]]))]),
      toList([text2(msg)])
    );
  } else {
    return span(toList([]), toList([]));
  }
}
function show_page_success(msg) {
  if (msg instanceof Some) {
    let msg$1 = msg[0];
    return span(
      toList([style(toList([["color", "green"]]))]),
      toList([text2(msg$1)])
    );
  } else {
    return span(toList([]), toList([]));
  }
}
function show_error(error) {
  if (error instanceof None) {
    return span(toList([]), toList([]));
  } else {
    let error$1 = error[0];
    return span(
      toList([
        style(
          toList([
            ["color", "red"],
            ["display", "flex"],
            ["justify-content", "flex-end"]
          ])
        )
      ]),
      toList([text2(error$1)])
    );
  }
}
function input_element(name4, value4, label2, msg, error) {
  return div(
    toList([form_control_style(toList([]))]),
    toList([
      control_label(label2),
      div(
        toList([style(control_with_error_styles())]),
        toList([
          input2(
            toList([
              name(name4),
              attribute("maxlength", "10"),
              attribute("minlength", "1"),
              value(value4),
              on2(
                "change",
                (_capture) => {
                  return to_msg(_capture, msg);
                }
              )
            ])
          ),
          show_error(error)
        ])
      )
    ])
  );
}
function action_choices(trade) {
  let buy = "Buy";
  let sell = "Sell";
  return div(
    toList([form_control_style(toList([]))]),
    toList([
      control_label("Action"),
      div(
        toList([style(control_with_error_styles())]),
        toList([
          combobox(
            toList([
              style(toList([["flex-grow", "1"]])),
              value3(trade.action),
              on_change((var0) => {
                return new ActionUpdated(var0);
              })
            ]),
            toList([
              option("", ""),
              option(buy, buy),
              option(sell, sell)
            ])
          ),
          show_error(trade.action_error)
        ])
      )
    ])
  );
}
function create_form(state) {
  let trade = state.raw_trade;
  let action_selected = trade.action;
  debug(action_selected);
  return card(
    toList([style(toList([]))]),
    toList([
      header2(
        toList([]),
        toList([
          h4(
            toList([
              style(
                toList([
                  ["width", "100%"],
                  ["display", "flex"],
                  ["justify-content", "center"]
                ])
              )
            ]),
            toList([text2("Trade Data")])
          )
        ])
      ),
      content(
        toList([]),
        toList([
          div(
            toList([
              style(
                toList([
                  ["width", "100%"],
                  ["min-height", "300px"],
                  ["display", "flex"],
                  ["flex-direction", "column"],
                  ["justify-content", "space-evenly"],
                  ["gap", "20px"],
                  ["align-items", "center"]
                ])
              )
            ]),
            toList([
              show_page_error(trade.error),
              show_page_success(trade.success),
              input_element(
                "ticker",
                trade.ticker,
                "Ticker",
                (var0) => {
                  return new TickerUpdated(var0);
                },
                trade.ticker_error
              ),
              input_element(
                "quantity",
                trade.quantity,
                "Quantity",
                (var0) => {
                  return new QuantityUpdated(var0);
                },
                trade.quantity_error
              ),
              input_element(
                "price",
                trade.price,
                "Price",
                (var0) => {
                  return new PriceUpdated(var0);
                },
                trade.price_error
              ),
              input_element(
                "date",
                trade.date,
                "Date (YYYY-MM-DD)",
                (var0) => {
                  return new DateUpdated(var0);
                },
                trade.date_error
              ),
              action_choices(trade),
              div(
                toList([
                  form_control_style(
                    toList([
                      ["justify-content", "flex-end"],
                      ["align-items", "center"],
                      ["gap", "10px"]
                    ])
                  )
                ]),
                toList([
                  submit_button(state),
                  a(toList([href("/home")]), toList([text2("Cancel")]))
                ])
              )
            ])
          )
        ])
      )
    ])
  );
}
var path_prefix = "/manual-trade";
function post_api(state) {
  let full_url2 = full_url(path_prefix);
  debug(full_url2);
  return post(
    full_url2,
    rawtrade_to_json(state),
    expect_json(
      populate_state_with_response,
      (result) => {
        let _pipe = populate_state_with_error_response(result, state);
        return new TradeValuesResponseReceived(_pipe);
      }
    )
  );
}
function update3(mystate, msg) {
  let state = mystate.raw_trade;
  let new_state = (() => {
    if (msg instanceof TickerUpdated) {
      let value4 = msg.value;
      return new State(
        state.withFields({ ticker: value4 }),
        none(),
        false
      );
    } else if (msg instanceof PriceUpdated) {
      let value4 = msg.value;
      return new State(
        state.withFields({ price: value4 }),
        none(),
        false
      );
    } else if (msg instanceof QuantityUpdated) {
      let value4 = msg.value;
      return new State(
        state.withFields({ quantity: value4 }),
        none(),
        false
      );
    } else if (msg instanceof DateUpdated) {
      let value4 = msg.value;
      return new State(state.withFields({ date: value4 }), none(), false);
    } else if (msg instanceof ActionUpdated) {
      let value4 = msg.value;
      return new State(
        state.withFields({ action: value4 }),
        none(),
        false
      );
    } else if (msg instanceof TradeValuesEntered) {
      let with_no_errors = reset_errors(state);
      return new State(with_no_errors, post_api(with_no_errors), true);
    } else {
      let response_state = msg.response_state;
      return new State(response_state, none(), false);
    }
  })();
  return [new_state, new_state.effect];
}
function main2() {
  debug("Manual Trade Starting");
  let $ = register2();
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "pages/manual_trade/manual_trade",
      40,
      "main",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    );
  }
  let $1 = start2(
    application(init4, update3, create_form),
    "#app",
    void 0
  );
  if ($1.isOk()) {
    return debug("Manual Trade Started");
  } else {
    return debug("Failed to Start App");
  }
}

// build/.lustre/entry.mjs
main2();
