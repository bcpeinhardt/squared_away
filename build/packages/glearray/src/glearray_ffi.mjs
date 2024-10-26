export function newArray() {
  return [];
}

export function fromList(list) {
  return list.toArray();
}

export function arrayLength(array) {
  return array.length;
}

export function get(array, index) {
  return array[index];
}

export function set(array, index, value) {
  const copy = [...array];
  copy[index] = value;
  return copy;
}

export function push(array, value) {
  const copy = [...array];
  copy.push(value);
  return copy;
}

export function insert(array, index, value) {
  return array.toSpliced(index, 0, value);
}
