export const stdlib = {
  length: "length",
  includes: "includes",
  array: {
    push: "array.push",
    pop: "array.pop",
    join: "array.join",
  },
  Object: {
    assign: "Object.assign",
    keys: "Object.keys",
    values: "Object.values",
    deleteProperties: "Object.deleteProperties",
  },
  strings: {
    hasPrefix: "strings.hasPrefix",
    hasSuffix: "strings.hasSuffix",
    toLower: "strings.toLower",
    toUpper: "strings.toUpper",
    trim: "strings.trim",
    trimLeft: "strings.trimLeft",
    trimRight: "strings.trimRight",
    split: "strings.split",
    findAll: "strings.findAll",
    contains: "strings.contains",
    contains_any: "strings.contains_any",
    index: "strings.index",
    indexAny: "strings.indexAny",
    lastIndex: "strings.lastIndex",
    lastIndexAny: "strings.lastIndexAny",
    repeat: "strings.repeat",
    match: "strings.match",
    substring: "strings.substring",
    toString: "strings.toString",
  },
  binary: {
    base64Encode: "binary.base64Encode",
    base64Decode: "binary.base64Decode",
    hex: "binary.hex",
  },
  hash: {
    crc32: "hash.crc32",
    md5: "hash.md5",
    sha1: "hash.sha1",
    sha256: "hash.sha256",
  },
  JSON: {
    parse: "JSON.parse",
    stringify: "JSON.stringify",
  },
  math: {
    floor: "math.floor",
    abs: "math.abs",
  },
  rand: {
    random: "rand.random",
    canary: "rand.canary",
  },
  time: {
    now: "time.now",
    format: "time.format",
  },
  url: {
    parseQuery: "url.parseQuery",
    buildQuery: "url.buildQuery",
    decodeComponent: "url.decodeComponent",
    encodeComponent: "url.encodeComponent",
  },
} as const;

export type Stdlib = typeof stdlib;
