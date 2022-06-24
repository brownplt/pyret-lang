import type * as PRIM_TYPES from './primitives';
const RUNTIME = require("./runtime.js");
const PRIMITIVES = require("./primitives.js") as typeof PRIM_TYPES;
const EQUALITY = require("./equality.js");

const VS = require("./valueskeleton.arr.js");
const OPTION = require("./option.arr.js");
const SETS = require("./sets.arr.js");
const LISTS = require("./lists.arr.js");
const RAW_ARRAY = require("./raw-array.arr.js");

const $PMutableStringDictBrand = "mutable-string-dict";
const $PBrandImmutable = "immutable-string-dict";

// used for removing values
var NOT_SET = {}

// TODO(MATT): is this bad?
var CHANGE_LENGTH = { value: false };
var DID_ALTER = { value: false };

var SHIFT = 5;
var SIZE = 1 << SHIFT;
var MASK = SIZE - 1;
var MAX_ARRAY_MAP_SIZE = SIZE / 4;
var MAX_BITMAP_INDEXED_SIZE = SIZE / 2;
var MIN_HASH_ARRAY_MAP_SIZE = SIZE / 4;
var STRING_HASH_CACHE_MIN_STRLEN = 16;
var STRING_HASH_CACHE_MAX_SIZE = 255;
var STRING_HASH_CACHE_SIZE = 0;
var stringHashCache = {};

function OwnerID() {}

// v8 has an optimization for storing 31-bit signed numbers.
// Values which have either 00 or 11 as the high order bits qualify.
// This function drops the highest order bit in a signed number, maintaining
// the sign bit.
function smi(i32) {
    return ((i32 >>> 1) & 0x40000000) | (i32 & 0xBFFFFFFF);
}

function hash(str) {
    return str.length > STRING_HASH_CACHE_MIN_STRLEN ? cachedHashString(str) : hashString(str);
}

function cachedHashString(string) {
    var hash = stringHashCache[string];
    if (hash === undefined) {
        hash = hashString(string);
        if (STRING_HASH_CACHE_SIZE === STRING_HASH_CACHE_MAX_SIZE) {
            STRING_HASH_CACHE_SIZE = 0;
            stringHashCache = {};
        }
        STRING_HASH_CACHE_SIZE++;
        stringHashCache[string] = hash;
    }
    return hash;
}

// http://jsperf.com/hashing-strings
function hashString(string) {
    // This is the hash from JVM
    // The hash code for a string is computed as
    // s[0] * 31 ^ (n - 1) + s[1] * 31 ^ (n - 2) + ... + s[n - 1],
    // where s[i] is the ith character of the string and n is the length of
    // the string. We "mod" the result to make it between 0 (inclusive) and 2^31
    // (exclusive) by dropping high bits.
    var hash = 0;
    for (var ii = 0; ii < string.length; ii++) {
        hash = 31 * hash + string.charCodeAt(ii) | 0;
    }
    return smi(hash);
}

function popCount(x) {
    x = x - ((x >> 1) & 0x55555555);
    x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
    x = (x + (x >> 4)) & 0x0f0f0f0f;
    x = x + (x >> 8);
    x = x + (x >> 16);
    return x & 0x7f;
}

function setIn(array, idx, val, canEdit) {
    var newArray = canEdit ? array : arrCopy(array, 0);
    newArray[idx] = val;
    return newArray;
}

function spliceIn(array, idx, val, canEdit) {
    var newLen = array.length + 1;
    if (canEdit && idx + 1 === newLen) {
        array[idx] = val;
        return array;
    }
    var newArray = new Array(newLen);
    var after = 0;
    for (var ii = 0; ii < newLen; ii++) {
        if (ii === idx) {
            newArray[ii] = val;
            after = -1;
        } else {
            newArray[ii] = array[ii + after];
        }
    }
    return newArray;
}

function spliceOut(array, idx, canEdit) {
    var newLen = array.length - 1;
    if (canEdit && idx === newLen) {
        array.pop();
        return array;
    }
    var newArray = new Array(newLen);
    var after = 0;
    for (var ii = 0; ii < newLen; ii++) {
        if (ii === idx) {
            after = 1;
        }
        newArray[ii] = array[ii + after];
    }
    return newArray;
}

function arrCopy(arr, offset) {
    offset = offset || 0;
    var len = Math.max(0, arr.length - offset);
    var newArr = new Array(len);
    for (var ii = 0; ii < len; ii++) {
        newArr[ii] = arr[ii + offset];
    }
    return newArr;
}

function MakeRef(ref) {
    ref.value = false;
    return ref;
}

function SetRef(ref) {
    ref && (ref.value = true);
}

function emptyMap() {
    // NOTE: originally last two arugments were omitted
    //   Set to undefined?
    return new ImmutableMap(0, undefined, undefined);
}

function ImmutableMap(size, root, ownerID) {
    this.size = size;
    this._root = root;
    this.__ownerID = ownerID;

    this.get = function(k, notSetValue) {
        return this._root ?
            this._root.get(0, undefined, k, notSetValue) :
            notSetValue;
    };

    this.set = function(k, v) {
        return updateMap(this, k, v);
    };

    this.remove = function(k) {
        return updateMap(this, k, NOT_SET);
    };

    this.keys = function() {
        if (!this._root) {
            return [];
        } else {
            return this._root.keys([]);
        }
    };
}

function updateMap(map, k, v) {
    var newRoot;
    var newSize;
    if (!map._root) {
        if (v == NOT_SET) {
            return map;
        }
        newSize = 1;
        newRoot = new ArrayMapNode(map.__ownerID, [[k, v]]);
    } else {
        var didChangeSize = MakeRef(CHANGE_LENGTH);
        var didAlter = MakeRef(DID_ALTER);
        newRoot = updateNode(map._root, map.__ownerID, 0, undefined, k, v, didChangeSize, didAlter);
        if (!didAlter.value) {
            return map;
        }
        newSize = map.size + (didChangeSize.value ? v === NOT_SET ? -1 : 1 : 0);
    }
    if (map.__ownerID) {
        map.size = newSize;
        map._root = newRoot;
        return map;
    }
    return newRoot ? new ImmutableMap(newSize, newRoot, undefined) : new ImmutableMap(0, undefined, undefined);
}

function updateNode(node, ownerID, shift, keyHash, key, value, didChangeSize, didAlter) {
    if (!node) {
        if (value === NOT_SET) {
            return node;
        }
        SetRef(didAlter);
        SetRef(didChangeSize);
        return new ValueNode(ownerID, keyHash, [key, value]);
    }
    return node.update(ownerID, shift, keyHash, key, value, didChangeSize, didAlter);
}

function createNodes(ownerID, entries, key, value) {
    if (!ownerID) {
        ownerID = new OwnerID();
    }
    var node = new ValueNode(ownerID, hash(key), [key, value]);
    for (var ii = 0; ii < entries.length; ii++) {
        var entry = entries[ii];
        node = node.update(ownerID, 0, undefined, entry[0], entry[1]);
    }
    return node;
}

function packNodes(ownerID, nodes, count, excluding) {
    var bitmap = 0;
    var packedII =0;
    var packedNodes = new Array(count);
    for (var ii = 0, bit = 1, len = nodes.length; ii < len; ii++, bit <<= 1) {
        var node = nodes[ii];
        if (node !== undefined && ii !== excluding) {
            bitmap |= bit;
            packedNodes[packedII++] = node;
        }
    }
    return new BitmapIndexedNode(ownerID, bitmap, packedNodes);
}

function expandNodes(ownerID, nodes, bitmap, including, node) {
    var count = 0;
    var expandedNodes = new Array(SIZE);
    for (var ii = 0; bitmap !== 0; ii++, bitmap >>>= 1) {
        expandedNodes[ii] = bitmap & 1 ? nodes[count++] : undefined;
    }
    expandedNodes[including] = node;
    return new HashArrayMapNode(ownerID, count + 1, expandedNodes);
}

function mergeIntoNode(node, ownerID, shift, keyHash, entry) {
    if (node.keyHash === keyHash) {
        return new HashCollisionNode(ownerID, keyHash, [node.entry, entry]);
    }

    var idx1 = (shift === 0 ? node.keyHash : node.keyHash >>> shift) & MASK;
    var idx2 = (shift === 0 ? keyHash : keyHash >>> shift) & MASK;

    var newNode;
    var nodes = (idx1 === idx2) ?
        [mergeIntoNode(node, ownerID, shift + SHIFT, keyHash, entry)] :
        ((newNode = new ValueNode(ownerID, keyHash, entry)), idx1 < idx2 ? [node, newNode] : [newNode, node]);

    return new BitmapIndexedNode(ownerID, (1 << idx1) | (1 << idx2), nodes);
}

function isLeafNode(node) {
    return node.constructor === ValueNode || node.constructor === HashCollisionNode;
}

function ArrayMapNode(ownerID, entries) {
    this.ownerID = ownerID;
    this.entries = entries;

    this.get = function(shift, keyHash, key, notSetValue) {
        var entries = this.entries;
        for (var ii = 0, len = entries.length; ii < len; ii++) {
            if (key === entries[ii][0]) {
                return entries[ii][1];
            }
        }
        return notSetValue;
    };

    this.keys = function(ret) {
        for (var i = 0; i < this.entries.length; i++)
            ret.push(this.entries[i][0]);
        return ret;
    };

    this.update = function(ownerID, shift, keyHash, key, value, didChangeSize, didAlter) {
        var removed = (value === NOT_SET);

        var entries = this.entries;
        var idx = 0;
        for (var len = entries.length; idx < len; idx++) {
            if (key === entries[idx][0]) {
                break;
            }
        }
        var exists = idx < len;

        if (exists ? entries[idx][1] === value : removed) {
            return this;
        }

        SetRef(didAlter);
        (removed || !exists) && SetRef(didChangeSize);

        if (removed && entries.length === 1) {
            return undefined;
        }

        if (!exists && !removed && entries.length >= MAX_ARRAY_MAP_SIZE) {
            return createNodes(ownerID, entries, key, value);
        }

        var isEditable = ownerID && ownerID === this.ownerID;
        var newEntries = isEditable ? entries : arrCopy(entries, 0);

        if (exists) {
            if (removed) {
                idx === len - 1 ? newEntries.pop() : (newEntries[idx] = newEntries.pop());
            } else {
                newEntries[idx] = [key, value];
            }
        } else {
            newEntries.push([key, value]);
        }

        if (isEditable) {
            this.entries = newEntries;
            return this;
        }

        return new ArrayMapNode(ownerID, newEntries);
    };
}

function ValueNode(ownerID, keyHash, entry) {
    this.ownerID = ownerID;
    this.keyHash = keyHash;
    this.entry = entry;

    this.get = function(shift, keyHash, key, notSetValue) {
        return key === this.entry[0] ? this.entry[1] : notSetValue;
    };

    this.keys = function(ret) {
        ret.push(this.entry[0]);
        return ret;
    };

    this.update = function(ownerID, shift, keyHash, key, value, didChangeSize, didAlter) {
        var removed = (value === NOT_SET);
        var keyMatch = (key === this.entry[0]);
        if (keyMatch ? value === this.entry[1] : removed) {
            return this;
        }

        SetRef(didAlter);

        if (removed) {
            SetRef(didChangeSize);
            return undefined;
        }

        if (keyMatch) {
            if (ownerID && ownerID === this.ownerID) {
                this.entry[1] = value;
                return this;
            }
            return new ValueNode(ownerID, this.keyHash, [key, value]);
        }

        SetRef(didChangeSize);
        return mergeIntoNode(this, ownerID, shift, hash(key), [key, value]);
    };
}

function HashCollisionNode(ownerID, keyHash, entries) {
    this.ownerID = ownerID;
    this.keyHash = keyHash;
    this.entries = entries;

    this.get = function(shift, keyHash, key, notSetValue) {
        var entries = this.entries;
        for (var ii = 0, len = entries.length; ii < len; ii++) {
            if (key === entries[ii][0]) {
                return entries[ii][1];
            }
        }
        return notSetValue;
    };

    this.keys = function(ret) {
        for (var i = 0; i < this.entries.length; i++)
            ret.push(this.entries[i][0]);
        return ret;
    };

    this.update = function(ownerID, shift, keyHash, key, value, didChangeSize, didAlter) {
        if (keyHash === undefined) {
            keyHash = hash(key);
        }

        var removed = (value === NOT_SET);

        if (keyHash !== this.keyHash) {
            if (removed) {
                return this;
            }
            SetRef(didAlter);
            SetRef(didChangeSize);
            return mergeIntoNode(this, ownerID, shift, keyHash, [key, value]);
        }

        var entries = this.entries;
        var idx = 0;
        for (var len = entries.length; idx < len; idx++) {
            if (key === entries[idx][0]) {
                break;
            }
        }
        var exists = idx < len;

        if (exists ? entries[idx][1] === value : removed) {
            return this;
        }

        SetRef(didAlter);
        (removed || !exists) && SetRef(didChangeSize);

        if (removed && len === 2) {
            return new ValueNode(ownerID, this.keyHAsh, entries[idx ^ 1]);
        }

        var isEditable = ownerID && ownerID === this.ownerID;
        var newEntries = isEditable ? entries : arrCopy(entries, 0);

        if (exists) {
            if (removed) {
                idx === len - 1 ? newEntries.pop() : (newEntries[idx] = newEntries.pop());
            } else {
                newEntries[idx] = [key, value];
            }
        } else {
            newEntries.push([key, value]);
        }

        if (isEditable) {
            this.entries = newEntries;
            return this;
        }

        return new HashCollisionNode(ownerID, this.keyHash, newEntries);
    };
}

function BitmapIndexedNode(ownerID, bitmap, nodes) {
    this.ownerID = ownerID;
    this.bitmap = bitmap;
    this.nodes = nodes;

    this.get = function(shift, keyHash, key, notSetValue) {
        if (keyHash === undefined) {
            keyHash = hash(key);
        }
        var bit = (1 << ((shift === 0 ? keyHash : keyHash >>> shift) & MASK));
        var bitmap = this.bitmap;
        return (bitmap & bit) === 0 ? notSetValue :
            this.nodes[popCount(bitmap & (bit -1))].get(shift + SHIFT, keyHash, key, notSetValue);
    };

    this.keys = function(ret) {
        var nodes = this.nodes;
        for (var ii = 0, maxIndex = nodes.length - 1; ii <= maxIndex; ii++) {
            var node = nodes[ii];
            if (node) {
                node.keys(ret);
            }
        }
        return ret;
    };

    this.update = function(ownerID, shift, keyHash, key, value, didChangeSize, didAlter) {
        if (keyHash === undefined) {
            keyHash = hash(key);
        }
        var keyHashFrag = (shift === 0 ? keyHash : keyHash >>> shift) & MASK;
        var bit = 1 << keyHashFrag;
        var bitmap = this.bitmap;
        var exists = (bitmap & bit) !== 0;

        if (!exists && value === NOT_SET) {
            return this;
        }

        var idx = popCount(bitmap & (bit - 1));
        var nodes = this.nodes;
        var node = exists ? nodes[idx] : undefined;
        var newNode = updateNode(node, ownerID, shift + SHIFT, keyHash, key, value, didChangeSize, didAlter);

        if (newNode === node) {
            return this;
        }

        if (!exists && newNode && nodes.length >= MAX_BITMAP_INDEXED_SIZE) {
            return expandNodes(ownerID, nodes, bitmap, keyHashFrag, newNode);
        }

        if (exists && !newNode && nodes.length === 2 && isLeafNode(nodes[idx ^ 1])) {
            return nodes[idx ^ 1];
        }

        if (exists && newNode && nodes.length === 1 && isLeafNode(newNode)) {
            return newNode;
        }

        var isEditable = ownerID && ownerID === this.ownerID;
        var newBitmap = exists ? newNode ? bitmap : bitmap ^ bit : bitmap | bit;
        var newNodes = exists ? newNode ?
            setIn(nodes, idx, newNode, isEditable) :
            spliceOut(nodes, idx, isEditable) :
            spliceIn(nodes, idx, newNode, isEditable);

        if (isEditable) {
            this.bitmap = newBitmap;
            this.nodes = newNodes;
            return this;
        }

        return new BitmapIndexedNode(ownerID, newBitmap, newNodes);
    };
}

function HashArrayMapNode(ownerID, count, nodes) {
    this.ownerID = ownerID;
    this.count = count;
    this.nodes = nodes;

    this.get = function(shift, keyHash, key, notSetValue) {
        if (keyHash === undefined) {
            keyHash = hash(key);
        }
        var idx = (shift === 0 ? keyHash : keyHash >>> shift) & MASK;
        var node = this.nodes[idx];
        return node ? node.get(shift + SHIFT, keyHash, key, notSetValue) : notSetValue;
    };
    this.keys = function(ret) {
        var nodes = this.nodes;
        for (var ii = 0, maxIndex = nodes.length - 1; ii <= maxIndex; ii++) {
            var node = nodes[ii];
            if (node) {
                node.keys(ret);
            }
        }
        return ret;
    };

    this.update = function(ownerID, shift, keyHash, key, value, didChangeSize, didAlter) {
        if (keyHash === undefined) {
            keyHash = hash(key);
        }
        var idx = (shift === 0 ? keyHash : keyHash >>> shift) & MASK;
        var removed = (value === NOT_SET);
        var nodes = this.nodes;
        var node = nodes[idx];

        if (removed && !node) {
            return this;
        }

        var newNode = updateNode(node, ownerID, shift + SHIFT, keyHash, key, value, didChangeSize, didAlter);
        if (newNode === node) {
            return this;
        }

        var newCount = this.count;
        if (!node) {
            newCount++;
        } else if (!newNode) {
            newCount--;
            if (newCount < MIN_HASH_ARRAY_MAP_SIZE) {
                return packNodes(ownerID, nodes, newCount, idx);
            }
        }

        var isEditable = ownerID && ownerID === this.ownerID;
        var newNodes = setIn(nodes, idx, newNode, isEditable);

        if (isEditable) {
            this.count = newCount;
            this.nodes = newNodes;
            return this;
        }

        return new HashArrayMapNode(ownerID, newCount, newNodes);
    };
}

function eqHelp(pyretSelf, other, check, get, selfKeys, recEq) {
    // NOTES
    //   * The original implementation was heavily integrated in the old Pyret runtime
    //   * pyretSelf and other are dictionaries of the same length
    for (let i = 0; i < selfKeys.length; i++) {
        const currKey = selfKeys[i];
        if (other[check](currKey)) {
            const recResult = recEq(pyretSelf[get](currKey), other[get](currKey));
            if (!EQUALITY.isEqual(recResult)) {
                return recResult;
            }
        } else {
            return EQUALITY.NotEqual("Missing key", pyretSelf, other);
        }
    }

    return EQUALITY.Equal;
}


//////////////////////////////////////////////////
const getISDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf, key) {
    const missing_value = {};
    const val = pyretSelf.$underlyingMap.get(key, missing_value);
    if (val === missing_value) {
        return OPTION.none;
    } else {
        return OPTION.some(val);
    }
});

const getValueISDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf, key) {
    const missing_value = {};
    const val = pyretSelf.$underlyingMap.get(key, missing_value);
    if (val === missing_value) {
        throw new Error('Key ' + key + ' not found');
    }
    return val;
});

const setISDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf, key, val) {
    const newMap = pyretSelf.$underlyingMap.set(key, val);
    return makeImmutableStringDict(newMap);
});

const mergeISDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf, otherDict) {
    // keys-list returns a PyretList
    const otherKeys = otherDict["keys-list"]();
    const otherKeysArr = LISTS["to-raw-array"](otherKeys);

    if (otherKeysArr.length === 0) { return pyretSelf; }

    let newMap = pyretSelf.$underlyingMap;
    for (let i = 0; i < otherKeysArr.length; i++) {
        let currKey = otherKeysArr[i];
        newMap = newMap.set(currKey, otherDict["get-value"](currKey));
    }
    return makeImmutableStringDict(newMap);
});

const removeISDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf, key) {
    const newMap = pyretSelf.$underlyingMap.remove(key);
    return makeImmutableStringDict(newMap);
});

const hasKeyISDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf, key) {
    const missing_value = {};
    const val = pyretSelf.$underlyingMap.get(key, missing_value);
    if (val === missing_value) {
        return false;
    } else {
        return true;
    }
});

const eachKeyISDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf, f) {
    RAW_ARRAY["raw-array-for-each"]["raw-array-for-each"](f, pyretSelf.$underlinyMaps.keys());
    return RUNTIME.Nothing;
});

const mapKeysISDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf, f) {
    const mapResult = RAW_ARRAY["raw-array-map"](f, pyretSelf.$underlyingMap.keys());
    return LISTS["raw-array-to-list"](mapResult);
});

const foldKeysISDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf, f, init) {
    // NOTE(alex): Double check type signatures for higher order functions
    //      acc comes first for raw arrays
    //      acc comes second for Pyret
    return RAW_ARRAY["raw-array-fold"](function(acc, key) {
        return f(key, acc);
    }, init, pyretSelf.$underlyingMap.keys());
});

const keysISDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf) {
    const keys = pyretSelf.$underlyingMap.keys();
    return SETS["tree-set"].make(keys);
    //return SETS["tree-set"].make(keys.map(function(key) {
    //    TODO(alex): Do we need a JS String conversion?
    //    return key;
    //    // return RUNTIME.makeString(key);
    //}));
});

const keysListISDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf) {
    const keys = pyretSelf.$underlyingMap.keys();
    return LISTS.list.make(keys);
    //return runtime.ffi.makeList(keys.map(function(key) {
    //  return runtime.makeString(key);
    //}));
});

const countISDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf) {
    const count = pyretSelf.$underlyingMap.size;
    // NOTE(alex): The $makeRational() call is not strictly necessary
    //   b/c JS Numbers are a subset of Pyret numbers.
    //   Added for (potentially useless) future-proofing
    return RUNTIME.$makeRational(count);
});

// TODO(alex): valueskeleton
var outputISD = PRIMITIVES.makeMethodBinder(function(self, toOutput) {
  var elts : any[] = [];
  var keys = self.$underlyingMap.keys();
  for (var i = 0; i < keys.length; i++) {
    elts.push(toOutput(RUNTIME.PTuple([keys[i], self.$underlyingMap.get(keys[i])])));
  }
  return VS["vs-collection"]("string-dict", elts);
});

const equalsISDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf, other, recursiveEquality) {
    if (!PRIMITIVES.hasBrand($PBrandImmutable, other)) {
        return EQUALITY.NotEqual("Different brands", pyretSelf, other);
    } else {
        const keys = pyretSelf.$underlyingMap.keys();
        const otherKeysLength = other.count();
        if (keys.length !== otherKeysLength) {
            return EQUALITY.NotEqual("Different key lengths", pyretSelf, other);
        } else {
            return eqHelp(pyretSelf, other, "has-key", "get-value", keys, recursiveEquality);
        }
    }
});

const unfreezeISDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf) {
    const dict = Object.create(null);
    const keys = pyretSelf.$underlyingMap.keys();
    for (var ii = 0; ii < keys.length; ii++) {
        const key = keys[ii];
        const val = pyretSelf.$underlyingMap.get(key);
        dict[key] = val;
    }
    return makeMutableStringDict(dict, undefined);
});

function makeImmutableStringDict(underlyingMap) {
    let obj = {};

    obj['get'] = getISDBinder(obj);
    obj['get-value'] = getValueISDBinder(obj);
    obj['set'] = setISDBinder(obj);
    obj['merge'] = mergeISDBinder(obj);
    obj['remove'] = removeISDBinder(obj);
    obj['keys'] = keysISDBinder(obj);
    obj["keys-list"] = keysListISDBinder(obj);
    obj['map-keys'] = mapKeysISDBinder(obj);
    obj['fold-keys'] = foldKeysISDBinder(obj);
    obj['each-key'] = eachKeyISDBinder(obj);
    obj['count'] = countISDBinder(obj);
    obj['has-key'] = hasKeyISDBinder(obj);
    obj['_equals'] = equalsISDBinder(obj);
    obj['_output'] = outputISD(obj),
    obj['unfreeze'] = unfreezeISDBinder(obj);
    obj = PRIMITIVES.applyBrand($PBrandImmutable, obj);
    obj['$underlyingMap'] = underlyingMap;
    return obj;
}


//////////////////////////////////////////////////
const getMSDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf, key) {
    const val = pyretSelf.$underlyingDict[key];
    if (val === undefined) {
        return OPTION.none;
    } else {
        return OPTION.some(val);
    }
});

const getValueMSDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf, key) {
    const val = pyretSelf.$underlyingDict[key];
    if (val === undefined) {
        throw new Error("Key " + key + " not found");
    }
    return val;
});

const setMSDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf, key, val) {
    if (pyretSelf.$sealed) {
        throw new Error("Cannot modify sealed string dict");
    }
    pyretSelf.$underlyingDict[key] = val;
    return RUNTIME.$nothing;
});

const mergeMSDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf, other) {
    for (var key in other.$underlyingDict) {
        pyretSelf.$underlyingDict[key] = other.$underlyingDict[key];
    }
    return RUNTIME.$nothing;
});

const cloneMSDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf) {
    const newDict = Object.create(null);
    for (var key in pyretSelf.$underlyingDict) {
        newDict[key] = pyretSelf.$underlyingDict[key];
    }
    return makeMutableStringDict(newDict, undefined);
});

const removeMSDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf, key) {
    if (pyretSelf.$sealed) {
        throw new Error("Cannot modify sealed string dict");
    }
    delete pyretSelf.$underlyingDict[key];
    return RUNTIME.$nothing;
});

const hasKeyMSDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf, key) {
    if (key in pyretSelf.$underlyingDict) {
        return true;
    } else {
        return false;
    }
});

const keysMSDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf) {
    const keys = Object.keys(pyretSelf.$underlyingDict);
    return SETS["tree-set"].make(keys);
});

const keysListMSDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf) {
    const keys = Object.keys(pyretSelf.$underlyingDict);
    return LISTS.list.make(keys);
});

const eachKeyMSDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf, f) {
    RAW_ARRAY["raw-array-for-each"](f, Object.keys(pyretSelf.$underlyingDict));
    return RUNTIME.$nothing;
});

const mapKeysMSDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf, f) {
    const mapResult = RAW_ARRAY["raw-array-map"](f, Object.keys(pyretSelf.$underlyingDict));
    return LISTS.list.make(mapResult);
});

const foldKeysMSDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf, f, init) {
    return RAW_ARRAY["raw-array-fold"](function(acc, key) {
        return f(key, acc);
    }, init, Object.keys(pyretSelf.$underlyingDict));
});

const countMSDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf) {
    return RUNTIME.$makeRational(Object.keys(pyretSelf.$underlyingDict).length);
});

const toreprMSDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf, recursiveToRepr) {
    const keys = Object.keys(pyretSelf.$underlyingDict);
    const elts : any[] = [];
    function combine(elts) {
        //return "[string-dict: " + elts.join(", ") + "]";
        return "[mutable-string-dict: " + elts.join(", ") + "]";
    }
    function toreprElts() {
        if (keys.length === 0) { return combine(elts); }
        else {
            const thisKey = keys.pop();
            if (!thisKey) { throw new Error("Internal error: undefined key in dictionary"); }
            // The function recursiveToRepr is a callback for rendering
            // sub-elements of collections.  If we call it on anything other
            // than flat primitives, we need to use the following safeCall
            // calling convention, which makes this work with the stack
            // compilation strategy for Pyret.
            const result = recursiveToRepr(pyretSelf.$underlyingDict[thisKey]);
            elts.push(recursiveToRepr(thisKey));
            elts.push(result);
            return toreprElts();
        }
    }
    return toreprElts();
});

var outputMSD = PRIMITIVES.makeMethodBinder(function(self, toOutput) {
  var elts : any[] = [];
  var keys = Object.keys(self.$underlyingDict);
  for (var i = 0; i < keys.length; i++) {
    elts.push(toOutput(RUNTIME.PTuple([keys[i], self.$underlyingDict[keys[i]]])));
  }
  return VS["vs-collection"]("mutable-string-dict", elts);
});

const equalsMSDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf, other, recursiveEquality) {
    if (!PRIMITIVES.hasBrand($PMutableStringDictBrand, other)) {
        return EQUALITY.NotEqual("Not a mutable dictionary", pyretSelf, other);
    } else {
        const selfKeys = Object.keys(pyretSelf.$underlyingDict);
        const otherKeys = Object.keys(other.$underlyingDict);
        if (selfKeys.length !== otherKeys.length) {
            return EQUALITY.NotEqual("Different key lengths", pyretSelf, other);
        } else {
            return eqHelp(pyretSelf, other, "has-key-now", "get-value-now", selfKeys, recursiveEquality);
        }
    }
});

const freezeMSDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf) {
    var map = emptyMap();
    for (let key in pyretSelf.$underlyingDict) {
        map = map.set(key, pyretSelf.$underlyingDict[key]);
    }
    return makeImmutableStringDict(map);
});

const sealMSDBinder = PRIMITIVES.makeMethodBinder(function(pyretSelf) {
    return makeMutableStringDict(pyretSelf.$underlyingDict, true);
});

function makeMutableStringDict(underlyingDict, sealed) {
    let obj = {};
    obj['get-now'] = getMSDBinder(obj);
    obj['get-value-now'] = getValueMSDBinder(obj);
    obj['set-now'] = setMSDBinder(obj);
    obj['merge-now'] = mergeMSDBinder(obj);
    obj['remove-now'] = removeMSDBinder(obj);
    obj['keys-now'] = keysMSDBinder(obj);
    obj['keys-list-now'] = keysListMSDBinder(obj);
    obj['map-keys-now'] = mapKeysMSDBinder(obj);
    obj['fold-keys-now'] = foldKeysMSDBinder(obj);
    obj['each-key-now'] = eachKeyMSDBinder(obj);
    obj['count-now'] = countMSDBinder(obj);
    obj['has-key-now'] = hasKeyMSDBinder(obj);
    obj['clone-now'] = cloneMSDBinder(obj);
    obj['_equals'] = equalsMSDBinder(obj);
    obj['_output'] = outputMSD(obj);
    obj['freeze'] = freezeMSDBinder(obj);
    obj['seal'] = sealMSDBinder(obj);
    // Applying a brand creates a new object, so we need to add the reflective field afterward
    obj = PRIMITIVES.applyBrand($PMutableStringDictBrand, obj);
    obj['$underlyingDict'] = underlyingDict;
    obj['$sealed'] = sealed;

    return obj;
}



function internal_isMSD(obj) {
    return PRIMITIVES.hasBrand($PMutableStringDictBrand, obj);
}

function isMutableStringDict(obj) {
    return internal_isMSD(obj);
}

function createMutableStringDict() {
    var dict = Object.create(null);
    return makeMutableStringDict(dict, undefined);
}

function createMutableStringDictFromArrayAlternating(array) {
    const dict = Object.create(null);
    const len = array.length;
    if(len % 2 !== 0) {
        throw new Error("Expected an even number of arguments to constructor for mutable dictionaries, got array of length " + len);
    }
    for(let i = 0; i < len; i += 2) {
        const key = array[i];
        const val = array[i + 1];
        dict[key] = val;
    }
    return makeMutableStringDict(dict, undefined);
}

function createMutableStringDictFromArray(array) {
  var dict = Object.create(null);
  var len = array.length;
  for(var i = 0; i < len; i += 1) {
    var key = array[i][0];
    var val = array[i][1];
    dict[key] = val;
  }
  return makeMutableStringDict(dict, undefined);
}

function internal_isISD(obj) {
    return PRIMITIVES.hasBrand($PBrandImmutable, obj);
}

function isImmutableStringDict(obj) {
    return internal_isISD(obj);
}

function createImmutableStringDict() {
    var map = emptyMap();
    return makeImmutableStringDict(map);
}

function createImmutableStringDictFromArrayAlternating(array) {
    const key_missing = {};
    let map = emptyMap();
    const len = array.length;
    if(len % 2 !== 0) {
        throw new Error("Expected an even number of arguments to constructor for immutable dictionaries, got array of length " + len);
    }
    for(var i = 0; i < len; i += 2) {
        let key = array[i];
        let val = array[i + 1];
        if (map.get(key, key_missing) !== key_missing) {
            throw new Error("Creating immutable string dict with duplicate key " + key);
        }
        map = map.set(key, val);
    }
    return makeImmutableStringDict(map);
}


function createImmutableStringDictFromArray(array) {
  var key_missing = {};
  var map = emptyMap();
  var len = array.length;
  for(var i = 0; i < len; i += 1) {
    var key = array[i][0];
    var val = array[i][1];
    if (map.get(key, key_missing) !== key_missing) {
      throw new Error("Creating immutable string dict with duplicate key " + key);
    }
    map = map.set(key, val);
  }
  return makeImmutableStringDict(map);
}



function createConstImmutableStringDict(names, val) {
    const arr = LISTS["to-raw-array"](names);
    let map = emptyMap();
    arr.forEach(function(k) {
        map = map.set(k, val)
    });
    return makeImmutableStringDict(map);
}

function mapKeys(f, isd) {
    return isd["map-keys"](f);
}

function mapKeysNow(f, msd) {
    return msd["map-keys-now"](f);
}

function foldKeys(f, init, isd) {
    // NOTE: f's type matches raw-array-fold's expected
    return RAW_ARRAY["raw-array-fold"](f, init, isd.$underlyingMap.keys());
}

function foldKeysNow(f, init, msd) {
    // NOTE: f's type matches raw-array-fold's expected
    return RAW_ARRAY["raw-array-fold"](f, init, Object.keys(msd.$underlyingDict));
}

function eachKey(f, isd) {
    return isd["each-key"](f);
}

function eachKeyNow(f, msd) {
    return msd["each-key-now"](f);
}

function createMutableStringDict0() {
    const dict = Object.create(null);
    return makeMutableStringDict(dict, undefined);
}

function createMutableStringDict1(arg) {
    throw new Error("Expected an even number of arguments to constructor for mutable dictionaries, got " + arguments.length);
}

function createMutableStringDict2(a, b) {
    const dict = Object.create(null);
    dict[a] = b;
    return makeMutableStringDict(dict, undefined);
}

function createMutableStringDict3(a, b, c) {
    throw new Error("Expected an even number of arguments to constructor for mutable dictionaries, got " + arguments.length);
}

function createMutableStringDict4(a, b, c, d) {
    const dict = Object.create(null);
    dict[a] = b;
    dict[c] = d;
    return makeMutableStringDict(dict, undefined);
}

function createMutableStringDict5(a, b, c, d, e) {
    throw new Error("Expected an even number of arguments to constructor for mutable dictionaries, got " + arguments.length);
}

function createImmutableStringDict0() {
    const map = emptyMap();
    return makeImmutableStringDict(map);
}

function createImmutableStringDict1(arg) {
    throw new Error("Expected an even number of arguments to constructor for immutable dictionaries, got " + arguments.length);
}

function createImmutableStringDict2(a, b) {
    let map = emptyMap();
    map = map.set(a, b);
    return makeImmutableStringDict(map);
}

function createImmutableStringDict3(a, b, c) {
    throw new Error("Expected an even number of arguments to constructor for immutable dictionaries, got " + arguments.length);
}

function createImmutableStringDict4(a, b, c, d) {
    let map = emptyMap();
    if (a === c) {
        throw new Error("Creating immutable string dict with duplicate key " + a)
    }
    map = map.set(a, b);
    map = map.set(c, d);
    return makeImmutableStringDict(map);
}

function createImmutableStringDict5(a, b, c, d, e) {
    throw new Error("Expected an even number of arguments to constructor for immutable dictionaries, got " + arguments.length);
}

module.exports = {
    "make-mutable-string-dict": createMutableStringDict,
    "mutable-string-dict": {
        make: createMutableStringDictFromArray,
        make0: createMutableStringDict0,
        make1: createMutableStringDict1,
        make2: createMutableStringDict2,
        make3: createMutableStringDict3,
        make4: createMutableStringDict4,
        make5: createMutableStringDict5
    },
    "alternating-mutable-string-dict": {
        make: createMutableStringDictFromArrayAlternating,
        make0: () => createMutableStringDictFromArrayAlternating([]),
        make1: (a) => { throw new Error("1 argument to alternating string-dict constructor"); },
        make2: (a, b) => createMutableStringDictFromArrayAlternating([PRIMITIVES.PTuple([a, b])]),
        make3: (a, b, c) => { throw new Error("3 arguments to alternating string-dict constructor"); },
        make4: (a, b, c, d) => createMutableStringDictFromArrayAlternating([
                PRIMITIVES.PTuple([a, b]),
                PRIMITIVES.PTuple([c, d])]),
        make5: (a, b, c, d, e) => { throw new Error("5 arguments to alternating string-dict constructor"); },
    },
    "is-mutable-string-dict": isMutableStringDict,
    "make-string-dict": createImmutableStringDict,
    "map-keys": mapKeys,
    "map-keys-now": mapKeysNow,
    "fold-keys": foldKeys,
    "fold-keys-now": foldKeysNow,
    "each-key": eachKey,
    "each-key-now": eachKeyNow,
    "string-dict": {
        make: createImmutableStringDictFromArray,
        make0: createImmutableStringDict0,
        make1: createImmutableStringDict1,
        make2: createImmutableStringDict2,
        make3: createImmutableStringDict3,
        make4: createImmutableStringDict4,
        make5: createImmutableStringDict5
    },
    "alternating-string-dict": {
        make: createImmutableStringDictFromArrayAlternating,
        make0: () => createImmutableStringDictFromArrayAlternating([]),
        make1: (a) => { throw new Error("1 argument to alternating string-dict constructor"); },
        make2: (a, b) => createImmutableStringDictFromArrayAlternating([PRIMITIVES.PTuple([a, b])]),
        make3: (a, b, c) => { throw new Error("3 arguments to alternating string-dict constructor"); },
        make4: (a, b, c, d) => createImmutableStringDictFromArrayAlternating([
                PRIMITIVES.PTuple([a, b]),
                PRIMITIVES.PTuple([c, d])]),
        make5: (a, b, c, d, e) => { throw new Error("5 arguments to alternating string-dict constructor"); },
    },
    "string-dict-of": createConstImmutableStringDict,
    "is-string-dict": isImmutableStringDict
};
