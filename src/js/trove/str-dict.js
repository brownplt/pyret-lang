({
  requires:
    [
      { "import-type": "builtin", name: "valueskeleton" }
    ],
  nativeRequires: ["pyret-base/js/namespace"],
  provides: {
    shorthands: {
      "sdOfA": ["tyapp", ["local", "StringDict"], [["tid", "a"]]],
      "msdOfA": ["tyapp", ["local", "MutableStringDict"], [["tid", "a"]]],

      "Equality": { tag: "name",
                    origin: { "import-type": "uri", uri: "builtin://equality" },
                    name: "EqualityResult" },
      "VS": { tag: "name",
                    origin: { "import-type": "uri", uri: "builtin://valueskeleton" },
                    name: "ValueSkeleton" },
      "SetOfA": ["tyapp", { tag: "name",
               origin: { "import-type": "uri", uri: "builtin://sets" },
               name: "Set" }, [["tid", "a"]]],
      "SetOfString": ["tyapp", { tag: "name",
               origin: { "import-type": "uri", uri: "builtin://sets" },
               name: "Set" }, ["String"]]
    },
    values: {
      "make-string-dict": ["forall", ["a"], ["arrow", [], "sdOfA"]],
      "make-mutable-string-dict": ["forall", ["a"], ["arrow", [], "msdOfA"]],
      "string-dict": ["forall", ["a"], ["Maker", ["tuple", ["String", ["tid", "a"]]], "sdOfA"]],
      "mutable-string-dict": ["forall", ["a"], ["Maker", ["tuple", ["String", ["tid", "a"]]], "msdOfA"]],
      "is-mutable-string-dict": ["arrow", ["Any"], "Boolean"],
      "is-string-dict": ["arrow", ["Any"], "Boolean"],
      "string-dict-of": ["forall", "a", ["arrow", [["List", "String"], ["tid", "a"]], "sdOfA"]]
    },
    aliases: {

    },
    datatypes: {
      "StringDict": ["data", "StringDict", ["a"], [], {
        "get": ["arrow", ["String"], ["Option", ["tid", "a"]]],
        "get-value": ["arrow", ["String"], ["tid", "a"]],
        "set": ["arrow", ["String", ["tid", "a"]], "sdOfA"],
        "merge": ["arrow", ["sdOfA"], "sdOfA"],
        "remove": ["arrow", ["String"], "sdOfA"],
        "keys": ["arrow", [], "SetOfString"],
        "keys-list": ["arrow", [], ["List", "String"]],
        "items": ["arrow", [], ["List", ["tuple", ["String", ["tid", "a"]]]]],
        "count": ["arrow", [], "Number"],
        "has-key": ["arrow", ["String"], "Boolean"],
        "_equals": ["arrow", ["sdOfA", ["arrow", ["Any", "Any"], "Equality"]], "Equality"],
        "_output":  ["arrow", [["arrow", ["Any"], "VS"]], "VS"],
        "unfreeze": ["arrow", [], "msdOfA"]
      }],
      "MutableStringDict": ["data", "MutableStringDict", ["a"], [], {
        "get-now": ["arrow", ["String"], ["Option", ["tid", "a"]]],
        "get-value-now": ["arrow", ["String"], ["tid", "a"]],
        "set-now": ["arrow", ["String", ["tid", "a"]], "Nothing"],
        "merge-now": ["arrow", ["msdOfA"], "Nothing"],
        "remove-now": ["arrow", ["String"], "Nothing"],
        "keys-now": ["arrow", [], "SetOfA"],
        "keys-list-now": ["arrow", [], ["List", ["tid", "a"]]],
        "items-now": ["arrow", [], ["List", ["tuple", ["String", ["tid", "a"]]]]],
        "count-now": ["arrow", [], "Number"],
        "has-key-now": ["arrow", ["String"], "Boolean"],
        "_equals": ["arrow", ["sdOfA", ["arrow", ["Any", "Any"], "Equality"]], "Equality"],
        "_output":  ["arrow", [["arrow", ["Any"], "VS"]], "VS"],
        "freeze": ["arrow", [], "sdOfA"],
        "seal": ["arrow", [], "msdOfA"]
      }],
    }
  },
  theModule: function(runtime, namespace, uri, VSlib){
    var O = runtime.makeObject;
    var F = runtime.makeFunction;
    var arity = runtime.checkArity;
    var get = runtime.getField;

    var VS = get(VSlib, "values");

    var brandMutable = runtime.namedBrander("mutable-string-dict", ["string-dict: mutable-string-dict brander"]);
    var brandImmutable = runtime.namedBrander("string-dict", ["string-dict: string-dict brander"]);

    var annMutable = runtime.makeBranderAnn(brandMutable, "MutableStringDict");
    var annImmutable = runtime.makeBranderAnn(brandImmutable, "StringDict");

    var checkMSD = function(v) { runtime._checkAnn(["string-dict"], annMutable, v); };
    var checkISD = function(v) { runtime._checkAnn(["string-dict"], annImmutable, v); };
      function applyBrand(brand, val) {
        return get(brand, "brand").app(val);
      }
      function hasBrand(brand, val) {
        return get(brand, "test").app(val);
      }

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
        var newArray = canEdit ? array : arrCopy(array);
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
        return new ImmutableMap(0);
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
            return this._root.keys();
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
        return newRoot ? new ImmutableMap(newSize, newRoot) : new ImmutableMap(0);
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

        this.keys = function() {
          return this.entries.map(function(kv) {
            return kv[0];
          })
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
          var newEntries = isEditable ? entries : arrCopy(entries);

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

        this.keys = function() {
          return [this.entry[0]];
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

        this.keys = function() {
          return this.entries.map(function(kv) {
            return kv[0];
          })
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
          var newEntries = isEditable ? entries : arrCopy(entries);

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

        this.keys = function() {
          var keys = new Array();
          var nodes = this.nodes;
          for (var ii = 0, maxIndex = nodes.length - 1; ii <= maxIndex; ii++) {
            var node = nodes[ii];
            if (node) {
              var nodeKeys = node.keys();
              Array.prototype.push.apply(keys, nodeKeys);
            }
          }
          return keys;
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
        this.keys = function() {
          var keys = new Array();
          var nodes = this.nodes;
          for (var ii = 0, maxIndex = nodes.length - 1; ii <= maxIndex; ii++) {
            var node = nodes[ii];
            if (node) {
              var nodeKeys = node.keys();
              Array.prototype.push.apply(keys, nodeKeys);
            }
          }
          return keys;
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

    
      function makeImmutableStringDict(underlyingMap) {

        var getISD = runtime.makeMethod1(function(_, key) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['get'], 2, $a, true); }
          runtime.checkString(key);
          var missing_value = {};
          var val = underlyingMap.get(key, missing_value);
          if (val === missing_value) {
            return runtime.ffi.makeNone();
          } else {
            return runtime.ffi.makeSome(val);
          }
        }, "get");

        var getValueISD = runtime.makeMethod1(function(_, key) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['get-value'], 2, $a, true); }
          runtime.checkString(key);
          var missing_value = {};
          var val = underlyingMap.get(key, missing_value);
          if (val === missing_value) {
            runtime.ffi.throwMessageException('Key ' + key + ' not found');
          }
          return val;
        }, "get-value");

        var setISD = runtime.makeMethod2(function(_, key, val) {
          if (arguments.length !== 3) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['set'], 3, $a, true); }
          runtime.checkString(key);
          runtime.checkPyretVal(val);
          var newMap = underlyingMap.set(key, val);
          return makeImmutableStringDict(newMap);
        }, "set");

        var mergeISD = runtime.makeMethod1(function(self, other) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["merge"], 2, $a, true); }
          checkISD(other);
          var otherKeys = runtime.getField(other, "keys-list").app();
          var otherKeysArr = runtime.ffi.toArray(otherKeys);
          if (otherKeysArr.length === 0) { return self; }
          var newMap = underlyingMap;
          for (var i = 0; i < otherKeysArr.length; i++) {
            newMap = newMap.set(otherKeysArr[i], runtime.getField(other, "get-value").app(otherKeysArr[i]));
          }
          return makeImmutableStringDict(newMap);
        }, "merge");

        var removeISD = runtime.makeMethod1(function(_, key) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['remove'], 2, $a, true); }
          runtime.checkString(key);
          var newMap = underlyingMap.remove(key);
          return makeImmutableStringDict(newMap);
        }, "remove");

        var hasKeyISD = runtime.makeMethod1(function(_, key) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['has-key'], 2, $a, true); }
          runtime.checkString(key);
          var missing_value = {};
          var val = underlyingMap.get(key, missing_value);
          if (val === missing_value) {
            return runtime.makeBoolean(false);
          } else {
            return runtime.makeBoolean(true);
          }
        }, "has-key");

        var keysISD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['keys'], 1, $a, true); }
          var keys = underlyingMap.keys();
          return runtime.ffi.makeTreeSet(keys.map(function(key) {
            return runtime.makeString(key);
          }));
        }, "keys");

       var itemsISD = runtime.makeMethod0(function(_) {
          var elts = [];
          var keys = underlyingMap.keys();
          for (var i = 0; i < keys.length; i++) {
            elts.push(runtime.makeTuple([keys[i], underlyingMap.get(keys[i])]));
          }
          return runtime.ffi.makeList(elts);
        }, "items");



     //var eachLoopISD = runtime.makeMethod1(eachLoop);
     
        var eachLoopISD = runtime.makeMethod1(function(self, func) {
          var keys = underlyingMap.keys();
          function callEachLoop() {
            return runtime.eachLoop(runtime.makeFunction(function(i) {
              return func.app(runtime.makeTuple([keys[i], underlyingMap.get(keys[i])])); 
            }, "callEachLoop"), 0, keys.length);
          }
         return callEachLoop();
        }, "each-loop");
       

        var eachISD = runtime.makeMethod1(function(self, func) {
          var keys = underlyingMap.keys();
           function deepCallTuple(i) {
            return runtime.safeCall(function() {
              return func.app(runtime.makeTuple([keys[i], underlyingMap.get(keys[i])]));
            }, function(result) {
                if((i + 1) == keys.length) { return runtime.nothing; }
                else { return deepCallTuple(i + 1); }
            },
            "deepCallTuple");
           }
           return deepCallTuple(0);
        }, "each");

        var keysListISD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['keys-list'], 1, $a, true);}
          var keys = underlyingMap.keys();
          return runtime.ffi.makeList(keys.map(function(key) {
            return runtime.makeString(key);
          }));
        }, "keys-list");

        var countISD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['count'], 1, $a, true); }
          var count = underlyingMap.size;
          return runtime.makeNumber(count);
        }, "count");

        var outputISD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['_output'], 1, $a, true); }
          var elts = [];
          var keys = underlyingMap.keys();
          var vsValue = get(VS, "vs-value");
          var vsStr = get(VS, "vs-str");
          for (var i = 0; i < keys.length; i++) {
            elts.push(vsValue.app(runtime.makeTuple([keys[i], underlyingMap.get(keys[i])])));
          }
          return get(VS, "vs-collection").app(
            runtime.makeString("string-dict"),
            runtime.ffi.makeList(elts));
        }, "output");

        

        var equalsISD = runtime.makeMethod2(function(self, other, recursiveEquality) {
          if (arguments.length !== 3) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['equals'], 3, $a, true); }
          if (!hasBrand(brandImmutable, other)) {
            return runtime.ffi.notEqual.app('', self, other);
          } else {
            var keys = underlyingMap.keys();
            var otherKeysLength = get(other, 'count').app();
            function equalsHelp() {
              if (keys.length === 0) {
                return runtime.ffi.equal;
              } else {
                var thisKey = keys.pop();
                if (!get(other, 'has-key').app(thisKey)) {
                  return runtime.ffi.notEqual.app('', self, other);
                } else {
                  return runtime.safeCall(function() {
                    return recursiveEquality.app(underlyingMap.get(thisKey),
                        get(other, 'get-value').app(thisKey));
                  },
                  function (result) {
                    if (runtime.ffi.isNotEqual(result)) {
                      return result;
                    } else {
                      return equalsHelp();
                    }
                  }, "recursiveEquality");
                }
              }
            }
            if (keys.length !== otherKeysLength) {
              return runtime.ffi.notEqual.app('', self, other);
            } else {
              return equalsHelp();
            }
          }
        }, "equals");

        var unfreezeISD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['unfreeze'], 1, $a, true); }
          var dict = Object.create(null);
          var keys = underlyingMap.keys();
          for (var ii = 0; ii < keys.length; ii++) {
            var key = keys[ii];
            var val = underlyingMap.get(key);
            dict[key] = val;
          }
          return makeMutableStringDict(dict);
        }, "unfreeze");

        obj = O({
          get: getISD,
          'get-value': getValueISD,
          set: setISD,
          merge: mergeISD,
          remove: removeISD,
          keys: keysISD,
          "keys-list": keysListISD,
          count: countISD,
          'has-key': hasKeyISD,
          'items': itemsISD,
          'each': eachISD,
          'each-loop' : eachLoopISD,
          _equals: equalsISD,
          _output: outputISD,
          unfreeze: unfreezeISD
        });

        return applyBrand(brandImmutable, obj);
      }

      function makeMutableStringDict(underlyingDict, sealed) {

        var getMSD = runtime.makeMethod1(function(_, key) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['get-now'], 2, $a, true); }
          runtime.checkString(key);
          var val = underlyingDict[key];
          if (val === undefined) {
            return runtime.ffi.makeNone();
          } else {
            return runtime.ffi.makeSome(val);
          }
        }, "get");

        var getValueMSD = runtime.makeMethod1(function(_, key) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["get-value-now"], 2, $a, true); }
          runtime.checkString(key);
          var val = underlyingDict[key];
          if (val === undefined) {
            runtime.ffi.throwMessageException("Key " + key + " not found");
          }
          return val;
        }, "get-value");

        var setMSD = runtime.makeMethod2(function(self, key, val) {
          if (arguments.length !== 3) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["set-now"], 3, $a, true); }
          if (sealed) {
            runtime.ffi.throwMessageException("Cannot modify sealed string dict");
          }
          runtime.checkString(key);
          runtime.checkPyretVal(val);
          underlyingDict[key] = val;
          return runtime.nothing;
        }, "set");

        var mergeMSD = runtime.makeMethod1(function(self, other) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["merge-now"], 2, $a, true); }
          checkMSD(other);
          var otherKeys = runtime.getField(other, "keys-list-now").app();
          var otherKeysArr = runtime.ffi.toArray(otherKeys);
          for (var i = 0; i < otherKeysArr.length; i++) {
            var key = otherKeysArr[i];
            var val = runtime.getField(other, "get-value-now").app(key);
            runtime.getField(self, "set-now").app(key, val);
          }
          return runtime.nothing;
        }, "merge");

        var removeMSD = runtime.makeMethod1(function(self, key) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["remove-now"], 2, $a, true); }
          if (sealed) {
            runtime.ffi.throwMessageException("Cannot modify sealed string dict");
          }
          runtime.checkString(key);
          delete underlyingDict[key];
          return runtime.nothing;
        }, "remove");

        var hasKeyMSD = runtime.makeMethod1(function(_, key) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["has-key-now"], 2, $a, true); }
          runtime.checkString(key);
          if (key in underlyingDict) {
            return runtime.makeBoolean(true);
          } else {
            return runtime.makeBoolean(false);
          }
        }, "has-key");

        var keysMSD = runtime.makeMethod0(function(self) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["keys-now"], 1, $a, true); }
          var keys = Object.keys(underlyingDict);
          return runtime.ffi.makeTreeSet(keys.map(function(mkey) {
            return runtime.makeString(mkey);
          }));
        }, "keys");

        var itemsMSD = runtime.makeMethod0(function(self) {
          var elts = [];
          var keys = Object.keys(underlyingDict);
          for (var i = 0; i < keys.length; i++) {
            elts.push(runtime.makeTuple([keys[i], underlyingDict[keys[i]]]));
          }
          return runtime.ffi.makeList(elts);
        }, "items");

        var eachLoopMSD = runtime.makeMethod1(function(self, func) {
          var keys = Object.keys(underlyingDict);
          function callEachLoop() {
            return runtime.eachLoop(runtime.makeFunction(function(i) {
              return func.app(runtime.makeTuple([keys[i], underlyingDict[keys[i]]])); 
            }, "callEachLoop"), 0, keys.length);
          }
          return callEachLoop();
        }, "each-loop");

        var eachMSD = runtime.makeMethod1(function(self, func) {
          var keys = Object.keys(underlyingDict);
           function deepCallTuple(i) {
            return runtime.safeCall(function() {
              return func.app(runtime.makeTuple([keys[i], underlyingDict[keys[i]]]));
            }, function(result) {
                if((i + 1) == keys.length) { return runtime.nothing; }
                else { return deepCallTuple(i + 1); }
            },
            "deepCallTuple");
           }
          return deepCallTuple(0);
        }, "each");
        
        var keysListMSD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['keys-list-now'], 1, $a, true); }
          var keys = Object.keys(underlyingDict);
          return runtime.ffi.makeList(keys.map(function(mkey) {
            return runtime.makeString(mkey);
          }));
        }, "keys-list");

        var countMSD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["count-now"], 1, $a, true); }
          return runtime.makeNumber(Object.keys(underlyingDict).length);
        }, "count");

        /*
        var toreprMSD = runtime.makeMethod1(function(self, recursiveToRepr) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["torepr"], 2, $a, true); }
          var keys = Object.keys(underlyingDict);
          var elts = [];
          function combine(elts) {
            //return "[string-dict: " + elts.join(", ") + "]";
            return "[mutable-string-dict: " + elts.join(", ") + "]";
          }
          function toreprElts() {
            if (keys.length === 0) { return combine(elts); }
            else {
              var thisKey = keys.pop();
              // The function recursiveToRepr is a callback for rendering
              // sub-elements of collections.  If we call it on anything other
              // than flat primitives, we need to use the following safeCall
              // calling convention, which makes this work with the stack
              // compilation strategy for Pyret.
              return runtime.safeCall(function() {
                return recursiveToRepr.app(underlyingDict[thisKey]);
              },
              function(result / * stringification of element * /) {
                elts.push(recursiveToRepr.app(thisKey));
                elts.push(result);
                return toreprElts();
              });
            }
          }
          return toreprElts();
        }, "torepr");
       */

        var outputMSD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['_output'], 1, $a, true); }
          var elts = [];
          var keys = Object.keys(underlyingDict);
          var vsValue = get(VS, "vs-value");
          for (var i = 0; i < keys.length; i++) {
            elts.push(vsValue.app(runtime.makeTuple([keys[i], underlyingDict[keys[i]]])));
          }
          return get(VS, "vs-collection").app(
            runtime.makeString("mutable-string-dict"),
            runtime.ffi.makeList(elts));
        }, "output");

        var equalsMSD = runtime.makeMethod2(function(self, other, recursiveEquality) {
          if (arguments.length !== 3) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["equals"], 3, $a, true); }
          if (!hasBrand(brandMutable, other)) {
            return runtime.ffi.notEqual.app("", self, other);
          } else {
            var keys = Object.keys(underlyingDict);
            var otherKeysLength = get(other, "count-now").app();
            function eqElts() {
              if (keys.length === 0) {
                return runtime.ffi.equal;
              } else {
                var thisKey = keys.pop();
                if (!get(other, 'has-key-now').app(thisKey)) {
                  return runtime.ffi.notEqual.app('', self, other);
                } else {
                  return runtime.safeCall(function() {
                    return recursiveEquality.app(underlyingDict[thisKey],
                        get(other, 'get-value-now').app(thisKey));
                  },
                  function (result) {
                    if (runtime.ffi.isNotEqual(result)) {
                      return result;
                    } else {
                      return eqElts();
                    }
                  }, "equalsMSD:recursive-equality");
                }
              }
            }
            if (keys.length !== otherKeysLength) {
              return runtime.ffi.notEqual.app("", self, other);
            } else {
              return eqElts();
            }
          }
        }, "equals");

        var freezeMSD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['freeze'], 1, $a, true); }
          var map = emptyMap();
          for (var key in underlyingDict) {
            map = map.set(key, underlyingDict[key]);
          }
          return makeImmutableStringDict(map);
        }, "freeze");

        var sealMSD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['seal'], 1, $a, true); }
          return makeMutableStringDict(underlyingDict, true);
        }, "seal");

        obj = O({
          'get-now': getMSD,
          'get-value-now': getValueMSD,
          'set-now': setMSD,
          'merge-now': mergeMSD,
          'remove-now': removeMSD,
          'keys-now': keysMSD,
          'keys-list-now': keysListMSD,
          'count-now': countMSD,
          'has-key-now': hasKeyMSD,
          'items-now': itemsMSD,
          'each' : eachMSD,
          'each-loop' : eachLoopMSD,
          _equals: equalsMSD,
          _output: outputMSD,
          freeze: freezeMSD,
          seal: sealMSD
        });

        return applyBrand(brandMutable, obj);
      }

      function internal_isMSD(obj) {
        return hasBrand(brandMutable, obj);
      }

      var jsCheckMSD =
          runtime.makeCheckType(internal_isMSD, "MutableStringDict")

      function isMutableStringDict(obj) {
        arity(1, arguments, "is-mutable-string-dict", false)
        return runtime.makeBoolean(internal_isMSD(obj))
      }

      function createMutableStringDict() {
        arity(0, arguments, "make-mutable-string-dict", false);
        var dict = Object.create(null);
        return makeMutableStringDict(dict);
      }

      function createMutableStringDictFromArray(array) {
        //TODO/Note (Sarah): doesn't check for duplicate keys?
        arity(1, arguments, "mutable-string-dict", false);
        runtime.checkArray(array);
        var key_missing = {};
        var dict = Object.create(null);
        var len = array.length;
        for(var i = 0; i < len; i += 1) {
          var key = array[i].vals[0];
          var val = array[i].vals[1];
          runtime.checkString(key);
        if (dict[key] !== key_missing) {
            runtime.ffi.throwMessageException("Creating mutable string dict with duplicate key " + key);
          }
          dict[key] = val;
        }
        return makeMutableStringDict(dict);
      }

      function internal_isISD(obj) {
        return hasBrand(brandImmutable, obj);
      }

      var jsCheckISD =
          runtime.makeCheckType(internal_isISD, "StringDict")

      function isImmutableStringDict(obj) {
        arity(1, arguments, "is-immutable-string-dict", false)
        return runtime.makeBoolean(internal_isISD(obj))
      }

      function dictEach(func, obj) {
      //TODO: check that func is a function and obj is a string dict
       arity(2, arguments,'dict-each', false);
       runtime.checkObject(obj);
       runtime.checkFunction(func)
       eachMethod = runtime.getField(obj, "each");
       return eachMethod.app(func);
      }
      
      function dictEachLoop(func, obj) {
      //TODO: check that func is a function and obj is a string dict
       arity(2, arguments,'dict-each-loop', false);
       runtime.checkObject(obj);
       runtime.checkFunction(func);
       eachMethod = runtime.getField(obj, "each-loop");
       return eachMethod.app(func);
      }


      function createImmutableStringDict() {
        arity(0, arguments, "make-string-dict", false);
        var map = emptyMap();
        return makeImmutableStringDict(map);
      }

      function createImmutableStringDictFromArray(array) {
        arity(1, arguments, "string-dict", false);
        runtime.checkArray(array);
        var key_missing = {};
        var map = emptyMap();
        var len = array.length;
        for(var i = 0; i < len; i += 1) {
          var key = array[i].vals[0];
          var val = array[i].vals[1];
          runtime.checkString(key);
          if (map.get(key, key_missing) !== key_missing) {
            runtime.ffi.throwMessageException("Creating immutable string dict with duplicate key " + key);
          }
          map = map.set(key, val);
        }
        return makeImmutableStringDict(map);
      }

      function createConstImmutableStringDict(names, val) {
        arity(2, arguments, "string-dict-of", false);
        runtime.checkList(names);
        var arr = runtime.ffi.toArray(names);
        var map = emptyMap();
        arr.forEach(function(k) {
          map = map.set(k, val)
        });
        return makeImmutableStringDict(map);
      }

      function createMutableStringDict0() {
        arity(0, arguments, "mutable-string-dict0", false);
        var dict = Object.create(null);
        return makeMutableStringDict(dict);
      }
      
      function createMutableStringDict1(arg) {
        arity(1, arguments, "mutable-string-dict1", false);
        var dict = Object.create(null);
        runtime.checkTuple(arg);
        runtime.checkString(arg.vals[0]);
        dict[arg.vals[0]] = arg.vals[1];
        return makeMutableStringDict(dict);
      }

      function createMutableStringDict2(a, b) {
        arity(2, arguments, "mutable-string-dict2", false);
        var dict = Object.create(null);
        runtime.checkTuple(a);
        runtime.checkTuple(b);
        var aval = a.vals[0];
        var bval = b.vals[0];
        runtime.checkString(aval);
        runtime.checkString(bval);
        if (aval == bval) {
          runtime.ffi.throwMessageException("Creating mutable string dict with duplicate key " + aval);
        }
        dict[aval] = a.vals[1];
        dict[bval] = b.vals[1];
        return makeMutableStringDict(dict);
      }

      function createMutableStringDict3(a, b, c) {
        arity(3, arguments, "mutable-string-dict3", false);
        var dict = Object.create(null);
        runtime.checkTuple(a);
        runtime.checkTuple(b);
        runtime.checkTuple(c);
        var aval = a.vals[0];
        var bval = b.vals[0];
        var cval = c.vals[0];
        runtime.checkString(aval);
        runtime.checkString(bval);
        runtime.checkString(cval);
        if (aval == bval || aval == cval || bval == cval) {
          if (aval == bval || aval == cval) {
            runtime.ffi.throwMessageException("Creating mutable string dict with duplicate key " + aval);
          }
          else {
            runtime.ffi.throwMessageException("Creating mutable string dict with duplicate key " + bval);
          }
        }
        dict[aval] = a.vals[1];
        dict[bval] = b.vals[1];
        dict[cval] = c.vals[1];
        return makeMutableStringDict(dict); 
      }

      function createMutableStringDict4(a, b, c, d) {
        arity(4, arguments, "mutable-string-dict4", false);
        var dict = Object.create(null);
        runtime.checkTuple(a);
        runtime.checkTuple(b);
        runtime.checkTuple(c);
        runtime.checkTuple(d);
        var aval = a.vals[0];
        var bval = b.vals[0];
        var cval = c.vals[0];
        var dval = d.vals[0];
        runtime.checkString(aval);
        runtime.checkString(bval);
        runtime.checkString(cval);
        runtime.checkString(dval);
        if (aval == bval || aval == cval || bval == cval || aval == dval || bval == dval || cval == dval) {
          if (aval == bval || aval == cval || aval == dval) {
            runtime.ffi.throwMessageException("Creating mutable string dict with duplicate key " + aval);
          }
          else if (bval == cval || bval == dval) {
            runtime.ffi.throwMessageException("Creating mutable string dict with duplicate key " + bval);
          }
          else {
            runtime.ffi.throwMessageException("Creating mutable string dict with duplicate key " + cval);
          }
        }
        dict[aval] = a.vals[1];
        dict[bval] = b.vals[1];
        dict[cval] = c.vals[1];
        dict[dval] = d.vals[1];
        return makeMutableStringDict(dict); 
      }

      function createMutableStringDict5(a, b, c, d, e) {
        arity(5, arguments, "mutable-string-dict5", false);
        var dict = Object.create(null);
        runtime.checkTuple(a);
        runtime.checkTuple(b);
        runtime.checkTuple(c);
        runtime.checkTuple(d);
        runtime.checkTuple(e);
        var aval = a.vals[0];
        var bval = b.vals[0];
        var cval = c.vals[0];
        var dval = d.vals[0];
        var e_val = e.vals[0];
        runtime.checkString(aval);
        runtime.checkString(bval);
        runtime.checkString(cval);
        runtime.checkString(dval);
        runtime.checkString(e_val);
        if (aval == bval || aval == cval || bval == cval || aval == dval || bval == dval || cval == dval || aval == e_val || bval == e_val || cval == e_val || dval == e_val) {
          if (aval == bval || aval == cval || aval == dval || aval == e_val) {
            runtime.ffi.throwMessageException("Creating mutable string dict with duplicate key " + aval);
          }
          else if (bval == cval || bval == dval || bval == e_val) { 
            runtime.ffi.throwMessageException("Creating mutable string dict with duplicate key " + bval);
          }
          else if (cval == dval || cval == e_val) {
            runtime.ffi.throwMessageException("Creating mutable string dict with duplicate key " + cval);
          }
          else {
            runtime.ffi.throwMessageException("Creating mutable string dict with duplicate key " + dval);
          }
        }
        dict[aval] = a.vals[1];
        dict[bval] = b.vals[1];
        dict[cval] = c.vals[1];
        dict[dval] = d.vals[1];
        dict[e_val] = e.vals[1];
        return makeMutableStringDict(dict); 

      }

      function createImmutableStringDict0() {
        arity(0, arguments, "string-dict0", false);
        var map = emptyMap();
        return makeImmutableStringDict(map);
      }

      function createImmutableStringDict1(arg) {
        arity(1, arguments, "string-dict1", false);
        var map = emptyMap();
        runtime.checkTuple(arg);
        runtime.checkString(arg.vals[0]);
        //Note (Sarah): mapping to full tuples eventually
        map = map.set(arg.vals[0], arg.vals[1]);
        return makeImmutableStringDict(map);
      }

      function createImmutableStringDict2(a, b) {
        arity(2, arguments, "string-dict2", false);
        var map = emptyMap();
        runtime.checkTuple(a);
        runtime.checkTuple(b);
        var aval = a.vals[0];
        var bval = b.vals[0];
        runtime.checkString(aval);
        runtime.checkString(bval);
        if (aval == bval) {
          runtime.ffi.throwMessageException("Creating immutable string dict with duplicate key " + aval);
        }
        map = map.set(aval, a.vals[1]);
        map = map.set(bval, b.vals[1]);
        return makeImmutableStringDict(map);
      }

      function createImmutableStringDict3(a, b, c) {
        arity(3, arguments, "string-dict3", false);
        var map = emptyMap();
        runtime.checkTuple(a);
        runtime.checkTuple(b);
        runtime.checkTuple(c);
        var aval = a.vals[0];
        var bval = b.vals[0];
        var cval = c.vals[0];
        runtime.checkString(aval);
        runtime.checkString(bval);
        runtime.checkString(cval);
        if (aval == bval || aval == cval || bval == cval) {
          //TODO: figure out which key is duplciated
          if (aval == bval || aval == cval) { 
            runtime.ffi.throwMessageException("Creating immutable string dict with duplicate key " + aval);
          }
          else {
            runtime.ffi.throwMessageException("Creating immutable string dict with duplicate key " + bval);
          }
        }
        map = map.set(aval, a.vals[1]);
        map = map.set(bval, b.vals[1]);
        map = map.set(cval, c.vals[1]);
        return makeImmutableStringDict(map);
      }

      function createImmutableStringDict4(a, b, c, d) {
        arity(4, arguments, "string-dict4", false);
        var map = emptyMap();
        runtime.checkTuple(a);
        runtime.checkTuple(b);
        runtime.checkTuple(c);
        runtime.checkTuple(d);
        var aval = a.vals[0];
        var bval = b.vals[0];
        var cval = c.vals[0];
        var dval = d.vals[0];
        runtime.checkString(aval);
        runtime.checkString(bval);
        runtime.checkString(cval);
        runtime.checkString(dval);
        if (aval == bval || aval == cval || bval == cval || aval == dval || bval == dval || cval == dval) {
          //TODO: figure out which key is duplciated
          if (aval == bval || aval == cval || aval == dval) {
             runtime.ffi.throwMessageException("Creating immutable string dict with duplicate key " + aval);
          }
          else if (bval == cval || bval == dval) {
             runtime.ffi.throwMessageException("Creating immutable string dict with duplicate key " + bval);
          }
          else { 
             runtime.ffi.throwMessageException("Creating immutable string dict with duplicate key " + cval);
          }
        }
        map = map.set(aval, a.vals[1]);
        map = map.set(bval, b.vals[1]);
        map = map.set(cval, c.vals[1]);
        map = map.set(dval, d.vals[1]);
        return makeImmutableStringDict(map);
      }

      function createImmutableStringDict5(a, b, c, d, e) {
        arity(5, arguments, "string-dict5", false);
        var map = emptyMap();
        runtime.checkTuple(a);
        runtime.checkTuple(b);
        runtime.checkTuple(c);
        runtime.checkTuple(d);
        runtime.checkTuple(e);
        var aval = a.vals[0];
        var bval = b.vals[0];
        var cval = c.vals[0];
        var dval = d.vals[0];
        var e_val = e.vals[0];
        runtime.checkString(aval);
        runtime.checkString(bval);
        runtime.checkString(cval);
        runtime.checkString(dval);
        runtime.checkString(e_val);
        if (aval == bval || aval == cval || bval == cval || aval == dval || bval == dval || cval == dval || aval == e_val || bval == e_val || cval == e_val || dval == e_val) {
          //TODO: figure out which key is duplciated
          if (aval == bval || aval == cval || aval == dval || aval == e_val) {
             runtime.ffi.throwMessageException("Creating immutable string dict with duplicate key " + aval);
          }
          else if (bval == cval || bval == dval || bval == e_val) {
             runtime.ffi.throwMessageException("Creating immutable string dict with duplicate key " + bval);
          }
          else if (cval == dval || cval == e_val) {
             runtime.ffi.throwMessageException("Creating immutable string dict with duplicate key " + cval);
          }
          else {
             runtime.ffi.throwMessageException("Creating immutable string dict with duplicate key " + dval);
          }   
        }
        map = map.set(aval, a.vals[1]);
        map = map.set(bval, b.vals[1]);
        map = map.set(cval, c.vals[1]);
        map = map.set(dval, d.vals[1]);
        map = map.set(e_val, e.vals[1]);
        return makeImmutableStringDict(map);
      }

      var vals = {
        "make-mutable-string-dict": F(createMutableStringDict, "make-mutable-string-dict"),
        "mutable-string-dict": O({
          make: F(createMutableStringDictFromArray, "mutable-string-dict:make"),
          make0: F(createMutableStringDict0, "mutable-string-dict:make0"),
          make1: F(createMutableStringDict1, "mutable-string-dict:make1"),
          make2: F(createMutableStringDict2, "mutable-string-dict:make2"),
          make3: F(createMutableStringDict3, "mutable-string-dict:make3"),
          make4: F(createMutableStringDict4, "mutable-string-dict:make4"),
          make5: F(createMutableStringDict5, "mutable-string-dict:make5")
        }),
        "is-mutable-string-dict": F(isMutableStringDict, "is-mutable-string-dict"),
        "make-string-dict": F(createImmutableStringDict, "make-string-dict"),
        "dict-each": F(dictEach, "dict-each"),
        "dict-each-loop": F(dictEachLoop, "dict-each-loop"),
        "string-dict": O({
          make: F(createImmutableStringDictFromArray, "string-dict:make"),
          make0: F(createImmutableStringDict0, "string-dict:make0"),
          make1: F(createImmutableStringDict1, "string-dict:make1"),
          make2: F(createImmutableStringDict2, "string-dict:make2"),
          make3: F(createImmutableStringDict3, "string-dict:make3"),
          make4: F(createImmutableStringDict4, "string-dict:make4"),
          make5: F(createImmutableStringDict5, "string-dict:make5")
        }),
        "string-dict-of": F(createConstImmutableStringDict, "string-dict-of"),
        "is-string-dict": F(isImmutableStringDict, "is-string-dict")
      };
      var types = {
        MutableStringDict: annMutable,
        StringDict: annImmutable
      };
      var internal = {
        checkISD: jsCheckISD,
        checkMSD: jsCheckMSD
      };
      return runtime.makeModuleReturn(vals, types, internal);
    }
  })
