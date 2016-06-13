define(["js/runtime-util", "js/type-util", "js/namespace", "trove/valueskeleton"], function(util, t, Namespace, valueskeleton) {
  var sdOfA = t.tyapp(t.localType("StringDict"), [t.tyvar("a")]);
  var msdOfA = t.tyapp(t.localType("MutableStringDict"), [t.tyvar("a")]);
  return util.definePyretModule(
    "string-dict",
    [],
    {
      values:
      {
        "make-string-dict": t.forall(["a"], sdOfA),
        "string-dict":
          t.record({
            "make":
              // NOTE(joe): any for RawArray instantiation until we have tuples
              t.forall(["a"],
                t.arrow([t.tyapp(t.builtinName("RawArray"), [t.any])], sdOfA)),
            "make0": t.forall(["a"], t.arrow([], sdOfA)),
            "make1": t.forall(["a"], t.arrow([t.tyvar("a")], sdOfA)),
            "make2": t.forall(["a"], t.arrow([t.tyvar("a"), t.tyvar("a")], sdOfA)),
            "make3": t.forall(["a"], t.arrow([t.tyvar("a"), t.tyvar("a"), t.tyvar("a")], sdOfA)),
            "make4": t.forall(["a"], t.arrow([t.tyvar("a"), t.tyvar("a"), t.tyvar("a"), t.tyvar("a")], sdOfA)),
            "make5": 
              t.forall(["a"], t.arrow([t.tyvar("a"), t.tyvar("a"), t.tyvar("a"), t.tyvar("a"), t.tyvar("a")],
                                      sdOfA))
          }),
        "string-dict-of":
          t.forall(["a"],
            t.arrow(
              [
                t.tyapp(t.libName("lists", "List"), [t.builtinName("String")]),
                t.tyvar("a")
              ],
              sdOfA)),
        "make-mutable-string-dict": t.forall(["a"], t.arrow([], msdOfA)),
        "mutable-string-dict":
          t.record({
            "make":
              t.forall(["a"], t.arrow([t.tyapp(t.builtinName("RawArray"), [t.any])], msdOfA)),
            "make0": t.forall(["a"], t.arrow([], msdOfA)),
            "make1": t.forall(["a"], t.arrow([t.tyvar("a")], msdOfA)),
            "make2": t.forall(["a"], t.arrow([t.tyvar("a"), t.tyvar("a")], msdOfA)),
            "make3": t.forall(["a"], t.arrow([t.tyvar("a"), t.tyvar("a"), t.tyvar("a")], msdOfA)),
            "make4": t.forall(["a"], t.arrow([t.tyvar("a"), t.tyvar("a"), t.tyvar("a"), t.tyvar("a")], msdOfA)),
            "make5": 
              t.forall(["a"], t.arrow([t.tyvar("a"), t.tyvar("a"), t.tyvar("a"), t.tyvar("a"), t.tyvar("a")],
                                      msdOfA))
          })
      },
      aliases: {},
      datatypes: {
        StringDict: t.dataType(
          "StringDict",
          ["a"],
          [],
          {
            "get": t.arrow([t.string], t.tyapp(t.libName("option", "Option"), [t.tyvar("a")])),
            "get-value": t.arrow([t.string], t.tyvar("a")),
            "set": t.arrow([t.string, t.tyvar("a")], sdOfA),
            "merge": t.arrow([sdOfA], sdOfA),
            "remove": t.arrow([t.string], sdOfA),
            "keys": t.arrow([], t.tyapp(t.libName("sets", "Set"), [t.string])),
            "keys-list": t.arrow([], t.tyapp(t.libName("lists", "List"), [t.string])),
            "count": t.arrow([], t.number),
            "has-key": t.arrow([t.string], t.boolean),
            "unfreeze": t.arrow([], msdOfA),
            // TODO(joe): _output and _equals
          }
        ),
        MutableStringDict: t.dataType(
          "MutableStringDict",
          ["a"],
          [],
          {
            "get-now": t.arrow([t.string], t.tyapp(t.libName("option", "Option"), [t.tyvar("a")])),
            "get-value-now": t.arrow([t.string], t.tyvar("a")),
            "set-now": t.arrow([t.string, t.tyvar("a")], t.nothing),
            "merge-now": t.arrow([msdOfA], t.nothing),
            "remove-now": t.arrow([t.string], t.nothing),
            "keys-now": t.arrow([], t.tyapp(t.libName("sets", "Set"), [t.string])),
            "keys-list-now": t.arrow([], t.tyapp(t.libName("lists", "List"), [t.string])),
            "count-now": t.arrow([], t.number),
            "has-key-now": t.arrow([t.string], t.boolean),
            "freeze": t.arrow([], sdOfA),
            "seal": t.arrow([], msdOfA),
            // TODO(joe): _output and _equals
          }
        )
      }
    },
    function(runtime, namespace /* no pyret dependencies */) {
    return runtime.loadModulesNew(namespace, [valueskeleton], function(VSlib) {

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
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['get'], 2, $a); }
          runtime.checkString(key);
          var missing_value = {};
          var val = underlyingMap.get(key, missing_value);
          if (val === missing_value) {
            return runtime.ffi.makeNone();
          } else {
            return runtime.ffi.makeSome(val);
          }
        });

        var getValueISD = runtime.makeMethod1(function(_, key) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['get-value'], 2, $a); }
          runtime.checkString(key);
          var missing_value = {};
          var val = underlyingMap.get(key, missing_value);
          if (val === missing_value) {
            runtime.ffi.throwMessageException('Key ' + key + ' not found');
          }
          return val;
        });

        var setISD = runtime.makeMethod2(function(_, key, val) {
          if (arguments.length !== 3) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['set'], 3, $a); }
          runtime.checkString(key);
          runtime.checkPyretVal(val);
          var newMap = underlyingMap.set(key, val);
          return makeImmutableStringDict(newMap);
        });

        var mergeISD = runtime.makeMethod1(function(self, other) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["merge"], 2, $a); }
          checkISD(other);
          var otherKeys = runtime.getField(other, "keys-list").app();
          var otherKeysArr = runtime.ffi.toArray(otherKeys);
          if (otherKeysArr.length === 0) { return self; }
          var newMap = underlyingMap;
          for (var i = 0; i < otherKeysArr.length; i++) {
            newMap = newMap.set(otherKeysArr[i], runtime.getField(other, "get-value").app(otherKeysArr[i]));
          }
          return makeImmutableStringDict(newMap);
        });

        var removeISD = runtime.makeMethod1(function(_, key) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['remove'], 2, $a); }
          runtime.checkString(key);
          var newMap = underlyingMap.remove(key);
          return makeImmutableStringDict(newMap);
        });

        var hasKeyISD = runtime.makeMethod1(function(_, key) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['has-key'], 2, $a); }
          runtime.checkString(key);
          var missing_value = {};
          var val = underlyingMap.get(key, missing_value);
          if (val === missing_value) {
            return runtime.makeBoolean(false);
          } else {
            return runtime.makeBoolean(true);
          }
        });

        var keysISD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['keys'], 1, $a); }
          var keys = underlyingMap.keys();
          return runtime.ffi.makeTreeSet(keys.map(function(key) {
            return runtime.makeString(key);
          }));
        });

       var itemsISD = runtime.makeMethod0(function(_) {
          var elts = [];
          var keys = underlyingMap.keys();
          for (var i = 0; i < keys.length; i++) {
            elts.push(runtime.makeTuple([keys[i], underlyingMap.get(keys[i])]));
          }
          return runtime.ffi.makeList(elts);
        });

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
        });

        var keysListISD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['keys-list'], 1, $a);}
          var keys = underlyingMap.keys();
          return runtime.ffi.makeList(keys.map(function(key) {
            return runtime.makeString(key);
          }));
        });

        var countISD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['count'], 1, $a); }
          var count = underlyingMap.size;
          return runtime.makeNumber(count);
        });

        var outputISD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['_output'], 1, $a); }
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
        });

        

        var equalsISD = runtime.makeMethod2(function(self, other, recursiveEquality) {
          if (arguments.length !== 3) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['equals'], 3, $a); }
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
                  });
                }
              }
            }
            if (keys.length !== otherKeysLength) {
              return runtime.ffi.notEqual.app('', self, other);
            } else {
              return equalsHelp();
            }
          }
        });

        var unfreezeISD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['unfreeze'], 1, $a); }
          var dict = Object.create(null);
          var keys = underlyingMap.keys();
          for (var ii = 0; ii < keys.length; ii++) {
            var key = keys[ii];
            var val = underlyingMap.get(key);
            dict[key] = val;
          }
          return makeMutableStringDict(dict);
        });

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
          _equals: equalsISD,
          _output: outputISD,
          unfreeze: unfreezeISD
        });

        return applyBrand(brandImmutable, obj);
      }

      function makeMutableStringDict(underlyingDict, sealed) {

        var getMSD = runtime.makeMethod1(function(_, key) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['get-now'], 2, $a); }
          runtime.checkString(key);
          var val = underlyingDict[key];
          if (val === undefined) {
            return runtime.ffi.makeNone();
          } else {
            return runtime.ffi.makeSome(val);
          }
        });

        var getValueMSD = runtime.makeMethod1(function(_, key) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["get-value-now"], 2, $a); }
          runtime.checkString(key);
          var val = underlyingDict[key];
          if (val === undefined) {
            runtime.ffi.throwMessageException("Key " + key + " not found");
          }
          return val;
        });

        var setMSD = runtime.makeMethod2(function(self, key, val) {
          if (arguments.length !== 3) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["set-now"], 3, $a); }
          if (sealed) {
            runtime.ffi.throwMessageException("Cannot modify sealed string dict");
          }
          runtime.checkString(key);
          runtime.checkPyretVal(val);
          underlyingDict[key] = val;
          return runtime.nothing;
        });

        var mergeMSD = runtime.makeMethod1(function(self, other) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["merge-now"], 2, $a); }
          checkMSD(other);
          var otherKeys = runtime.getField(other, "keys-list-now").app();
          var otherKeysArr = runtime.ffi.toArray(otherKeys);
          for (var i = 0; i < otherKeysArr.length; i++) {
            var key = otherKeysArr[i];
            var val = runtime.getField(other, "get-value-now").app(key);
            runtime.getField(self, "set-now").app(key, val);
          }
          return runtime.nothing;
        });

        var removeMSD = runtime.makeMethod1(function(self, key) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["remove-now"], 2, $a); }
          if (sealed) {
            runtime.ffi.throwMessageException("Cannot modify sealed string dict");
          }
          runtime.checkString(key);
          delete underlyingDict[key];
          return runtime.nothing;
        });

        var hasKeyMSD = runtime.makeMethod1(function(_, key) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["has-key-now"], 2, $a); }
          runtime.checkString(key);
          if (key in underlyingDict) {
            return runtime.makeBoolean(true);
          } else {
            return runtime.makeBoolean(false);
          }
        });

        var keysMSD = runtime.makeMethod0(function(self) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["keys-now"], 1, $a); }
          var keys = Object.keys(underlyingDict);
          return runtime.ffi.makeTreeSet(keys.map(function(mkey) {
            return runtime.makeString(mkey);
          }));
        });

        var itemsMSD = runtime.makeMethod0(function(self) {
          var elts = [];
          var keys = Object.keys(underlyingDict);
          for (var i = 0; i < keys.length; i++) {
            elts.push(runtime.makeTuple([keys[i], underlyingDict[keys[i]]]));
          }
          return runtime.ffi.makeList(elts);
        });

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
        });

        var keysListMSD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['keys-list-now'], 1, $a); }
          var keys = Object.keys(underlyingDict);
          return runtime.ffi.makeList(keys.map(function(mkey) {
            return runtime.makeString(mkey);
          }));
        });

        var countMSD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["count-now"], 1, $a); }
          return runtime.makeNumber(Object.keys(underlyingDict).length);
        });

        var toreprMSD = runtime.makeMethod1(function(self, recursiveToRepr) {
          if (arguments.length !== 2) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["torepr"], 2, $a); }
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
              function(result /* stringification of element */) {
                elts.push(recursiveToRepr.app(thisKey));
                elts.push(result);
                return toreprElts();
              });
            }
          }
          return toreprElts();
        });

        var outputMSD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['_output'], 1, $a); }
          var elts = [];
          var keys = Object.keys(underlyingDict);
          var vsValue = get(VS, "vs-value");
          for (var i = 0; i < keys.length; i++) {
            elts.push(vsValue.app(runtime.makeTuple([keys[i], underlyingDict[keys[i]]])));
          }
          return get(VS, "vs-collection").app(
            runtime.makeString("mutable-string-dict"),
            runtime.ffi.makeList(elts));
        });

        var equalsMSD = runtime.makeMethod2(function(self, other, recursiveEquality) {
          if (arguments.length !== 3) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(["equals"], 3, $a); }
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
                  });
                }
              }
            }
            if (keys.length !== otherKeysLength) {
              return runtime.ffi.notEqual.app("", self, other);
            } else {
              return eqElts();
            }
          }
        });

        var freezeMSD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['freeze'], 1, $a); }
          var map = emptyMap();
          for (var key in underlyingDict) {
            map = map.set(key, underlyingDict[key]);
          }
          return makeImmutableStringDict(map);
        });

        var sealMSD = runtime.makeMethod0(function(_) {
          if (arguments.length !== 1) { var $a=new Array(arguments.length); for (var $i=0;$i<arguments.length;$i++) { $a[$i]=arguments[$i]; } throw runtime.ffi.throwArityErrorC(['seal'], 1, $a); }
          return makeMutableStringDict(underlyingDict, true);
        });

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
          'items': itemsMSD,
          'each' : eachMSD,
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
        arity(1, arguments, "is-mutable-string-dict")
        return runtime.makeBoolean(internal_isMSD(obj))
      }

      function createMutableStringDict() {
        arity(0, arguments, "make-mutable-string-dict");
        var dict = Object.create(null);
        return makeMutableStringDict(dict);
      }

      function createMutableStringDictFromArray(array) {
        arity(1, arguments, "mutable-string-dict");
        runtime.checkArray(array);
        var dict = Object.create(null);
        var len = array.length;
        for(var i = 0; i < len; i += 1) {
          var key = array[i].vals[0];
          var val = array[i].vals[1];
          runtime.checkString(key);
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
        arity(1, arguments, "is-immutable-string-dict")
        return runtime.makeBoolean(internal_isISD(obj))
      }

      function createImmutableStringDict() {
        arity(0, arguments, "make-string-dict");
        var map = emptyMap();
        return makeImmutableStringDict(map);
      }

      function createImmutableStringDictFromArray(array) {
        arity(1, arguments, "string-dict");
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
        arity(2, arguments, "string-dict-of");
        runtime.checkList(names);
        var arr = runtime.ffi.toArray(names);
        var map = emptyMap();
        arr.forEach(function(k) {
          map = map.set(k, val)
        });
        return makeImmutableStringDict(map);
      }

      function createMutableStringDict0() {
        arity(0, arguments, "mutable-string-dict0");
        var dict = Object.create(null);
        return makeMutableStringDict(dict);
      }
      
      function createMutableStringDict1(arg) {
        arity(1, arguments, "mutable-string-dict1");
        var dict = Object.create(null);
        runtime.checkTuple(arg);
        runtime.checkString(arg.vals[0]);
        dict[arg.vals[0]] = arg.vals[1];
        return makeMutableStringDict(dict);
      }

      function createMutableStringDict2(a, b) {
        arity(2, arguments, "mutable-string-dict2");
        var dict = Object.create(null);
        runtime.checkTuple(a);
        runtime.checkTuple(b);
        aval = a.vals[0];
        bval = b.vals[0];
        runtime.checkString(aval);
        runtime.checkString(bval);
        if (aval == bval) {
          runtime.ffi.throwMessageException("Creating immutable string dict with duplicate key " + aval);
        }
        dict[aval] = a.vals[1];
        dict[bval] = b.vals[1];
        return makeMutableStringDict(dict);
      }

      function createMutableStringDict3(a, b, c) {
        arity(3, arguments, "mutable-string-dict3");
        var dict = Object.create(null);
        runtime.checkTuple(a);
        runtime.checkTuple(b);
        runtime.checkTuple(c);
        aval = a.vals[0];
        bval = b.vals[0];
        cval = c.vals[0];
        runtime.checkString(aval);
        runtime.checkString(bval);
        runtime.checkString(cval);
        if (aval == bval || aval == cval || bval == cval) {
          runtime.ffi.throwMessageException("Creating immutable string dict with duplicate key ");
        }
        dict[aval] = a.vals[1];
        dict[bval] = b.vals[1];
        dict[cval] = c.vals[1];
        return makeMutableStringDict(dict); 
      }

      function createMutableStringDict4(a, b, c, d) {
        arity(4, arguments, "mutable-string-dict4");
        var dict = Object.create(null);
        runtime.checkTuple(a);
        runtime.checkTuple(b);
        runtime.checkTuple(c);
        runtime.checkTuple(d);
        aval = a.vals[0];
        bval = b.vals[0];
        cval = c.vals[0];
        dval = d.vals[0];
        runtime.checkString(aval);
        runtime.checkString(bval);
        runtime.checkString(cval);
        runtime.checkString(dval);
        if (aval == bval || aval == cval || bval == cval || aval == dval || bval == dval || cval == dval) {
          runtime.ffi.throwMessageException("Creating immutable string dict with duplicate key ");
        }
        dict[aval] = a.vals[1];
        dict[bval] = b.vals[1];
        dict[cval] = c.vals[1];
        dict[dval] = d.vals[1];
        return makeMutableStringDict(dict); 
      }

      function createMutableStringDict5(a, b, c, d, e) {
        arity(5, arguments, "mutable-string-dict5");
        var dict = Object.create(null);
        runtime.checkTuple(a);
        runtime.checkTuple(b);
        runtime.checkTuple(c);
        runtime.checkTuple(d);
        runtime.checkTuple(e);
        aval = a.vals[0];
        bval = b.vals[0];
        cval = c.vals[0];
        dval = d.vals[0];
        eval = e.vals[0];
        runtime.checkString(aval);
        runtime.checkString(bval);
        runtime.checkString(cval);
        runtime.checkString(dval);
        runtime.checkString(eval);
        if (aval == bval || aval == cval || bval == cval || aval == dval || bval == dval || cval == dval || aval == eval || bval == eval || cval == eval || dval == eval) {
          runtime.ffi.throwMessageException("Creating immutable string dict with duplicate key ");
        }
        dict[aval] = a.vals[1];
        dict[bval] = b.vals[1];
        dict[cval] = c.vals[1];
        dict[dval] = d.vals[1];
        dict[eval] = e.vals[1];
        return makeMutableStringDict(dict); 

      }

      function createImmutableStringDict0() {
        arity(0, arguments, "string-dict0");
        var map = emptyMap();
        return makeImmutableStringDict(map);
      }

      function createImmutableStringDict1(arg) {
        arity(1, arguments, "string-dict1");
        var map = emptyMap();
        runtime.checkTuple(arg);
        runtime.checkString(arg.vals[0]);
        //Note (Sarah): mapping to full tuples eventually
        map = map.set(arg.vals[0], arg.vals[1]);
        return makeImmutableStringDict(map);
      }

      function createImmutableStringDict2(a, b) {
        arity(2, arguments, "string-dict2");
        var map = emptyMap();
        runtime.checkTuple(a);
        runtime.checkTuple(b);
        aval = a.vals[0];
        bval = b.vals[0];
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
        arity(3, arguments, "string-dict3");
        var map = emptyMap();
        runtime.checkTuple(a);
        runtime.checkTuple(b);
        runtime.checkTuple(c);
        aval = a.vals[0];
        bval = b.vals[0];
        cval = c.vals[0];
        runtime.checkString(aval);
        runtime.checkString(bval);
        runtime.checkString(cval);
        if (aval == bval || aval == cval || bval == cval) {
          //TODO: figure out which key is duplciated
          runtime.ffi.throwMessageException("Creating immutable string dict with duplicate key ");
        }
        map = map.set(aval, a.vals[1]);
        map = map.set(bval, b.vals[1]);
        map = map.set(cval, c.vals[1]);
        return makeImmutableStringDict(map);
      }

      function createImmutableStringDict4(a, b, c, d) {
        arity(4, arguments, "string-dict4");
        var map = emptyMap();
        runtime.checkTuple(a);
        runtime.checkTuple(b);
        runtime.checkTuple(c);
        runtime.checkTuple(d);
        aval = a.vals[0];
        bval = b.vals[0];
        cval = c.vals[0];
        dval = d.vals[0];
        runtime.checkString(aval);
        runtime.checkString(bval);
        runtime.checkString(cval);
        runtime.checkString(dval);
        if (aval == bval || aval == cval || bval == cval || aval == dval || bval == dval || cval == dval) {
          //TODO: figure out which key is duplciated
          runtime.ffi.throwMessageException("Creating immutable string dict with duplicate key ");
        }
        map = map.set(aval, a.vals[1]);
        map = map.set(bval, b.vals[1]);
        map = map.set(cval, c.vals[1]);
        map = map.set(dval, d.vals[1]);
        return makeImmutableStringDict(map);
      }

      function createImmutableStringDict5(a, b, c, d, e) {
        arity(5, arguments, "string-dict5");
        var map = emptyMap();
        runtime.checkTuple(a);
        runtime.checkTuple(b);
        runtime.checkTuple(c);
        runtime.checkTuple(d);
        runtime.checkTuple(e);
        aval = a.vals[0];
        bval = b.vals[0];
        cval = c.vals[0];
        dval = d.vals[0];
        eval = e.vals[0];
        runtime.checkString(aval);
        runtime.checkString(bval);
        runtime.checkString(cval);
        runtime.checkString(dval);
        runtime.checkString(eval);
        if (aval == bval || aval == cval || bval == cval || aval == dval || bval == dval || cval == dval || aval == eval || bval == eval || cval == eval || dval == eval) {
          //TODO: figure out which key is duplciated
          runtime.ffi.throwMessageException("Creating immutable string dict with duplicate key ");
        }
        map = map.set(aval, a.vals[1]);
        map = map.set(bval, b.vals[1]);
        map = map.set(cval, c.vals[1]);
        map = map.set(dval, d.vals[1]);
        map = map.set(eval, e.vals[1]);
        return makeImmutableStringDict(map);
      }

      return O({
        "provide-plus-types": O({
          types: {
            MutableStringDict: annMutable,
            StringDict: annImmutable
          },
          values: O({
            "make-mutable-string-dict": F(createMutableStringDict),
            "mutable-string-dict": O({
              make: F(createMutableStringDictFromArray),
              make0: F(createMutableStringDict0),
              make1: F(createMutableStringDict1),
              make2: F(createMutableStringDict2),
              make3: F(createMutableStringDict3),
              make4: F(createMutableStringDict4),
              make5: F(createMutableStringDict5)
            }),
            "is-mutable-string-dict": F(isMutableStringDict),
            "make-string-dict": F(createImmutableStringDict),
            "string-dict": O({
              make: F(createImmutableStringDictFromArray),
              make0: F(createImmutableStringDict0),
              make1: F(createImmutableStringDict1),
              make2: F(createImmutableStringDict2),
              make3: F(createImmutableStringDict3),
              make4: F(createImmutableStringDict4),
              make5: F(createImmutableStringDict5)
            }),
            "string-dict-of": F(createConstImmutableStringDict),
            "is-string-dict": F(isImmutableStringDict)
          }),
          internal: {
            checkISD: jsCheckISD,
            checkMSD: jsCheckMSD
          }
        }),
        "answer": runtime.nothing
      });

    });
    });
});
