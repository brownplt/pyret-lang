(function() {
  
  var makePrimitiveProcedure = plt.baselib.functions.makePrimitiveProcedure;
  var makeList = plt.baselib.lists.makeList;
  var isString = plt.baselib.strings.isString;
  var isList = plt.baselib.lists.isList;
  var isEmpty = plt.baselib.lists.isEmpty;
  var isPair = plt.baselib.lists.isPair;
  var listLength = plt.baselib.lists.length;
  var checkList = plt.baselib.check.checkList;
  var checkPair = plt.baselib.check.checkPair;
  var checkString = plt.baselib.check.checkString;
  var exceptions = plt.baselib.exceptions;
  var checkAny = plt.baselib.check.makeCheckArgumentType(
    function(specimen) { return true; },
    'Any');

  var checkJSMap = plt.baselib.check.makeCheckArgumentType(
    function(specimen) { return (specimen instanceof JSMap); },
    'JSMap');

  function getElement(dict, javaScriptString) {
    if (javaScriptString === '__proto__') {
      return dict.proto;
    } else {
      if (javaScriptString in dict.normalKeys) {
        return dict.normalKeys[javaScriptString];
      } else {
        //console.log("Key miss on :", dict);
        exceptions.raise(M, exceptions.makeExnFailContract(
          plt.baselib.format.format("Key not found: ~a, in dictionary ~a", [javaScriptString, dict])),
          M.captureContinuationMarks());
      }
    }
  }

  function hasKey(dict, javaScriptString) {
    if (javaScriptString === '__proto__') {
      return (typeof dict.proto !== 'undefined');
    } else {
      return (typeof dict.normalKeys[javaScriptString] !== 'undefined');
    }
  }

  function getKeys(dict) {
    function getAllKeys(obj, skip) {
      if (obj === null) { return []; }
      else {
        //console.log(obj);
        var theseKeys = Object.keys(obj).filter(function(elt) {
          return !(skip.some(function(elt2) { return elt === elt2; }));
        });
        //console.log(theseKeys);
        return theseKeys.concat(getAllKeys(Object.getPrototypeOf(obj),
                                           theseKeys.concat(skip)));
      }
    }
    var keys = getAllKeys(dict.normalKeys, []);
    if (typeof dict.proto !== 'undefined') {
      keys.push('__proto__');
    }
    return keys;
  }

  function addElement(dict, javaScriptString, value) {
//    console.log("Dict is: ", dict);
    if (javaScriptString === '__proto__') {
      dict.proto = value;
    } else {
      dict.normalKeys[javaScriptString] = value;
    }
    return dict;
  }

  function addElements(M, aDict, aRacketList) {
//    console.log(aDict);
//    console.log(aRacketList);
    if (isEmpty(aRacketList)) {
      //console.log("It was empty");
      return aDict;
    } else {
      var thePair = aRacketList.first;
      var theKey = thePair.first;
      if (!(isString(theKey))) {
        exceptions.raise(M, exceptions.makeExnFailContract(
          plt.baselib.format.format("Expected string, got ~a", [theKey])),
          M.captureContinuationMarks());
      }
      var theValue = thePair.rest;
      var theJavaScriptString = String(theKey);
      var updatedOnce = addElement(aDict, theJavaScriptString, theValue);
      return addElements(M, updatedOnce, aRacketList.rest);
    }
  }

  function freshDict() {
    return { normalKeys: Object.create(null), proto: undefined };
  }

  function chainDict(oldDict) {
    return {
      normalKeys: Object.create(oldDict.normalKeys),
      proto: oldDict.proto
    };
  }

  function JSMap(dict) {
    this.dict = dict;
  }

  var makeJSMap = makePrimitiveProcedure(
    'make-js-map',
    1,
    function(M) {
      var theList = checkList(M, 'make-js-map', 0);
      var theDict = freshDict();
      return new JSMap(addElements(M, theDict, theList));
    }
  );

  var jsMapSet = makePrimitiveProcedure(
    'js-map-set',
    3,
    function(M) {
      var theMap = checkJSMap(M, 'js-map-set', 0);
      var theDict = theMap.dict;
      var theKey = checkString(M, 'js-map-set', 1);
      var theValue = checkAny(M, 'js-map-set', 2);
      var theJavaScriptString = String(theKey);
      var newDict = chainDict(theDict);
      return new JSMap(addElement(newDict, theJavaScriptString, theValue));
    }
  );

  var jsMapSetMany = makePrimitiveProcedure(
    'js-map-set*',
    2,
    function(M) {
      var theMap = checkJSMap(M, 'js-map-set*', 0);
      var theDict = theMap.dict;
      var theList = checkList(M, 'js-map-set*', 1);
      var newDict = chainDict(theDict);
      //console.log("Chained: ", newDict);
      var result = new JSMap(addElements(M, newDict, theList));
      //console.log("Result: ", result);
      return result;
    }
  );

  var jsMapGet = makePrimitiveProcedure(
    'js-map-get',
    2,
    function(M) {
      var theMap = checkJSMap(M, 'js-map-get', 0);
      var theDict = theMap.dict;
      var theKey = checkString(M, 'js-map-get', 1);
      var theJavaScriptString = String(theKey);
      return getElement(theDict, theJavaScriptString);
    }
  );

  var jsMapKeys = makePrimitiveProcedure(
    'js-map-keys',
    1,
    function(M) {
      var theMap = checkJSMap(M, 'js-map-keys', 0);
      var theDict = theMap.dict;
      return makeList.apply(null, getKeys(theDict));
    }
  );

  var jsMapHasKey = makePrimitiveProcedure(
    'js-map-has-key',
    2,
    function(M) {
      var theMap = checkJSMap(M, 'js-map-has-key', 0);
      var theDict = theMap.dict;
      var theString = checkString(M, 'js-map-has-key', 1);
      var theJavaScriptString = String(theString);
      return hasKey(theDict, theJavaScriptString);
    }
  );

  plt.runtime.jsMap = {};
  plt.runtime.jsMap.getElement = getElement;
  plt.runtime.jsMap.hasKey = hasKey;
  plt.runtime.jsMap.getKeys = getKeys;
  plt.runtime.jsMap.addElements = addElements;

  EXPORTS['make-js-map'] = makeJSMap;
  EXPORTS['js-map-set'] = jsMapSet;
  EXPORTS['js-map-set*'] = jsMapSetMany;
  EXPORTS['js-map-get'] = jsMapGet;
  EXPORTS['js-map-keys'] = jsMapKeys;
  EXPORTS['js-map-has-key?'] = jsMapHasKey; 
}());

