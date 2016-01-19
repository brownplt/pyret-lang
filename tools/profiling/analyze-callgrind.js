var StringMap        = require('./lib/stringmap/stringmap.js');
var fs               = require('fs');

var CONFIG           = require('./config');

var FUNC_REGEX       = /^fn=(.+)$/;
var FN_INDEX         = 1; // indexes into the match array of FUNC_REGEX
var MS_INDEX         = 1;
var HITS_INDEX       = 2; // indexes into subsequent line
var NAME_REGEX       = /^(.*)\s([^\s]*):(\d+)$/;
var NAME_INDEX       = 1;
var FILE_INDEX       = 2;
var LINE_INDEX       = 3; // these index into the match array of NAME_REGEX
var NEWLINE          = '\n';
var DEFAULT_NAME     = '(anonymous function)';
var DEFAULT_FILE     = '(unknown)';
var DEFAULT_LINE     = '(?)';
var NO_DIFFERENCE    = 'NO_DIFFERENCE';

var THRESHOLD_FACTOR = CONFIG.THRESHOLD_FACTOR;
var MIN_THRESHOLD    = CONFIG.MIN_THRESHOLD;




// return StringMap :: keys -> values,
//   keys:   full_name :: <string>
//   values: [{name :: <string>, self_hits :: <number>,
//            self_ms :: <number>}]
function readFile(filename, keepUnknowns) {
  var src = fs.readFileSync(filename, {'encoding': 'UTF-8'});
  var lines = src.split(NEWLINE);

  var functionMap = {};

  for (var i = 0; i < lines.length; i++) {
    var match = lines[i].match(FUNC_REGEX);
    if (match) {
      var theseData = lines[i+1].split(' ');
      var selfMs    = parseInt(theseData[MS_INDEX]);
      var selfHits  = parseInt(theseData[HITS_INDEX]);

      var fnName    = match[FN_INDEX];
      var nameData  = fnName.match(NAME_REGEX);
      var name      = nameData[NAME_INDEX] || DEFAULT_NAME;
      var file      = nameData[FILE_INDEX] || DEFAULT_FILE;
      var line      = nameData[LINE_INDEX] || DEFAULT_LINE;

      var includeMe = keepUnknowns ||
        ((file !== DEFAULT_FILE) && (line !== DEFAULT_LINE));

      if (includeMe) {
        var newObj = {
            name: name,
            file: file,
            line: line,
            self_ms: selfMs,
            self_hits: selfHits
          };

        if (functionMap.hasOwnProperty(fnName)) {
          functionMap[fnName].push(newObj);  
        } else {
          functionMap[fnName] = [newObj];
        }
        
      }
    }
  }
  return new StringMap(functionMap);
}

function logMissing (k, p) {
  var name;
  switch (p) {
    case 1: name = 'first'; break;
    case 2: name = 'second'; break;
    default: name = p.toString();
  }
  //var msg = ['Function', k, 'missing from', name, 'profile.'].join(' ');
  var msg = k;
  console.log(msg);
  return msg;
}

// @param functions :: StringMap
// return [keys] for which there are more than one callgrind entry
function getDuplicates (functions) {
  var keys = [];
  functions.forEach(function (v, k) {
    if (v.length > 1) {
      keys.push(k);
    }
  });
  return keys;
}

// @param functions :: StringMap
function totalByKey (functions, key) {
  var total = 0;
  functions.forEach(function (v, k) {
    for (var i = v.length - 1; i >= 0; i--) {
      total += v[i][key];
    }
  });
  return total;
}

function totalMs (functions) {
  return totalByKey(functions, 'self_ms');
}

function totalHits (functions) {
  return totalByKey(functions, 'self_hits');
}

function assertSameSource(obj1, obj2) {
  console.assert(obj1.name === obj2.name);
  console.assert(obj1.file === obj2.file);
  console.assert(obj1.line === obj2.line);
}

function percentageDiff (num1, num2) {
  if (num1 === 0) {
    return 0;
  } else {
    return ((100 * (num2 - num1))/num1);
  }
}

function diff (file1, file2) {
  var functions1 = readFile(file1, false);
  var functions2 = readFile(file2, false);

  var ms2 = totalMs(functions2);
  var ms1 = totalMs(functions1);
  var hits2 = totalHits(functions2);
  var hits1 = totalHits(functions1);

  var relMsDiff   = percentageDiff(ms1, ms2);
  var relHitsDiff = percentageDiff(hits1, hits2);

  var relMsDiffABS   = Math.abs(relMsDiff);
  var relHitsDiffABS = Math.abs(relHitsDiff);

  var functions = new StringMap();

  var thresholdMs   = Math.max(MIN_THRESHOLD, THRESHOLD_FACTOR * Math.abs(ms2 - ms1));
  var thresholdHits = Math.max(MIN_THRESHOLD, THRESHOLD_FACTOR * Math.abs(hits2 - hits1));

  var onlyIn1_ms   = 0;
  var onlyIn1_hits = 0;
  var onlyIn2_ms   = 0;
  var onlyIn2_hits = 0;

  // console.log('MISSING FROM FIRST PROFILE:');

  functions2.forEach(function (v1, k) {
    if (!functions1.has(k)) { 
      onlyIn2_ms   += v1[0].self_ms;
      onlyIn2_hits += v1[0].self_hits;
      //logMissing(k, 1);
    }
  });

  // console.log('\nMISSING FROM SECOND PROFILE:');

  functions1.forEach(function (v1, k) {
    var obj1 = v1[0];

    if (functions2.has(k)) {
      var obj2 = functions2.get(k)[0];

      assertSameSource(obj1, obj2);

      var newObj = {
          name: obj1.name,
          file: obj1.file,
          line: obj1.line,
          self_ms_diff: obj2.self_ms - obj1.self_ms,
          self_hits_diff: obj2.self_hits - obj1.self_hits,
          ms1: obj1.self_ms,
          ms2: obj2.self_ms,
          hits1: obj1.self_hits,
          hits2: obj2.self_hits
        };

      var msPercentDiff   = Math.abs(percentageDiff(obj1.self_ms, obj2.self_ms));
      var hitsPercentDiff = Math.abs(percentageDiff(obj1.self_hits, obj2.self_hits));
      
      var includeMe =
        msPercentDiff > relMsDiffABS &&
          Math.abs(newObj.self_ms_diff) > thresholdMs &&
          hitsPercentDiff > relHitsDiffABS &&
          Math.abs(newObj.self_hits_diff) > thresholdHits;

      if (includeMe) {
        functions.set(k, newObj);  
      }
    } else {
      onlyIn1_ms   += v1[0].self_ms;
      onlyIn1_hits += v1[0].self_hits;
      //logMissing(k, 2);
    }
  });

  var out = {
    functions: functions,
    ms1: ms1,
    ms2: ms2,
    hits1: hits1,
    hits2: hits2,
    onlyIn1_ms: onlyIn1_ms,
    onlyIn1_hits: onlyIn1_hits,
    onlyIn2_ms: onlyIn2_ms,
    onlyIn2_hits: onlyIn2_hits
  };

  return out;

}

function formatPercent (num, total) {
  return [(100 * num / total).toFixed(2).toString(), '%'].join('');
}

function formatSignPercent (percent, decPlaces) {
  return formatSignNum(percent.toFixed(decPlaces || 2)) + '%';
}

function formatChange(x1, x2) {
  return [
    x1.toString(), '=>', x2.toString(),
    '...',
    formatSignNum(x2 - x1),
    '(' + formatSignPercent(percentageDiff(x1, x2)) + ')'].join(' ');
}

// <number> -> <string>
function formatSignNum (num) {
  if (num > 0) {
    return '+' + num.toString();
  } else {
    return num.toString();
  }
}



/******************************************************************************/

var numArgs = process.argv.length;

if (numArgs === 3) {
  /* single file */
  var functions = readFile(process.argv[2], true);
  
  console.log('ms:  ', totalMs(functions));
  console.log('hits:', totalHits(functions));

} else if (numArgs === 4) {
  /* diff of two files */
  var file1 = process.argv[2];
  var file2 = process.argv[3];

  var difference = diff(file1, file2);

  printInfo(difference);

} else {
  console.log('Usage: node analyze-callgrind.js <file.profile>+');
}

function printInfo (difference) {
  var ms1   = difference.ms1;
  var ms2   = difference.ms2;
  var hits1 = difference.hits1;
  var hits2 = difference.hits2;
  console.log('ms:   %s', formatChange(ms1, ms2));
  console.log('hits: %s', formatChange(hits1, hits2));
  console.log();
  console.log('Not called in profile 1:\nms:   %s (%s of profile 2)\nhits: %s (%s of profile 2)',
    difference.onlyIn2_ms, formatPercent(difference.onlyIn2_ms, ms2), difference.onlyIn2_hits, formatPercent(difference.onlyIn2_hits, hits2));
  console.log('Not called in profile 2:\nms:   %s (%s of profile 1)\nhits: %s (%s of profile 1)', 
    difference.onlyIn1_ms, formatPercent(difference.onlyIn1_ms, ms1), difference.onlyIn1_hits, formatPercent(difference.onlyIn1_hits, hits1));
  console.log();

  difference.functions.forEach(function (obj, k) {
    console.log(k);
    console.log('ms:   %s', formatChange(obj.ms1, obj.ms2));
    console.log('hits: %s', formatChange(obj.hits1, obj.hits2));
    console.log('');
  });

}
