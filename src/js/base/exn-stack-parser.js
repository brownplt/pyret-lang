define("pyret-base/js/exn-stack-parser", ["source-map"], function(sourceMap) {

  // TODO(joe): make this match exactly 64 characters
  var matchHashedPyretURI = /_([a-f0-9]+)__/;
  var matchInnerEvalLocationV8 = /<anonymous>:([0-9]+):([0-9]+)/;
  var matchInnerEvalLocationFirefoxSafari = /([0-9]+):([0-9]+)$/;

  function isSourcePyretFrame(line) {
    return line.match(matchHashedPyretURI) !== null;
  }

  function parseFrame(line) {
    if(!isSourcePyretFrame(line)) {
      return {};
    }
    var matchedURI = line.match(matchHashedPyretURI);
    var matchedLoc1 = line.match(matchInnerEvalLocationV8);
    var matchedLoc2 = line.match(matchInnerEvalLocationFirefoxSafari);
    var matchedLoc = matchedLoc1 || matchedLoc2;
    // console.log(matchedLoc);
    return {
      startLine: matchedLoc[1],
      startCol: matchedLoc[2],
      hashedURI: matchedURI[1]
    };
  }

  function parseStack(stacktrace) {
    var lines = stacktrace.split("\n");
    return lines.filter(isSourcePyretFrame).map(parseFrame);
  }

  function convertExceptionToPyretStackTrace(e, program) {
    var parsedStack = parseStack(e.stack);
    var staticModules = program.staticModules;

    var pyretStack = parsedStack.map(function(frame) {
      var uri = program.uris[frame.hashedURI];
      // console.log("The URI for ", frame.hashedURI, " is ", uri);
      var moduleSourceMap = staticModules[uri].theMap;
      var consumer = new sourceMap.SourceMapConsumer(moduleSourceMap);
      consumer.computeColumnSpans();
      var original = consumer.originalPositionFor({
        source: uri,
        line: Number(frame.startLine),
        column: Number(frame.startCol) },
        sourceMap.SourceMapConsumer.LEAST_UPPER_BOUND);
      // console.log(original);
      var posForPyret = original.name.split(",");
      // NOTE(joe): this loop intentionally starts at one.  The split array
      // will be a length-7 array where the first is the URI of the module
      // the source location, and the remaining 6 are numbers for the line,
      // column, and character offsets of the start and end.
      for(var i = 1; i < posForPyret.length; i += 1) {
        posForPyret[i] = Number(posForPyret[i]);
      }
      return posForPyret;
    });

    if (e.pyretStack) {
      return pyretStack.concat(e.pyretStack);
    } else {
      return pyretStack;
    }
  }

  return {
    isSourcePyretFrame,
    parseFrame,
    parseStack,
    convertExceptionToPyretStackTrace
  };
});
