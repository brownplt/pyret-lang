define("pyret-base/js/exn-stack-parser", [], function() {

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
    console.log(matchedLoc);
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

  return {
    isSourcePyretFrame,
    parseFrame,
    parseStack
  };
});
