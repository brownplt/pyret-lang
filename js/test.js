j = require('jasmine-node');

j.executeSpecsInFolder({
    specFolders: [process.argv[2]],
    onComplete:   function() { console.log("done"); },
    isVerbose:    true,
    showColors:   true,
    teamcity:     false,
    useRequireJs: false,
    regExpSpec:   false,
    junitreport:  false,
    includeStackTrace: true,
    growl:        false
  });

