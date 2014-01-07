j = require('jasmine-node');

j.executeSpecsInFolder({
    specFolders: ["test/"],
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

