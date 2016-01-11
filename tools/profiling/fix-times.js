var fs = require('fs');

var filename = process.argv[2];

var src = fs.readFileSync(filename, {'encoding': 'UTF-8'});
var profile = JSON.parse(src);

// console.log(profile.startTime, profile.endTime)

var trueStartTime = profile.timestamps[0];
var trueEndTime   = profile.timestamps[profile.timestamps.length - 1];

// microseconds -> seconds
var newStartTime = trueStartTime / 1000000;
var newEndTime   = trueEndTime   / 1000000;

profile.startTime = newStartTime;
profile.endTime   = newEndTime;

var newSrc = JSON.stringify(profile);

fs.writeFileSync(filename, newSrc);


// console.log(profile.startTime, profile.endTime)
// console.log("s:", profile.endTime - profile.startTime)