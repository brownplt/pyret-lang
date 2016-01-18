#!/usr/bin/env node
var fs = require("fs");

var argparse = require("argparse");
var temp = require("temp");
var exec = require('sync-exec');

// Clean up temporary files on exit
temp.track();

var chrome2calltree = require("../index.js");
var packageJson = require("../package.json");

var parser = new argparse.ArgumentParser({
    version: packageJson.version,
    addHelp: true,
    description: "Convert CPU profiles exported from Chrome to callgrind format"
});
parser.addArgument(['-o', '--outfile'], {
    'help': "Save calltree stats to OUTFILE. " +
            "If omitted, writes to standard out."
});
parser.addArgument(['-i', '--infile'], {
    'help': "Read chrome CPU profile from INFILE. " +
            "If omitted, reads from standard in."
});
parser.addArgument(['-k', '--kcachegrind'], {
    'help': "Run the kcachegrind tool on the converted data. Unless combined " +
            "with the -o flag, will write output to a temporary file and " +
            "remove it after kcachegrind exits.",
    'action': 'storeTrue'
});

var args = parser.parseArgs();

var outStream;
var inStream;

if (args.infile) {
    inStream = fs.createReadStream(args.infile);
} else {
    if (process.stdin.isTTY) {
        parser.printHelp();
        process.exit(1);
    }
    inStream = process.stdin;
}

if (args.outfile) {
    outStream = fs.createWriteStream(args.outfile);
} else if (args.kcachegrind) {
    outStream = temp.createWriteStream({
        'prefix': 'chrome2calltree',
        'suffix': '.log'
    });
} else {
    outStream = process.stdout;
}

var readEntireStream = function(stream, cb) {
    var buffer = "";
    stream.resume();
    stream.setEncoding("utf8");
    stream.on("data", function(chunk) {
        buffer += chunk;
    });
    stream.on("end", function() {
        cb(buffer);
    });
};

var launchKCacheGrind = function(logpath) {
    var found = false;
    var commands = ['qcachegrind', 'kcachegrind'];
    for (var i = 0; i < commands.length; i++) {
        var cmd = commands[i];
        if (exec('which ' + cmd).status === 0) {
            exec(cmd + ' ' + outStream.path);
            found = true;
            break;
        }
    }
    if (!found) {
        process.stdout.write("Could not find kcachegrind executable. Tried: " +
                commands.join(", "));
        process.exit(1);
    }
};

readEntireStream(inStream, function(contents) {
    chrome2calltree.chromeProfileToCallgrind(JSON.parse(contents), outStream);
    outStream.on('finish', function() {
        if (outStream !== process.stdout) {
          outStream.close();
        }
        if (args.kcachegrind) {
            launchKCacheGrind();
        }
    });
    if (outStream !== process.stdout) {
      outStream.end();
    }
});
