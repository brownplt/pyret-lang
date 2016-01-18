# chrome2calltree

Convert CPU profiles exported from Chrome to callgrind format.

![uwflow.com example](http://i.imgur.com/yWHKvzQ.png)

# Installation

This is only useful if you have [KCacheGrind][1] or QCacheGrind installed.

To install KCacheGrind on Linux:

```
sudo apt-get install kcachegrind
```

To install QCacheGrind on OS X:

```
brew install qcachegrind
```

Once you have that, grab the npm package:

```
npm install -g chrome2calltree
```

# Usage

Collect a Chrome CPU profile either by using [`console.profile()`][2] or by 
clicking buttons in the Developer Tools. Save the profile somewhere to disk.

![save Chrome CPU profile](http://i.imgur.com/eoiOesw.png)

Assuming you have KCacheGrind or QCacheGrind installed, you can then load your 
CPU profile using `chrome2calltree`. Note that it might open in the background, 
so you might have to manually switch applications.

```
chrome2calltree -k -i ~/Downloads/CPU-20131213T155541.cpuprofile
```

![qcachegrind showing a CPU profile](http://i.imgur.com/yaGMJNA.png)

If you want to save the converted callgrind output for later, you can do that 
using the `-o` flag.

```
chrome2calltree -i ~/Downloads/CPU-20131213T155541.cpuprofile -o foobar.profile
```

```
usage: chrome2calltree [-h] [-v] [-o OUTFILE] [-i INFILE] [-k]

Convert CPU profiles exported from Chrome to callgrind format

Optional arguments:
  -h, --help            Show this help message and exit.
  -v, --version         Show program's version number and exit.
  -o OUTFILE, --outfile OUTFILE
                        Save calltree stats to OUTFILE. If omitted, writes to
                        standard out.
  -i INFILE, --infile INFILE
                        Read chrome CPU profile from INFILE. If omitted,
                        reads from standard in.
  -k, --kcachegrind     Run the kcachegrind tool on the converted data.
                        Unless combined with the -o flag, will write output
                        to a temporary file and remove it after kcachegrind
                        exits.
```

[1]: http://kcachegrind.sourceforge.net/html/Download.html
[2]: https://developers.google.com/chrome-developer-tools/docs/console-api#consoleprofilelabel
