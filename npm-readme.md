```
Pyret Command-line Interface

  The pyret command compiles and runs Pyret programs. It helps manage a compile
  server that runs in the background to speed up compilation jobs, and manages
  state in a project's working directory to cache compiled files.

Basic Usage

  $ cat ahoy-world.arr
  check: "Ahoy " + "world!" is "Ahoy world!" end
  $ pyret ahoy-world.arr
  Starting Parley server...
  1/1 modules compiled
  Looks shipshape, your test passed, mate!


  This command compiled and ran ahoy-world.arr. The first time, this will take
  a few seconds as a server starts up in the background, in order to make
  future compiles fast.


  It's worth noting that the file is compiled into a standalone JavaScript file
  with the .jarr extension:

  $ node ahoy-world.jarr
  Looks shipshape, your test passed, mate!


  Most uses (e.g. for homework) only need to use the pyret command directly on
  .arr files, but there are several other options that can be provided.

Options

  -h, --help                  Show this help message.
  -p, --program <file>.arr    This is the default option, so using the flag is optional. Specifies the path
                              to the program to compile (usually a .arr file). Will start a server if one
                              isn't running, and will report an error if there is no .pyret in this
                              directory or in any parent of this directory. Generates a standalone compiled
                              file based on --outfile, and immediately executes it. The exit code is non-
                              zero if the file fails to compile, and is the exit code of the executed
                              program if it compiles successfully.
  -o, --outfile <file>.jarr   Specify the file to put the standalone compiled output into. The program can
                              be re-run without re-compiling by using the node command. Defaults to the
                              name of the --program with .arr replaced with .jarr.
  -q, --quiet                 Don't show the progress indicator output like "1/4 modules compiled"
  -k, --no-check-mode         Omit check blocks during compilation, and generate a standalone program that
                              doesn't print testing information at all.
  -n, --clean                 Removes all compiled code in the .pyret directory. This can be necessary when
                              Pyret updates to clear out stale compiled files.
  -s, --shutdown              Shuts down the currently-running compile server (if any is running), by
                              sending it a message over the specified or default --port
  -t, --port string           Specify the path to a socket file to use to communicate with the server.
                              Defaults to /tmp/parley-<username>/comm.sock.

The .pyret/ Directory

  The first time you run the pyret command in a directory, it creates a .pyret/
  directory there.

  This directory is used to store the compiled versions of individual .arr
  files in your project. They will appear in .pyret/compiled.

  Each directory in which you run pyret will have this sub-directory created.
  In general, you should never need to look in or modify the directory. If you
  want to look at the innards of Pyret, you can check out these files.

The Compile Server

  The compiler will not run without a running server. The pyret command
  tries to connect to a compile server on the specified port on
  startup, and if it cannot, starts one before continuing. The server
  accepts requests to start and stop compile jobs, and to shut down, and
  sends messages indicating compile status and when the job is complete.

  The default mode of operation is to have a single compile server running
  per user (hence the default naming of the --port). This makes it
  simple to change directories to different projects and get the benefits
  of the server's quick responses. If multiple compile jobs are sent at the
  same time, they are queued and processed in FIFO order.

  This server is experimental, and likely can get stuck running, or lose
  track of its connection, so it's worth specifying what resources it uses
  so they can be cleaned up. The server that's started in the background
  does the work of creating the specified socket file, and its process
  should clean up that file when it's done or when --shutdown is
  used. You can also manually remove it (check
  /tmp/parley-<username>/) if something gets wedged. This is
  the only bit of state that the client and server use to communicate. The
  server runs a file called pyret.jarr, so if you want to
  check on potential runaway server processes this command can find them:

  $ ps aux | grep pyret.jarr

Support and Contact

  This interface is experimental. Feedback and issue reports at
  https://github.com/brownplt/pyret-lang/issues/new are most appreciated.
```
