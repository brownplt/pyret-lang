
![Yarr](https://raw.github.com/brownplt/pyret-lang/master/img/pyret-banner.png)

[![Build Status](https://travis-ci.org/brownplt/pyret-lang.svg)](https://travis-ci.org/brownplt/pyret-lang)

Pyret + Stopify
===============

This branch contains the new experimental version of the Pyret compiler that
uses [Stopify](https://github.com/plasma-umass/stopify) for stack manangement.

This branch also contains the straight line compiler that can be used to
generate Pyret code without stack management instrumentation.

**NOTE**: Unlink master, this branch expects you to use
[yarn](https://yarnpkg.com/en/).  Rachit has had traumatic experiences with
`npm link` and refused to support it.

Basic Setup
-----------

Simply run the following commands to setup the compiler:
```
yarn install
make -B stopify-build
make
```

Note that this makes use of the latest release version of Stopify on npm.
If you want to work with the latest version of Stopify, follow the directions
below.

Developing with Stopify
-----------------------

1. Clone the [Stopify](https://github.com/plasma-umass/stopify) repository.
2. In the project root, run:
   ```
   Stopify $ yarn install && yarn run build
   Stopify $ cd stopify-continuations && yarn link && cd -
   Stopify $ cd stopify && yarn link && cd -
   ```
   The first two lines build stopify while the third line makes the `stopify`
   and `stopify-continuations` modules available globally.
3. Navigate to pyret-lang and run
   ```
   rm -rf node_modules/{stopify,stopify-continuations}
   yarn link stopify stopify-continuations
   ```
4. Build the runtime using `make -B stopify-build`. **NOTE**: This command
   needs to be re-run everytime the local copy of stopify is changed.
5. Build the pyret compiler using `make`.

**Troubleshooting**: If you're still using `npm`, then `npm link` is known to
have issues with linking up modules properly. If the above steps don't work,
the problem is mostly likely, with the linking. In order to get Stopify working
with Pyret, you need to get the following commands to succeed in a node
instance in the pyret-lang repository:
```
> require('stopify-continuations')
> require('stopify/dist/src/stopify/compileFunction')
```

Using the compiler from the CLI
-------------------------------

1. To build straight-line code without stopify, run `make <filename>.v.jarr` in
   the repository's top-level.

2. To build a pyret file with stopify, run `make <filename>.vs.jarr` in
   the repository's top-level.

Stopify's options for pyret are configured in require-node-dependencies.js. For
more information about the options, take a look at the stopify
[README](https://github.com/plasma-umass/Stopify/blob/master/README.md).
