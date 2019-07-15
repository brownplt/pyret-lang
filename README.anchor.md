![Yarr](https://raw.github.com/brownplt/pyret-lang/master/img/pyret-banner.png)

Installing
-----------

### Setting up the environment

Using Linux or MacOS is highly recommended.

First, install npm, Node.js, git, and Python.

It is important to have a recent version of npm and Node.js. You can install a recent version 
of these by following the instructions [here]('https://github.com/nodesource/distributions/blob/master/README.md').

Install git and Python through your package manager.

Next, clone the pyret-lang repository from GitHub, and switch to the anchor branch.

```shell
~ $ git clone 'https://github.com/brownplt/pyret-lang.git'
~ $ cd pyret-lang
~/pyret-lang $ git checkout anchor
```

### Installing dependencies and launching the server

Install the required dependencies with npm.

```shell
~/pyret-lang $ npm install
```

Now, build the compiler.

```shell
~/pyret-lang $ npm run web
```

Next, start the server.

```shell
~/pyret-lang $ cd build/worker
~/pyret-lang/build/worker $ python --version           # check Python version
~/pyret-lang/build/worker $ python -m http.server      # if using Python 3
~/pyret-lang/build/worker $ python -m SimpleHTTPServer # if using python 2.7
```

Once the server starts, navigate to http://0.0.0.0:8000 in a web browser.
