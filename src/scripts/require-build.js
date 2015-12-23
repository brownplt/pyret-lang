({
    optimize: "uglify",
    paths: {
        'q': 'empty:',
        'fs': 'empty:',
        'path': 'empty:',
        'requirejs': 'empty:',
        's-expression': 'empty:',
        'seedrandom': 'empty:'
      },
      uglify: {
      	no_mangle: false,
      	mangle: true,
      	dead_code: true,
      	verbose: true,
      	reserved_names: ["define"]
      }
})