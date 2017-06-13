browserify browserify-require-combined.js > nodemods.js
cat header.js nodemods.js $1 > compiled.js
