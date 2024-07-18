if("this is a silly way to make dependency-tree notice checker.js" ===
   "without actually instantiating it for all modules (which can cause cycles)" +
   "the actual instantiation is in runtime.ts, initializeCheckContext"
) {
    module.exports.checkerLib = require('./checker.js');
}