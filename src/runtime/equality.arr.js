// Pyret wrapper around equality.ts


const EQUALITY_CORE = require('./equality.js');


// Hack needed b/c of interactions with the 'export' keyword
// Pyret instantiates singleton data varaints by taking a reference to the value
// TODO(alex): Should Pyret perform a function call to create a singleton data variant
module.exports["Equal"] = EQUALITY_CORE.Equal();
module.exports["NotEqual"] = EQUALITY_CORE.NotEqual;
module.exports["Uknown"] = EQUALITY_CORE.Unknown;

// Hack needed to match generate Pyret-code
module.exports["is-Equal"] = EQUALITY_CORE.isEqual;
module.exports["is-NotEqual"] = EQUALITY_CORE.isNotEqual;
module.exports["is-Unknown"] = EQUALITY_CORE.isUnknown;

module.exports["to-boolean"] = EQUALITY_CORE.to_boolean;
module.exports["equal-or"] = EQUALITY_CORE.equal_or;
module.exports["equal-and"] = EQUALITY_CORE.equal_and;
