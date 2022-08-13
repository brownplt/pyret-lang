// Pyret wrapper around equality.ts


const EQUALITY_CORE = require('./equality.js');


// Hack needed b/c of interactions with the 'export' keyword
// Pyret instantiates singleton data varaints by taking a reference to the value
// TODO(alex): Should Pyret perform a function call to create a singleton data variant
module.exports["Equal"] = EQUALITY_CORE.Equal;
module.exports["NotEqual"] = EQUALITY_CORE.NotEqual;
module.exports["Uknown"] = EQUALITY_CORE.Unknown;

// Hack needed to match generate Pyret-code
module.exports["is-Equal"] = EQUALITY_CORE.isEqual;
module.exports["is-NotEqual"] = EQUALITY_CORE.isNotEqual;
module.exports["is-Unknown"] = EQUALITY_CORE.isUnknown;

module.exports["to-boolean"] = EQUALITY_CORE.to_boolean;
module.exports["equal-or"] = EQUALITY_CORE.equal_or;
module.exports["equal-and"] = EQUALITY_CORE.equal_and;

module.exports["within"] = EQUALITY_CORE.within;
module.exports["within-now"] = EQUALITY_CORE.withinNow;
module.exports["within-rel"] = EQUALITY_CORE.withinRel;
module.exports["within-abs"] = EQUALITY_CORE.withinAbs;
module.exports["within-rel-now"] = EQUALITY_CORE.withinRelNow;
module.exports["within-abs-now"] = EQUALITY_CORE.withinAbsNow;

module.exports["within3"] = EQUALITY_CORE.within3;
module.exports["within-now3"] = EQUALITY_CORE.withinNow3;
module.exports["within-rel3"] = EQUALITY_CORE.withinRel3;
module.exports["within-abs3"] = EQUALITY_CORE.withinAbs3;
module.exports["within-rel-now3"] = EQUALITY_CORE.withinRelNow3;
module.exports["within-abs-now3"] = EQUALITY_CORE.withinAbsNow3;

module.exports["roughly-equal"] = EQUALITY_CORE.roughlyEqualAlways;
module.exports["roughly-equal-always"] = EQUALITY_CORE.roughlyEqualAlways;
module.exports["roughly-equal-always3"] = EQUALITY_CORE.roughlyEqualAlways3;
module.exports["roughly-equal-now"] = EQUALITY_CORE.roughlyEqualNow;
module.exports["roughly-equal-now3"] = EQUALITY_CORE.roughlyEqualNow3;

module.exports["equal-now"] = EQUALITY_CORE.equalNow;
module.exports["equal-now3"] = EQUALITY_CORE.equalNow3;

module.exports["identical"] = EQUALITY_CORE.identical;
module.exports["identical3"] = EQUALITY_CORE.identical3;

module.exports["equal-always"] = EQUALITY_CORE.equalAlways;
module.exports["equal-always3"] = EQUALITY_CORE.equalAlways3;

module.exports["_lessthan"] = EQUALITY_CORE._lessthan;
module.exports["_greaterthan"] = EQUALITY_CORE._greaterthan;
module.exports["_lessequal"] = EQUALITY_CORE._lessequal;
module.exports["_greaterequal"] = EQUALITY_CORE._greaterequal;
