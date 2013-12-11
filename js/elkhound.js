const assert = require('assert');

//////////////////////////////////
////////// Ordered Sets //////////
//////////////////////////////////

function OrderedSet(items, comparison) {
  this.elements = {};
  this.ordered = [];
  this.comparison = comparison
  if (items instanceof OrderedSet) {
    if (!this.comparison)
      this.comparison = items.comparison;
    for (var i = 0; i < items.ordered.length; i++)
      this.add(items.ordered[i]);
  } else if (items) {
    for (var i = 0; i < items.length; i++)
      this.add(items[i]);
  }
}
OrderedSet.fromSerializable = function(obj, comparison, f) {
  var ret = new OrderedSet([], comparison);
  for (var i = 0; i < obj.length; i++) {
    if (f)
      ret.add(f(obj[i]));
    else
      ret.add(obj[i]);
  }
  return ret;
}
OrderedSet.equals = function orderedSetEquals(thiz, that) {
  if (thiz.size() !== that.size()) return false;
  for (var i = 0; i < thiz.ordered.length; i++)
    if (!that.contains(thiz.ordered[i]))
      return false;
  return true;
}
OrderedSet.prototype.toString = function(sep_lines, sorted) {
  var s = "";
  var items_strs = []
  for (var i = 0; i < this.ordered.length; i++)
    items_strs[i] = this.ordered[i].toString();
  if (sorted)
    items_strs.sort();
  for (var i = 0; i < items_strs.length; i++) {
    if (s == "")
      s += "{" + items_strs[i]
    else
      s += (sep_lines ? ",\n " : ", ") + items_strs[i];
  }
  if (s == "")
    return "{}";
  return s + "}";
}
OrderedSet.prototype.equals = function(that) { return OrderedSet.equals(this, that); }
OrderedSet.prototype.contains = function(item) {
  var key = item.toString();
  if (this.elements.hasOwnProperty(key))
    return (this.indexOfHelp(this.elements[key], item) >= 0);
  return false;
}
OrderedSet.prototype.add = function(item) {
  var key = item.toString();
  if (this.elements.hasOwnProperty(key)) {
    var items = this.elements[key];
    var index = this.indexOfHelp(items, item);
    if (index === -1) {
      items.push(item);
      this.ordered.push(item);
      return true;
    }
  } else {
    this.elements[key] = [item];
    this.ordered.push(item);
    return true;
  }
  return false;
}
OrderedSet.prototype.indexOf = function(item) {
  return this.indexOfHelp(this.ordered, item);
}
OrderedSet.prototype.indexOfHelp = function(items, item) {
  for (var i = 0; i < items.length; i++) {
    if ((this.comparison && this.comparison(items[i], item)) ||
        ((!this.comparison) && (items[i] == item))) {
      return i;
    }
  }
  return -1;
}  
OrderedSet.prototype.remove = function(item) {
  var key = item.toString();
  if (this.elements.hasOwnProperty(key)) {
    this.ordered.splice(this.indexOf(item), 1);
    this.elements[key].splice(this.indexOfHelp(this.elements[key], item), 1);
  }
  return this;
}
OrderedSet.prototype.size = function() { return this.ordered.length; }
OrderedSet.prototype.get = function(index) { return this.ordered[index]; }
OrderedSet.prototype.union = function(that) {
  var res = new OrderedSet(this.comparison);
  for (var i = 0; i < this.ordered.length; i++)
    res.add(this.ordered[i]);
  for (var i = 0; i < that.ordered.length; i++)
    res.add(that.ordered[i]);
  return res;
}
OrderedSet.prototype.merge = function(that) {
  var ret = false;
  for (var i = 0; i < that.ordered.length; i++)
    ret = this.add(that.ordered[i]) || ret;
  return ret;
}
OrderedSet.prototype.subtract = function(that) {
  for (var i = 0; i < that.ordered.length; i++)
    this.remove(that.ordered[i]);
  return this;
}
OrderedSet.prototype.inter = function(that) {
  var ret = new OrderedSet(this.comparison);
  var start = this;
  if (this.size() > that.size())
    start = that;
  if (start == that)
    that = this;
  for (var i = 0; i < start.ordered.length; i++)
    if (that.contains(start.ordered[i]))
      ret.add(start.ordered[i])
  return ret;
}
OrderedSet.prototype.toSerializable = function() {
  var ret = [];
  for (var i = 0; i < this.ordered.length; i++) {
    if (this.ordered[i].toSerializable)
      ret[i] = this.ordered[i].toSerializable()
    else
      ret[i] = this.ordered[i];
  }
  return ret;
}  

//////////////////////////////////
///////////// Queues /////////////
//////////////////////////////////

function Queue(items) {
  this.items = items.slice(0);
  this.start = 0;
  this.end = items.length;
  this.length = items.length;
}
Queue.prototype.push = function(item) {
  this.items[this.end] = item;
  this.end++;
  this.length++;
}
Queue.prototype.shift = function() {
  if (this.length === 0) return undefined;
  var ret = this.items[this.start];
  this.start++;
  this.length--;
  return ret;
}
Queue.prototype.get = function(idx) {
  if (this.start + idx < this.end) return this.items[this.start + idx];
  return undefined;
}
Queue.prototype.insertAt = function(idx, item) {
  if (this.start + idx < this.end) {
    this.items.splice(this.start + idx, 0, item);
    this.end++;
    this.length++;
  } else
    this.push(item);
}

//////////////////////////////////
///////////// Atoms //////////////
//////////////////////////////////

function Atom() {}
Atom.equals = function(thiz, that) {
  if (thiz === that) return true;
  if ((thiz instanceof Nonterm) && (that instanceof Nonterm) && (thiz.name == that.name)) return true;
  if ((thiz instanceof Token) && (that instanceof Token) && (thiz.name == that.name)) return true;
  if ((thiz instanceof Lit) && (that instanceof Lit) && (thiz.str == that.str)) return true;
  return false;
}
Atom.equals.toString = function() { return "Atom.equals" };
Atom.fromSerializable = function(obj) {
  if (obj === undefined) return undefined;
  if (obj.type === "Nonterm") return new Nonterm(obj.name);
  if (obj.type === "Token") return new Token(obj.name, obj.value);
  if (obj.type === "Lit") return new Lit(obj.str);
  if (obj.type === "EOF") return EOF;
  if (obj.type === "EPSILON") return EPSILON;
  if (obj.type === "HASH") return HASH;
  return null;
}
function Nonterm(name) {
  this.name = name;
}
Nonterm.prototype = Object.create(Atom.prototype);
Nonterm.prototype.toString = function() { return this.name; }
function Token(name, value) {
  this.name = name;
  if (value !== undefined)
    this.value = value;
  else
    this.value = name;
}
Nonterm.prototype.toSerializable = function() { return {type: "Nonterm", name: this.name}; }
Token.prototype = Object.create(Atom.prototype);
Token.prototype.toString = function(showVal) { 
  if (showVal && this.name !== this.value)
    return "('" + this.name + " " + JSON.stringify(this.value) + ")";
  else
    return "'" + this.name; 
}
Token.prototype.toSerializable = function() { return {type: "Token", name: this.name, value: this.value}; }
function Lit(str) {
  this.str = str;
  this.asString = '"' + this.str.toString().replace(/[\\"']/g, '\\$&') + '"';
}
Lit.prototype = Object.create(Token.prototype);
Lit.prototype.toString = function() { return this.asString; }
Lit.prototype.toSerializable = function() { return {type: "Lit", str: this.str}; }

const EOF = Object.create(Token.prototype, 
                          {name: {enumerable: true, value: "EOF"}, 
                           toString: {value: function() { return "$"; }},
                           toSerializable: {value: function() { return {type:"EOF"}; } }});
const EPSILON = Object.create(Atom.prototype, 
                              {name: {enumerable: true, value: "EPSILON"}, 
                               toString: {value: function() { return "ε"; }},
                               toSerializable: {value: function() { return {type:"EPSILON"}; } }});
const HASH = Object.create(Atom.prototype, 
                              {name: {enumerable: true, value: "HASH"}, 
                               toString: {value: function() { return "#"; }},
                               toSerializable: {value: function() { return {type:"HASH"}; } }});

//////////////////////////////////
//////////// Actions /////////////
//////////////////////////////////

function Action() { }
Action.equals = function actionEquals(thiz, that) {
  return (thiz.type === that.type && Rule.equals(thiz.rule, that.rule) && thiz.dest == that.dest);
}
Action.equals.toString = function() { return "Action.equals"; }
Action.prototype.toSerializable = function() { return this.toString(true); }
Action.fromSerializable = function (rulesByOldId) { return function(str) {
  if (str === undefined) return undefined;
  var parts = str.split(" ");
  if (parts[0] === "Reduce") return new ReduceAction(rulesByOldId[parts[1]]);
  if (parts[0] === "Shift") return new ShiftAction(parseInt(parts[1]));
  if (parts[0] === "Goto") return new GotoAction(parseInt(parts[1]));
  if (parts[0] === "Accept") return new AcceptAction();
  return null;
} }
function ReduceAction(rule) {
  this.type = "Reduce";
  this.rule = rule;
}
ReduceAction.prototype = Object.create(Action.prototype);
ReduceAction.prototype.toSerializable = function() { return "Reduce " + this.rule.id; }
ReduceAction.prototype.toString = function(hideRule) { 
  if (hideRule)
    return "Reduce " + this.rule.id;
  else
    return "Reduce " + this.rule.id + ":" + this.rule.asString;
}
ReduceAction.prototype.equals = function(that) { return (that instanceof ReduceAction) && (this.rule == that.rule); }
function ShiftAction(dest) {
  this.type = "Shift";
  this.dest = dest;
}
ShiftAction.prototype = Object.create(Action.prototype);
ShiftAction.prototype.toString = function() { return "Shift " + this.dest; }
ShiftAction.prototype.equals = function(that) { return (that instanceof ShiftAction) && (this.dest == that.dest); }
function GotoAction(dest) {
  this.type = "Goto";
  this.dest = dest;
}
GotoAction.prototype = Object.create(Action.prototype);
GotoAction.prototype.toString = function() { return "Goto " + this.dest; }
GotoAction.prototype.equals = function(that) { return (that instanceof GotoAction) && (this.dest == that.dest); }
function AcceptAction() {
  this.type = "Accept";
}
AcceptAction.prototype = Object.create(Action.prototype);
AcceptAction.prototype.toString = function() { return "Accept"; }
AcceptAction.prototype.equals = function(that) { return (that instanceof AcceptAction); }

//////////////////////////////////
///////////// Rules //////////////
//////////////////////////////////

// name :: string
// symbols :: array of Atoms
// lookahead :: Atom or undefined
// position :: Number or undefined
// action :: Function or undefined
function Rule(name, symbols, lookahead, position, action) {
  this.id = "r" + Rule.NextRuleId++;
  this.name = name;
  this.symbols = symbols;
  this.lookahead = lookahead
  this.action = action;
  if (action === undefined)
    this.action = Rule.defaultAction
  this.position = position || 0;
  var s = this.name.toString() + " =>";
  var c = s
  for (var i = 0; i < this.symbols.length; i++) {
    if (i === this.position)
      s += " .";
    s += " " + this.symbols[i].toString();
    c += " " + this.symbols[i].toString();
  }
  this.coreString = c;
  if (this.position === this.symbols.length)
    s += " .";
  if (this.lookahead)
    s = "[" + s + ", " + this.lookahead.toString() + "]";
  this.asString = s;
}
Rule.defaultASTToString = function() {
  var toStr = "(" + this.name; // + "@" + this.startColumn; // + " ";
  for (var i = 0; i < this.kids.length; i++) {
    // if (i === 0)
    //   toStr += this.kids[i].toString(true);
    // else
      toStr += " " + this.kids[i].toString(true);
  }
  toStr += ")";
  return toStr; 
}
Rule.defaultAction = function(kids) {
  var useful_kids = [];
  for (var i = 0; i < kids.length; i++) {
    if (kids[i] instanceof Lit) continue;
    else if (kids[i].shouldInline === true) useful_kids = useful_kids.concat(kids[i].kids);
    else useful_kids.push(kids[i]);
  }
  var start = (kids.length > 0 ? kids[kids.length - 1].startColumn : undefined);
  return { name: this.name, kids: useful_kids, toString: Rule.defaultASTToString, startColumn: start };
}
Rule.defaultAction.toString = function() { return "Rule.defaultAction"; }

Rule.ListCons = function(hd, tl, shouldInline) {
  var ret = function(kids) {
    var useful_kids = [];
    for (var i = 0; i < kids.length; i++) {
      if (kids[i].name === hd) {
        if (kids[i].shouldInline === true)
          useful_kids = useful_kids.concat(kids[i].kids);
        else
          useful_kids.push(kids[i]);
      } else if (kids[i].name === tl) useful_kids = useful_kids.concat(kids[i].kids); 
    }
    var start = (kids.length > 0 ? kids[kids.length - 1].startColumn : undefined);
    return { name: tl, kids: useful_kids, toString: E.Rule.defaultASTToString, startColumn: start,
             shouldInline: shouldInline };
  }
  ret.toString = function() { return "Rule.ListCons(" + JSON.stringify(hd) + ", " + JSON.stringify(tl) + ")"; };
  return ret;
}

Rule.Inline = function(kids) {
  var ret = E.Rule.defaultAction.call(this, kids);
  ret.shouldInline = true;
  return ret;
}
Rule.Inline.toString = function() { return "Rule.Inline"; }

var RuleFactory = {};
RuleFactory.cache = {};
RuleFactory.byId = {};
RuleFactory.make = function(name, symbols, lookahead, pos, action) {
  if (action === undefined)
    action = Rule.defaultAction
  pos = pos || 0;
  var findByName = RuleFactory.cache[name];
  if (findByName) {
    var findBySymbols = findByName[symbols];
    if (findBySymbols) {
      var findByLookahead = findBySymbols[lookahead];
      if (findByLookahead) {
        var findByPos = findByLookahead[pos];
        if (findByPos) {
          var findByAction = findByPos[action];
          if (findByAction) return findByAction;
          else {
            var rule = new Rule(name, symbols, lookahead, pos, action);
            RuleFactory.byId[rule.id] = rule;
            findByPos[action] = rule;
            return rule;
          }
        } else {
          var rule = new Rule(name, symbols, lookahead, pos, action);
          RuleFactory.byId[rule.id] = rule;
          var byAction = {}; byAction[action] = rule;
          findByLookahead[pos] = byAction;
          return rule;
        }
      } else {
        var rule = new Rule(name, symbols, lookahead, pos, action);
        RuleFactory.byId[rule.id] = rule;
        var byAction = {}; byAction[action] = rule;
        var byPos = {}; byPos[pos] = byAction;
        findBySymbols[lookahead] = byPos;
        return rule;
      }
    } else {
      var rule = new Rule(name, symbols, lookahead, pos, action);
      RuleFactory.byId[rule.id] = rule;
      var byAction = {}; byAction[action] = rule;
      var byPos = {}; byPos[pos] = byAction;
      var byLookahead = {}; byLookahead[lookahead] = byPos;
      findByName[symbols] = byLookahead;
      return rule;
    }
  } else {
    var rule = new Rule(name, symbols, lookahead, pos, action);
    RuleFactory.byId[rule.id] = rule;
    var byAction = {}; byAction[action] = rule;
    var byPos = {}; byPos[pos] = byAction;
    var byLookahead = {}; byLookahead[lookahead] = byPos;
    var bySymbols = {}; bySymbols[symbols] = byLookahead;
    RuleFactory.cache[name] = {}; RuleFactory.cache[name] = bySymbols;
    return rule;
  }
}
Rule.fromSerializable = function(rulesByOldId, id) {
  var obj = rulesByOldId[id];
  if (obj.hasOwnProperty("like")) {
    var base = Rule.fromSerializable(rulesByOldId, obj.like);
    var lookahead = obj.lookahead;
    return RuleFactory.make(base.name, base.symbols, Atom.fromSerializable(lookahead), obj.position, base.action);
  } else {
    var sym = [];
    sym.length = obj.symbols.length;
    for (var i = 0; i < obj.symbols.length; i++) {
      sym[i] = Atom.fromSerializable(obj.symbols[i]);
    }
    return RuleFactory.make(obj.name, sym, Atom.fromSerializable(obj.lookahead), obj.position, eval(obj.action));
  }
}

Rule.NextRuleId = 0;
Rule.equals = function(thiz, that) {
  if (thiz === that) return true;
  if (thiz === undefined || that === undefined) return false;
  return (thiz.id === that.id) ||
    (thiz.name == that.name && 
     thiz.symbols == that.symbols && 
     thiz.lookahead == that.lookahead && 
     thiz.action == that.action && 
     thiz.position == that.position);
}
Rule.equalsCore = function(thiz, that) {
  if (thiz === that) return true;
  if (thiz === undefined || that === undefined) return false;
  return (thiz.id === that.id) ||
    (thiz.name == that.name && 
     thiz.symbols == that.symbols && 
     thiz.action == that.action && 
     thiz.position == that.position);
}
Rule.prototype = {
  toString: function() { return this.asString; },
  withLookahead: function(lookahead) { 
    return RuleFactory.make(this.name, this.symbols, lookahead, this.position, this.action); 
  },
  toSerializable: function() {
    var ret = {};
    if (this.lookahead) {
      ret.lookahead = this.lookahead.toSerializable();
      ret.like = this.withLookahead(undefined).id;
      return ret;
    }
    ret.name = this.name;
    if (this.action)
      ret.action = this.action.toString();
    ret.position = this.position;
    ret.symbols = [];
    for (var i = 0; i < this.symbols.length; i++)
      ret.symbols[i] = this.symbols[i].toSerializable();
    return ret;
  }
}

//////////////////////////////////
//////////// Grammars ////////////
//////////////////////////////////

function Grammar(name, start) {
  this.name = name;
  this.rules = {};
  this.start = start;
  this.actionTable = [];
  this.gotoTable = [];
  this.acceptStates = [];
  this.atoms = new OrderedSet([EOF, EPSILON], Atom.equals);
  this.tokens = new OrderedSet([EOF, EPSILON], Atom.equals);
  this.nonterms = new OrderedSet([], Atom.equals);
}

Grammar.fromSerializable = function(obj) {
  var g = new Grammar(obj.name, obj.start);
  var rulesByOldId = {};
  for (var id in obj.rulesByOldId) {
    rulesByOldId[id] = Rule.fromSerializable(obj.rulesByOldId, id);
  }
  for (var i = 0; i < obj.rules.length; i++)
    g.addRule(rulesByOldId[obj.rules[i]]);
  g.actionTable = [];
  for (var i = 0; i < obj.actionTable.length; i++) {
    var tableRow = obj.actionTable[i];
    var newRow = g.actionTable[i] = {};
    for (var j = 0; j < g.atoms.size(); j++) {
      var atom = g.atoms.get(i);
      if (atom in tableRow)
        newRow[atom] = OrderedSet.fromSerializable(tableRow[atom], Action.equals, 
                                                   Action.fromSerializable(rulesByOldId))
      else
        newRow[atom] = new OrderedSet([], Action.equals);
    }
  }
  g.gotoTable = [];
  for (var i = 0; i < obj.gotoTable.length; i++) {
    var tableRow = obj.gotoTable[i];
    var newRow = g.gotoTable[i] = {};
    for (var name in tableRow)
      newRow[name] = Action.fromSerializable(rulesByOldId)(tableRow[name])
  }
  g.acceptStates = [];
  for (var i = 0; i < obj.acceptStates.length; i++)
    g.acceptStates[obj.acceptStates[i]] = true;
  g.derivable = {};
  for (var name in obj.derivable) {
    g.derivable[name] = {};
    for (var i = 0; i < obj.derivable[name].length; i++)
      g.derivable[name][obj.derivable[name][i]] = true;
  }
  g.mode = obj.mode;
  return g;
}

Grammar.prototype = {
  resetParser: function() {
    this.actionTable = [];
    this.gotoTable = [];
    this.acceptStates = [];
    delete this.first;
    delete this.nontermFirst;
    delete this.follow;
    delete this.states;
  },
  initializeParser: function(forceGLR) {
    console.log("Initializing " + this.name);
    var new_start;
    if (!this.initialized) {
      new_start = "START";
      while (this.rules.hasOwnProperty(new_start))
        new_start += "_";
      this.addRule(new_start, [new Nonterm(this.start)]);
      this.start = new_start;
      this.initialized = true;
    }
    if (!forceGLR) {
      console.log("Computing first sets");
      this.computeFirstSets();
      console.log("Computing follow sets");
      this.computeFollowSets();
      console.log("Computing states");
      // this.computeStates();
      // var oldStyleTables = this.printTables();
      // var oldStyleStates = "";
      // for (var i = 0; i < this.states.size(); i++)
      //   oldStyleStates += "State " + i + ": " + this.states.get(i).toString(true, true) + "\n";
      // this.resetParser();
      // this.computeFirstSets();
      // this.computeFollowSets();
      this.computeStateKernels();
      this.computeActions();
      // var newStyleTables = this.printTables();
      // var newStyleStates = "";
      // for (var i = 0; i < this.states.size(); i++)
      //   newStyleStates += "State " + i + ": " + this.completeClosure(this.states.get(i)).toString(true, true) + "\n";
      // if (oldStyleStates != newStyleStates) {
      //   console.log("LALR Discrepancies in state sets between two approaches:");
      //   console.log("Old approach:");
      //   console.log(oldStyleStates);
      //   console.log("New approach:");
      //   console.log(newStyleStates);
      //   throw("failure");
      // }
      // if (oldStyleTables != newStyleTables) {
      //   console.log("LALR Discrepancies in action tables between two approaches:");
      //   console.log("Old approach:");
      //   console.log(oldStyleTables);
      //   console.log("New approach:");
      //   console.log(newStyleTables);
      //   throw("failure");
      // }
      this.mode = "LALR";
      console.log("Checking for ambiguity in " + this.name);
    }
    var ambiguity = forceGLR || this.checkForLALRAmbiguity()
    if (forceGLR === true || ambiguity.length > 0) {
      var orig_start = this.rules[this.start][0].symbols[0];
      this.rules[this.start] = []
      this.addRule(this.start, [orig_start, EOF]);
      console.log("Computing first sets");
      this.resetParser();
      this.computeFirstSets();
      console.log("Computing follow sets");
      this.computeFollowSets();
      console.log("Computing derivability");
      this.computeDerivability();
      console.log("Sorting nonterminals");
      this.topoSortNonterms();
      console.log("Computing states");
      delete this.gotoTable;
      this.computeStateKernels();
      this.computeActions();
      // var newGotoStatesStr = JSON.stringify(this.gotoTable, null, "  ");
      // newStyleTables = this.printTables();
      // newStyleStates = "";
      // for (var i = 0; i < this.states.size(); i++)
      //   newStyleStates += "State " + i + ": " + this.completeClosure(this.states.get(i)).toString(true, true) + "\n";
      // this.resetParser();
      // this.computeFirstSets();
      // this.computeFollowSets();
      // this.computeStates();
      // var oldGotoTableStr = JSON.stringify(this.gotoTable, null, "  ");
      // oldStyleTables = this.printTables();
      // oldStyleStates = "";
      // for (var i = 0; i < this.states.size(); i++)
      //   oldStyleStates += "State " + i + ": " + this.states.get(i).toString(true, true) + "\n";
      // if (oldStyleStates != newStyleStates) {
      //   console.log("GLR Discrepancies in state sets between two approaches:");
      //   console.log("Old approach:");
      //   console.log(oldStyleStates);
      //   console.log("New approach:");
      //   console.log(newStyleStates);
      //   throw("failure 1");
      // }
      // if (oldGotoTableStr != newGotoStatesStr) {
      //   console.log("GLR Discrepancies in state sets between two approaches:");
      //   console.log("Old approach:");
      //   console.log(oldGotoTableStr);
      //   console.log("New approach:");
      //   console.log(newGotoStatesStr);
      //   throw("failure 2");
      // }
      // if (oldStyleTables != newStyleTables) {
      //   console.log("GLR Discrepancies in action tables between two approaches:");
      //   console.log("States:");
      //   console.log(newStyleStates);
      //   console.log("Old approach:");
      //   console.log(oldStyleTables);
      //   console.log("New approach:");
      //   console.log(newStyleTables);
      //   throw("failure 3");
      // }
      this.mode = "GLR";
    } else {
      console.log("No ambiguity, sticking to LALR mode");
      console.log(this.printTables());
    }
    console.log("Done initializing " + this.name);
  },
  parse: function(token_stream) {
    if (this.mode === "LALR")
      return this.parseLALR(token_stream);
    else if (this.mode === "GLR")
      return this.parseGLR(token_stream);
    return "Unknown parsing mode -- can't actually parse!"
  },
  toString: function() {
    var s = "Grammar " + this.name + ": (initial rule " + this.start + ")\n";
    for (var name in this.rules) {
      var rules = this.rules[name];
      for (var i = 0; i < rules.length; i++)
        s += rules[i].toString() + "\n";
    }
    return s;
  },

  toSerializable: function() {
    var ret = {};
    ret.start = this.start;
    ret.name = this.name;
    ret.acceptStates = [];
    for (var i = 0; i < this.acceptStates.length; i++)
      if (this.acceptStates[i])
        ret.acceptStates.push(i);
    ret.mode = this.mode;
    ret.derivable = {};
    for (var name in this.derivable) {
      ret.derivable[name] = [];
      for (var der in this.derivable[name])
        ret.derivable[name].push(der);
    }
    ret.rulesByOldId = {};
    ret.rules = [];
    for (var name in this.rules) {
      for (var i = 0; i < this.rules[name].length; i++) {
        var rule = this.rules[name][i];
        var id = rule.id;
        ret.rulesByOldId[id] = rule.toSerializable();
        ret.rules.push(id);
        var rule_noLookahead = rule.withLookahead(undefined);
        ret.rulesByOldId[rule_noLookahead.id] = rule_noLookahead.toSerializable();
      }
    }
    ret.actionTable = [];
    for (var i = 0; i < this.actionTable.length; i++) {
      ret.actionTable[i] = {};
      var tableRow = this.actionTable[i];
      for (var name in tableRow) {
        if (tableRow.hasOwnProperty(name) && tableRow[name].size() > 0) {
          ret.actionTable[i][name] = tableRow[name].toSerializable();
          for (var j = 0; j < tableRow[name].size(); j++) {
            var act = tableRow[name].get(j);
            if (act instanceof ReduceAction) {
              ret.rulesByOldId[act.rule.id] = act.rule.toSerializable();
              var rule_noLookahead = act.rule.withLookahead(undefined);
              ret.rulesByOldId[rule_noLookahead.id] = rule_noLookahead.toSerializable();
            }
          }
        }
      }
    }
    ret.gotoTable = [];
    for (var i = 0; i < this.gotoTable.length; i++) {
      ret.gotoTable[i] = {};
      var tableRow = this.gotoTable[i];
      for (var name in tableRow) {
        if (tableRow.hasOwnProperty(name)) {
          var _goto = tableRow[name];
          if (_goto !== undefined)
            _goto = _goto.toSerializable();
          ret.gotoTable[i][name] = _goto;
        }
      }
    }
    return ret;
  },

  addRule: function(name, symbols, action) {
    var new_rule;
    if (name instanceof Rule) {
      new_rule = name;
      name = new_rule.name;
      symbols = new_rule.symbols;
    } else {
      new_rule = RuleFactory.make(name, symbols, undefined, undefined, action);
    }
    if (!(this.rules.hasOwnProperty(name)))
      this.rules[name] = [];
    this.rules[name].push(new_rule);
    this.atoms.add(new Nonterm(name));
    this.nonterms.add(new Nonterm(name));
    for (var i = 0; i < symbols.length; i++) {
      this.atoms.add(symbols[i]);
      if (symbols[i] instanceof Nonterm)
        this.nonterms.add(symbols[i])
      else
        this.tokens.add(symbols[i])
    }
    return new_rule;
  },

  //////////////////////////////////
  // Computes what nonterminals can derive other nonterminals in one or more steps,
  // i.e. A ->+ B for two nonterminals A and B (possibly the same)
  //////////////////////////////////
  computeDerivability: function() {
    this.derivable = {}
    for (var i = 0; i < this.nonterms.size(); i++)
      this.derivable[this.nonterms.get(i)] = {};
    var changed = true;
    while(changed) {
      changed = false;
      // console.log("\nNew loop...");
      for (var name in this.rules) {
        var rules = this.rules[name]
        for (var i = 0; i < rules.length; i++) {
          var rule = rules[i];
          // console.log("Processing rule [" + rule + "]");
          if (rule.symbols.length === 0) {
            // if (!this.derivable[name][EPSILON])
            //   console.log("  " + name + " can derive EPSILON via rule " + rule);
            changed = changed || (!this.derivable[name][EPSILON]);
            this.derivable[name][EPSILON] = true; // Rule <name> is nullable
          }
          for (var j = 0; j < rule.symbols.length; j++) {
            var sym = rule.symbols[j]
            if ((sym instanceof Token) && (sym !== EOF)) {
              // console.log("  Rule [" + rule + "] isn't nullable due to " + sym + " so skipping rest");
              break; // A token in the RHS of a rule means it can't derive a nonterminal on its own
            }
            var restNullable = true;
            for (k = j + 1; k < rule.symbols.length; k++) {
              if (rule.symbols[k] === EOF) continue;
              if ((rule.symbols[k] instanceof Token) || 
                  (this.derivable[rule.symbols[k]][EPSILON] !== true)) {
                restNullable = false;
                break;
              }
            }
            if (restNullable) {
              // if (!this.derivable[name][sym])
              //   console.log("  " + name + " can derive " + sym + " via [" + rule + "]");
              changed = changed || (!this.derivable[name][sym]);
              this.derivable[name][sym] = true;
            }
            if ((!this.derivable[sym]) || this.derivable[sym][EPSILON] !== true) {
              // console.log("Rule [" + rule + "] isn't nullable after " + sym + " so skipping rest");
              break; // This nonterminal isn't nullable, so skip the rest of the rule
            }
          }
        }
      }

      for (var i = 0; i < this.nonterms.size(); i++) {
        var term_i = this.nonterms.get(i);
        for (var j = 0; j < this.nonterms.size(); j++) {
          var term_j = this.nonterms.get(j);
          if (i == j || !this.derivable[term_i][term_j]) continue;
          if (this.derivable[term_j][EPSILON] && !this.derivable[term_i][EPSILON]) {
            // console.log(term_i + " can derive EPSILON via " + term_j);
            this.derivable[term_i][EPSILON] = true;
          }
          for (var k = 0; k < this.nonterms.size(); k++) {
            var term_k = this.nonterms.get(k);
            if (j == k || !this.derivable[term_j][term_k]) continue;
            // if (!this.derivable[term_i][term_k])
            //   console.log(term_i + " can derive " + term_k + " via " + term_j);
            changed = changed || (!this.derivable[term_i][term_k])
            this.derivable[term_i][term_k] = true;
          }
        }
      }
    }
    // console.log("Done");
  },
  nontermOrdinals: {},
  topoSortNonterms: function() {
    var index = this.nonterms.size();
    const thiz = this;
    function help(nonterm) {
      for (var name in thiz.derivable[nonterm]) {
        if (!thiz.nontermOrdinals.hasOwnProperty(name) && name !== nonterm)
          help(name);
      }
      if (!thiz.nontermOrdinals.hasOwnProperty(nonterm))
        thiz.nontermOrdinals[nonterm] = index--;
    }
    help(this.start);
    for (var i = 0; i < this.nonterms.size(); i++)
      help(this.nonterms.get(i));
  },

  //////////////////////////////////
  // Computes the set of first tokens in a rule
  // If a rule is nullable, then EPSILON is one possible token
  //////////////////////////////////
  computeFirstSets: function() {
    var thiz = this;
    var changed = true;
    function addFirst(name, token) {
      var ret = !(token.toString() in thiz.first[name])
      thiz.first[name][token.toString()] = token;
      return ret;
    }
    function merge(dest, source) {
      var ret = false;
      for (var tok in thiz.first[source])
        ret = addFirst(dest, thiz.first[source][tok]) || ret;
      return ret;
    }
    this.first = {};
    this.nontermFirst = {};
    for (var name in this.rules) {
      if (this.rules.hasOwnProperty(name)) {
        this.first[name] = {};
        this.nontermFirst[name] = new OrderedSet([], Atom.equals);
        for (var i = 0; i < this.rules[name].length; i++)
          if (this.rules[name][i].symbols.length == 0)
            addFirst(name, EPSILON);
      }
    }
    while (changed) {
      changed = false;
      for (var name in this.rules) {
        this.nontermFirst[name].add(name);
        if (this.rules.hasOwnProperty(name)) {
          var name_rules = this.rules[name];
          for (var i = 0; i < name_rules.length; i++) {
            var name_rule = name_rules[i];
            for (var j = 0; j < name_rule.symbols.length; j++) {
              if (name_rule.symbols[j] instanceof Nonterm) {
                changed = merge(name, name_rule.symbols[j]) || changed;
                this.nontermFirst[name].merge(this.nontermFirst[name_rule.symbols[j]]);
                if (this.first[name_rule.symbols[j]][EPSILON] !== true)
                  break;
              } else {
                changed = addFirst(name, name_rule.symbols[j]) || changed;
                break;
              }
            }
          }
        }
      }
    }
    for (var name in this.first) {
      for (var i in this.first[name]) {
        assert.ok(this.first[name][i] instanceof Atom,
          "This.first[" + name + "][" + i + "] = " + this.first[name][i]);
      }
    }
  },

  //////////////////////////////////
  // Computes the follow sets for a rule: all possible tokens 
  // that can immediately follow a nonterminal
  // Following rules from http://www.cs.uaf.edu/~cs331/notes/FirstFollow.pdf
  // NOTE: This function isn't currently used
  computeFollowSets: function() {
    this.follow = {};
    var thiz = this;
    function addFollow(name, token) {
      var ret = !(token.toString() in thiz.follow[name])
      thiz.follow[name][token.toString()] = token;
      return ret;
    }
    function merge(dest, source, skipEpsilon) {
      var ret = false;
      for (var tok in source)
        if ((!skipEpsilon) || (source[tok] !== EPSILON))
          ret = addFollow(dest, source[tok]) || ret;
      return ret;
    }


    var rule_worklist = new Queue([]);
    for (var name in this.rules) {
      if (this.rules.hasOwnProperty(name)) {
        this.follow[name] = {};
        for (var i = 0; i < this.rules[name].length; i++) {
          rule_worklist.push(this.rules[name][i]);
          rule_worklist[this.rules[name][i]] = 1;
        }
      }
    }
    
    // console.log("Rule 1: Place EOF in FOLLOW(S) where S is the start rule");
    addFollow(this.start, EOF);


    while (rule_worklist.length > 0) {
      var rule_A = rule_worklist.shift();
      var name_A = rule_A.name;
      rule_worklist[rule_A]--;
      // console.log("Processing " + rule_A.toString());
      for (var i = 0; i < rule_A.symbols.length; i++) {
        rule_A_symbol_i = rule_A.symbols[i].toString();
        var changed = false;
        if (rule_A.symbols[i] instanceof Nonterm) {
          if (i === rule_A.symbols.length - 1) {
            // console.log("Rule 3a: if A => aB, then FOLLOW(B) gets everything in FOLLOW(A), "
            //             + "for A = " + name_A + " and B = " + rule_A.symbols[i]);
            changed = merge(rule_A_symbol_i, this.follow[name_A]);
          } else {
            // console.log("Rule 2: if A => aBb, then FOLLOW(B) gets everything in FIRST(b), "
            //             + "where B = " + rule_A_symbol_i + " and b = " + rule_A.symbols[i+1])
            if (rule_A.symbols[i+1] instanceof Nonterm) {
              changed = merge(rule_A_symbol_i, this.first[rule_A.symbols[i+1]], true);
              if (this.first[rule_A.symbols[i+1]][EPSILON]) {
                // console.log("Rule 3b: if A => aBb, where b => EPSILON, then FOLLOW(B) gets everything in FOLLOW(A), "
                //             + "for B = " + rule_A_symbol_i + ", A = " + name_A + " and b = " + rule_A.symbols[i+1]);
                changed = merge(rule_A_symbol_i, this.follow[name_A]) || changed;
              }
            } else {
              changed = addFollow(rule_A_symbol_i, rule_A.symbols[i+1]);
            }
          }
        }
        if (changed) {
          for (var j = 0; j < this.rules[rule_A_symbol_i].length; j++) {
            if (rule_worklist[this.rules[rule_A_symbol_i][j]] == 0) {
              rule_worklist.push(this.rules[rule_A_symbol_i][j]);
              rule_worklist[this.rules[rule_A_symbol_i][j]]++;
            }
          }
        }
      }
    }
  },

  //////////////////////////////////
  // Computes the tokens that can immediately follow the specified position
  // in the current rule.  This is a subset of the Follow set for the given
  // nonterminal, and is therefore more precise and less prone to producing
  // parsing conflicts.
  //////////////////////////////////
  computeFollowAtPosition: function(rule, pos) {
    var ret = new OrderedSet([], Atom.equals);
    var start_pos = (pos !== undefined ? pos : rule.position);
    return this.computeFirstOfStrings(rule.symbols.slice(start_pos), [rule.lookahead]);
    // for (var i = start_pos; i < rule.symbols.length; i++) {
    //   if (rule.symbols[i] instanceof Token) {
    //     ret.add(rule.symbols[i]);
    //     return ret;
    //   } else {
    //     var first = this.first[rule.symbols[i]];
    //     for (var name in first) {
    //       if (first[name] !== EPSILON) {
    //         ret.add(first[name]);
    //       }
    //     }
    //     if (first[EPSILON] === undefined) {
    //       return ret;
    //     }
    //   }
    // }
    // ret.add(rule.lookahead);
    // return ret;
  },

  //////////////////////////////////
  // Lifts the first relation from non-terminals to strings of grammar symbols
  computeFirstOfStrings: function() {
    var ret = new OrderedSet([], Atom.equals);
    var nullable = true;
    for (var i = 0; i < arguments.length; i++) {
      nullable = true;
      var syms_i = arguments[i];
      // console.log("syms[" + i + "] = [" + syms_i + "], length = " + syms_i.length);
      for (var j = 0; j < syms_i.length; j++) {
        if (syms_i[j] instanceof Token || syms_i[j] === HASH) { // Tokens aren't nullable, so we're done
          ret.add(syms_i[j]);
          nullable = false;
          break;
        } else {
          var first = this.first[syms_i[j]];
          if (first === undefined) {
            console.log("WTF? syms_i[j] = " + syms_i[j] + " but first is undefined");
          }
          for (var name in first) {
            if (first[name] !== EPSILON)
              ret.add(first[name]);
          }
          if (first[EPSILON] === undefined) { // This nonterminal isn't nullable, so we're done
            nullable = false;
            break;
          }
        }
      }
      if (!nullable) break;
    }
    if (nullable)
      ret.add(EPSILON);
    return ret;
  },

  //////////////////////////////////
  // Computes the LR(1) closure of a rule set: 
  // For a given rule [A -> a.Bb, x] in the set, it adds [B -> .g, x]
  // for all [B -> g] in the grammar.
  // 
  // If inline = true, then it mutates the provided argument
  // otherwise it constructs a new set and returns that.
  completeClosure: function(rule_set, inline) {
    var ret = inline ? rule_set : new OrderedSet(rule_set);
    var worklist = new Queue(ret.ordered);
    while (worklist.length > 0) {
      var rule = worklist.shift();
      if (rule.position < rule.symbols.length) {
        var next_symbol = rule.symbols[rule.position];
        if (next_symbol instanceof Nonterm) {
          var first = this.computeFollowAtPosition(rule, rule.position + 1);
          // console.log("first(" + rule + ") = " + first);
          for (var j = 0; j < this.rules[next_symbol].length; j++) {
            var rule_to_add = this.rules[next_symbol][j];
            for (var k = 0; k < first.size(); k++) {
              var new_rule = RuleFactory.make(rule_to_add.name, rule_to_add.symbols, first.get(k), 0, rule_to_add.action)
              if (!ret.contains(new_rule)) {
                ret.add(new_rule);
                worklist.push(new_rule);
              }
            }
          }
        }
      }
    }
    return ret;
  },
  
  //////////////////////////////////
  // Computes the LR(1) Goto set for a given rule and symbol:
  // If [A -> a.Xb, x] is in the set, and X is the symbol, then
  // add [A -> aX.b, x] to the output set
  // Finally, compute the closure of it.
  completeGoto: function(rule_set, symbol) {
    // console.log("GOTO(" + symbol + ") for rule_set " + rule_set.toString());
    var ret = new OrderedSet([], Rule.equals);
    for (var i = 0; i < rule_set.size(); i++) {
      var rule = rule_set.get(i);
      if (rule.position < rule.symbols.length && rule.symbols[rule.position].toString() == symbol) {
        var new_rule = RuleFactory.make(rule.name, rule.symbols, rule.lookahead, rule.position + 1, rule.action);
        // console.log("Pushing " + symbol + " over in rule " + rule + " ==> " + new_rule);
        ret.add(new_rule);
      }
    }
    // console.log("After pushing dot, new state has size " + ret.size() + " and is " + ret.toString(true));
    this.completeClosure(ret, true);
    // console.log("After closure, new state has size " + ret.size() + " and is " + ret.toString(true));



    // var kernel = this.computeGotoKernel(rule_set, symbol);
    // var complete = this.completeClosure(kernel);
    // var equal_sets = OrderedSet.equals(complete, ret);
    // console.log("Are completed kernels and this set equal? " + equal_sets);
    // if (!equal_sets) {
    //   console.log("Rule_set = " + rule_set + " and symbol = " + symbol);
    //   console.log("Kernel = " + kernel);
    //   console.log("Completed kernel = " + complete);
    //   console.log("ret = " + ret);
    // }
    return ret;
  },

  //////////////////////////////////
  // Computes the kernel of the LR(1) Goto set for a given kernel and symbol
  // Dragon book, p241
  computeGotoKernel: function(i, rule_set, symbol) {
    // console.log("--> Rule_set #" + i + " = " + rule_set + ", symbol = " + symbol);
    var ret = new OrderedSet([], Rule.equals);
    for (var i = 0; i < rule_set.size(); i++) {
      var rule = rule_set.get(i);
      // console.log("    Processing rule " + rule);
      if (rule.position < rule.symbols.length) {
        if (rule.symbols[rule.position].toString() == symbol) {
          var new_rule = RuleFactory.make(rule.name, rule.symbols, undefined, rule.position + 1, rule.action);
          // console.log("      Pushing " + symbol + " over in rule " + rule + " ==> " + new_rule);
          ret.add(new_rule);
        }
        if (rule.symbols[rule.position] instanceof Nonterm) {
          var C = rule.symbols[rule.position];
          var ntFirst = this.nontermFirst[C];
          // console.log("      nontermFirst[" + C + "] = " + ntFirst.toString());
          // console.log("      first[" + C + "] = " + JSON.stringify(this.first[C]));
          for (var idx = 0; idx < ntFirst.size(); idx++) {
            var a = ntFirst.get(idx);
            var rules_a = this.rules[a];
            for (var j = 0; j < rules_a.length; j++) {
              var rule_a = rules_a[j];
              // console.log("      Rule " + rule + " can derive " + a + " so examining " + rule_a + " for "+ symbol);
              if (rule_a.symbols.length > 0 && rule_a.symbols[0].toString() == symbol) {
                // console.log("      Adding " + rule_a);
                var new_rule = RuleFactory.make(rule_a.name, rule_a.symbols, undefined, 1, rule_a.action);
                ret.add(new_rule);
              }
            }
          }
        }
      }
    }
    // console.log("<-- Result = " + ret);
    return ret;
  },

  computeActions: function() {
    const thiz = this;
    function initTables(index) {
      if (thiz.actionTable[index] === undefined) {
        thiz.actionTable[index] = {};
        for (var k = 0; k < thiz.tokens.size(); k++)
          if (thiz.actionTable[index][thiz.tokens.get(k)] === undefined)
            thiz.actionTable[index][thiz.tokens.get(k)] = new OrderedSet([], Action.equals);
      }
      if (thiz.gotoTable[index] === undefined) {
        thiz.gotoTable[index] = {};
        for (var k = 0; k < thiz.nonterms.size(); k++)
          if (thiz.gotoTable[index][thiz.nonterms.get(k)] === undefined)
            thiz.gotoTable[index][thiz.nonterms.get(k)] = undefined;
      }
    }

    // console.log("Goto Table = ");
    // console.log(JSON.stringify(this.gotoTable, null, "  "));
    for (var i = 0; i < this.states.size(); i++) {
      var state_i = this.states.get(i);
      initTables(i);
      var full_state = this.completeClosure(state_i);
      for (var j = 0; j < this.atoms.size(); j++) {
        var _goto = this.gotoTable[i][this.atoms.get(j)];
        if (_goto) {
          if (this.atoms.get(j) instanceof Token) {
            delete this.gotoTable[i][this.atoms.get(j)]; // This shouldn't be a Goto, it should be a Shift
            this.actionTable[i][this.atoms.get(j)].add(new ShiftAction(_goto.dest));
          } else {
            // Done already
            // this.gotoTable[i][this.atoms.get(j)] = new GotoAction(state_num);
          }
        } // else
          // console.log("State " + i + " doesn't have a goto for token " + this.atoms.get(j))
        for (var k = 0; k < full_state.size(); k++) {
          var item = full_state.get(k);
          if (item.position == item.symbols.length) {
            if (item.name == this.start) {
              this.actionTable[i][EOF].add(new AcceptAction());
              this.acceptStates[i] = true;
            } else {
              this.actionTable[i][item.lookahead].add(new ReduceAction(item));
            }
          }
        }
      }
    }
  },


    //   // Add a shift action on token if there's a rule with the dot preceding it, and
    //   // goto the state in the goto table for that token
    //   for (var k = 0; k < this.tokens.size(); k++) {
    //     var token = this.token.get(k);
    //     for (j = 0; j < state_i.size(); j++) {
    //       var rule_j = state_i.get(j);
    //       if (rule_j.position < rule_j.symbols.length &&
    //           this.computeFirstOfStrings(rule_j.symbols.slice(rule_j.position)).contains(token)) {
    //         var gotoNum = this.gotoTable[i][token].dest;
    //         this.actionTable[i][token].add(new ShiftAction(gotoNum));
    //       }
    //     }
    //   }
    //   // Add A -> epsilon if A -> epsilon exists and
    //   // If there's a rule [B -> g.Cd, b] such that C =>* An, and a in FIRST(ndb), then
    //   for (var k = 0; k < this.nonterms.size(); k++) {
    //     var A = this.nonterms.get(k);
    //     var eps_A = undefined;
    //     for (var l = 0; l < this.rules[A].length; l++)
    //       if (this.rules[A][l].symbols.length === 0) {
    //         eps_A = this.rules[A][l];
    //         break;
    //       }
    //     if (eps_A !== undefined) {
    //       for (var j = 0; j < state_i.size(); j++) {
    //         var rule_j = state_i.get(j); // candidate [B -> g.Cd, b]
    //         if (rule_j.position < rule_j.symbols.length) {
    //           var C = rule_j.symbols[rule_j.position];
    //           if (C instanceof Nonterm && this.nontermFirst[C] && this.nontermFirst[C][A]) {
    //             if this.computeFirstOfStrings(rule_j
    //           }
    //         }
    //       }
    //     }
    //   }
    //   for (var j = 0; j < state_i.size(); j++) {
    //     var rule_j = state_i.get(j);
    //     var rule_j_eps = undefined;
    //     for (var k = 0; k < this.rules[rule_j.name].length; k++)
    //       if (this.rules[rule_j.name][k].symbols.length === 0) {
    //         rule_j_eps = this.rules[rule_j.name][k];
    //         break;
    //       }
    //     if (rule_j.position === rule_j.symbols.length) {
    //       if (rule_j.name === this.start) {
    //         this.actionTable[i][EOF].add(new AcceptAction());
    //         this.acceptStates[i] = true;
    //       } else {
    //         this.actionTable[i][rule_j.lookahead].add(new ReduceAction(rule_j));
    //       }
    //     }
    //   }

  //////////////////////////////////
  // Algorithm 4.13 (p242-243) in Dragon book
  computeStateKernels: function() {
    var init_rule = this.rules[this.start][0].withLookahead(EOF);
    this.init_set = new OrderedSet([init_rule], Rule.equalsCore);
    var kernelStates = new OrderedSet([this.init_set], OrderedSet.equals);
    this.gotoTable = [];
    // Step 1
    var worklist = new Queue([this.init_set]);
    var state_num = -1;
    while (worklist.length > 0) {
      var set = worklist.shift();
      state_num++;
      this.gotoTable[state_num] = undefined;
      for (var j = 0; j < this.atoms.size(); j++) {
        var new_set = this.computeGotoKernel(state_num, set, this.atoms.get(j));
        if (new_set.size() > 0) {
          var gotoStateNum = undefined;
          if (kernelStates.add(new_set)) {
            gotoStateNum = kernelStates.size() - 1;
            worklist.push(new_set);
          } else {
            gotoStateNum = kernelStates.indexOf(new_set);
          }
          if (this.gotoTable[state_num] === undefined)
            this.gotoTable[state_num] = {}
          this.gotoTable[state_num][this.atoms.get(j)] = new GotoAction(gotoStateNum);
        }
      }
    }
    console.log("Done with step 1");
    // for (var i = 0; i < kernelStates.size(); i++) {
    //   console.log("State " + i + "\n" + kernelStates.get(i));
    // }
    // console.log("Goto table = " + JSON.stringify(this.gotoTable, null, "  "));
    // Step 2
    this.states = [];
    var spontLookaheads = {};
    var propLookaheads = {};
    for (var i = 0; i < kernelStates.size(); i++) {
      this.states.push(new OrderedSet([], Rule.equals));
      spontLookaheads[i] = {};
      propLookaheads[i] = {};
    }
    init_rule = this.rules[this.start][0];
    spontLookaheads[0][init_rule.id] = new OrderedSet([EOF], Atom.equals);
    var closureCache = {}
    for (var i = 0; i < kernelStates.size(); i++) {
      for (var j = 0; j < this.atoms.size(); j++) {
        this.applyLookaheads(closureCache, spontLookaheads, propLookaheads, kernelStates.get(i), i, this.atoms.get(j));
        // console.log("i = " + i + " j = " + this.atoms.get(j));
      }
    }
    console.log("Done with step 2");
    // Step 3
    var allLookaheads = {}
    for (var i = 0; i < kernelStates.size(); i++) {
      allLookaheads[i] = {};
      for (var j in spontLookaheads[i]) {
        // console.log("i = " + i + ", j = " + j + ", spontLookaheads[i][j] = " + spontLookaheads[i][j]);
        allLookaheads[i][j] = new OrderedSet(spontLookaheads[i][j]);
      }
    }
    console.log("Done with step 3");
    // Step 4
    // console.log("\n\n\n");
    // console.log("spontLookaheads = " + JSON.stringify(spontLookaheads, null, "  "));
    // console.log("propLookaheads = " + JSON.stringify(propLookaheads, null, "  "));
    var changed = true;
    var pass = 0;
    while (changed) {
      changed = false;
      pass++;
      for (var i = 0; i < kernelStates.size(); i++) {
        var state_i = kernelStates.get(i);
        var gotoTable_i = this.gotoTable[i]
        for (var gs in gotoTable_i) {
          var gotoState = gotoTable_i[gs].dest;
          for (var j = 0; j < state_i.size(); j++) {
            var rule_j = state_i.get(j).withLookahead(undefined);
            var prop = propLookaheads[i][rule_j.id] ? propLookaheads[i][rule_j.id][gotoState] : undefined;
            var look = allLookaheads[i][rule_j.id];
            if (prop !== undefined && look !== undefined) {
              for (var k = 0; k < prop.size(); k++) {
                var id_k = prop.get(k);
                if (allLookaheads[gotoState][id_k] === undefined)
                  allLookaheads[gotoState][id_k] = new OrderedSet([], Atom.equals);
                changed = allLookaheads[gotoState][id_k].merge(look) || changed;
              }
            }
          }
        }
      }
    }
    console.log("Done with step 4");
    // console.log("allLookaheads = " + JSON.stringify(allLookaheads, null, "  "));
    // console.log("All rules:")
    // for (var r in RuleFactory.byId)
    //   console.log(r + ": " + RuleFactory.byId[r]);
    // Step 5 -- expand
    for (var i = 0; i < kernelStates.size(); i++) {
      var kernelState_i = kernelStates.get(i);
      var state_i = this.states[i];
      for (var j = 0; j < kernelState_i.size(); j++) {
        var rule_j = kernelState_i.get(j).withLookahead(undefined);
        var lookahead = allLookaheads[i][rule_j.id];
        if (lookahead) {
          for (var k = 0; k < lookahead.size(); k++)
            state_i.add(RuleFactory.make(rule_j.name, rule_j.symbols, lookahead.get(k), rule_j.position, rule_j.action));
        }
      }
    }
    this.states = new OrderedSet(this.states, OrderedSet.equals);
  },

  //////////////////////////////////
  // Algorithm 4.12 (p242) in Dragon book
  applyLookaheads: function(closureCache, spont, prop, rule_set, set_num, symbol) {
    if (this.gotoTable[set_num] === undefined) return;
    if (this.gotoTable[set_num][symbol] === undefined) return;
    var goto_set_num = this.gotoTable[set_num][symbol].dest;
    
    for (var i = 0; i < rule_set.size(); i++) {
      var rule = rule_set.get(i).withLookahead(undefined);
      var jPrime_rule = RuleFactory.make(rule.name, rule.symbols, HASH, rule.position, rule.action);
      var jPrime = closureCache[jPrime_rule];
      if (jPrime === undefined)
        jPrime = closureCache[jPrime_rule] = this.completeClosure(new OrderedSet([jPrime_rule], Rule.equals), true);
      for (var j = 0; j < jPrime.size(); j++) {
        var rule_j = jPrime.get(j);
        if (rule_j.position < rule_j.symbols.length && rule_j.symbols[rule_j.position].toString() == symbol) {
          if (rule_j.lookahead !== HASH) {
            var new_rule = RuleFactory.make(rule_j.name, rule_j.symbols, undefined, rule_j.position + 1, rule_j.action);
            if (spont[goto_set_num][new_rule.id] === undefined)
              spont[goto_set_num][new_rule.id] = new OrderedSet([], Atom.equals);
            spont[goto_set_num][new_rule.id].add(rule_j.lookahead);
          } else {
            var new_rule = RuleFactory.make(rule_j.name, rule_j.symbols, undefined, rule_j.position + 1, rule_j.action);
            for (var k = 0; k < rule_set.size(); k++) {
              var rule_k = rule_set.get(k).withLookahead(undefined);
              if (Rule.equalsCore(rule, rule_k)) {
                if (prop[set_num][rule_k.id] === undefined)
                  prop[set_num][rule_k.id] = {};
                if (prop[set_num][rule_k.id][goto_set_num] === undefined)
                  prop[set_num][rule_k.id][goto_set_num] = new OrderedSet([]);
                prop[set_num][rule_k.id][goto_set_num].add(new_rule.id);
              }
            }
          }
        }
      }
    }
  },



  //////////////////////////////////
  // Computes the LALR(1) states:
  // Essentially computes LR(1) states and merges them as soon as possible,
  // and enqueues any new items that were produced, in case they lead to more transitions
  // (This is subtle, and essential for proper execution)
  // Also computes the Action and Goto tables as it computes the states
  // Ultimately, only these two tables are needed to run the parser.
  //////////////////////////////////
  computeStates: function() {
    var init_rule = this.rules[this.start][0].withLookahead(EOF);
    this.init_set = this.completeClosure(new OrderedSet([init_rule], Rule.equals), true);
    this.states = new OrderedSet([this.init_set], OrderedSet.equals);
    this.actionTable = [];
    this.gotoTable = [];
    const thiz = this;
    function initTables(index) {
      if (thiz.actionTable[index] === undefined) {
        thiz.actionTable[index] = {};
        for (var k = 0; k < thiz.tokens.size(); k++)
          if (thiz.actionTable[index][thiz.tokens.get(k)] === undefined)
            thiz.actionTable[index][thiz.tokens.get(k)] = new OrderedSet([], Action.equals);
      }
      if (thiz.gotoTable[index] === undefined) {
        thiz.gotoTable[index] = {};
        for (var k = 0; k < thiz.nonterms.size(); k++)
          if (thiz.gotoTable[index][thiz.nonterms.get(k)] === undefined)
            thiz.gotoTable[index][thiz.nonterms.get(k)] = undefined;
      }
    }

    function findMatchingCore(states, target) {
      function coreSubset(src, tgt) {
        for (var i = 0; i < src.size(); i++) {
          var found = false;
          var cur_src = src.get(i);
          for (var j = 0; j < tgt.size(); j++) {
            var cur_tgt = tgt.get(j);
            if (Rule.equalsCore(cur_src, cur_tgt)) {
              found = true;
              break;
            }
          }
          if (!found)
            return false;
        }
        return true;
      }
      for (var i = 0; i < states.size(); i++) {
        var state = states.get(i);
        if (coreSubset(state, target) && coreSubset(target, state))
          return i;
      }
      return -1;
    }
    
    var worklist = new Queue([]);
    for (var i = 0; i < this.states.size(); i++) {
      worklist.push({i: i, state: this.states.get(i)})
    }
    var count = 0;
    while (worklist.length > 0) {
      count++;
      var temp = worklist.shift();
      var cur_state = temp.state;
      var i = temp.i;
      // console.log("Working on " + i);
      initTables(i);
      for (var j = 0; j < this.atoms.size(); j++) {
        var new_state = this.completeGoto(cur_state, this.atoms.get(j));
        if (new_state.size() > 0) {
          state_num = findMatchingCore(this.states, new_state);
          if (state_num == -1) {
            state_num = this.states.size();
            this.states.add(new_state);
            // console.log("Constructing state #" + state_num + " with " + new_state.size() + " items");
            worklist.push({i: state_num, state: new_state});
          } else { // Merge
            var state_to_merge = this.states.get(state_num)
            new_state.subtract(state_to_merge);
            var addsToCore = false;
            for (var k = 0; k < new_state.size(); k++) {
              var cantFind = true;
              var rule_k = new_state.get(k);
              for (l = 0; l < state_to_merge.size(); l++) {
                if (Rule.equalsCore(rule_k, state_to_merge.get(l))) {
                  cantFind = false;
                  break;
                }
              }
              if (!cantFind) {
                addsToCore = true;
                break;
              }
            }
            if (addsToCore) {
              // console.log("Merging " + new_state.size() + " items into state #" + state_num);
              worklist.push({i: state_num, state: new_state}); // Make sure to re-enqueue the new rules
              // in case any other sets might wind up growing
            }
            state_to_merge.merge(new_state);
            new_state = state_to_merge;
          }
          initTables(state_num);
          if (this.atoms.get(j) instanceof Token) {
            this.actionTable[i][this.atoms.get(j)].add(new ShiftAction(state_num));
          } else {
            this.gotoTable[i][this.atoms.get(j)] = new GotoAction(state_num);
          }
          for (var k = 0; k < new_state.size(); k++) {
            var item = new_state.get(k);
            if (item.position == item.symbols.length) {
              if (item.name == this.start) {
                this.actionTable[state_num][EOF].add(new AcceptAction());
                this.acceptStates[state_num] = true;
              } else {
                this.actionTable[state_num][item.lookahead].add(new ReduceAction(item));
              }
            }
          }
        }
      }
    }
    console.log("Done, count = " + count);
  },


  printTables: function() {
    var ret = "";
    for (var i = 0; i < this.actionTable.length; i++) {
      var str_action = ""
      for (var j = 0; j < this.tokens.size(); j++) {
        action = this.actionTable[i][this.tokens.get(j)]
        if (action && action.size() > 0)
          str_action += "\n    On " + this.tokens.get(j) + ", " + action;
      }
      var str_goto = ""
      for (var j = 0; j < this.nonterms.size(); j++) {
        _goto = this.gotoTable[i][this.nonterms.get(j)]
        if (_goto)
          str_goto += "\n    On " + this.nonterms.get(j) + ", " + _goto;
      }
      var s = "In state #" + i + ":";
      if (this.acceptStates[i])
        s += " (ACCEPT STATE)";
      if (str_action)
        s += "\n  Actions:" + str_action;
      if (str_goto)
        s += "\n  Gotos:" + str_goto;
      if (s !== "") s += "\n";
      ret += s;
    }
    return ret;
  },

  //////////////////////////////////
  // A LALR(1) ambiguity is a state/token pair that has multiple enabled actions
  // Returns a list of warning messages
  //////////////////////////////////
  checkForLALRAmbiguity: function() {
    var ambiguities = []
    for (var i = 0; i < this.actionTable.length; i++) {
      var tableRow = this.actionTable[i];
      for (var name in tableRow) {
        if (tableRow.hasOwnProperty(name)) {
          var actions = tableRow[name];
          if (actions.size() > 1) {
            ambiguities.push("In state #" + i + ", conflicting actions on token " + name + ": " + actions);
          }
        }
      }
    }
    return ambiguities;
  },


  //////////////////////////////////
  // The LALR parsing algorithm
  // Does not explicitly track source locations -- that's left to the semantic action
  // and presumes that tokens come equipped with source locations
  //////////////////////////////////
  parseLALR: function(token_source) {
    var state_stack = [0];
    var op_stack = [];
    var tokensParsed = 0;
    var next_tok = token_source.next();
    while (true) {
      console.log("Parsing token " + next_tok.toString(true));
      console.log("State_stack = [" + state_stack + "]")
      console.log("op_stack = [" + op_stack + "]");
      var actions = this.getActions(state_stack[state_stack.length - 1], next_tok);
      if (actions === null) return null;
      if (actions.size() === 0) {
        console.log("No actions found for state #" + state_stack[state_stack.length - 1] + " and " + next_tok);
        return null;
      }
      var action;
      if (actions.size() > 1) {
        console.log("Conflict in actions on token " + next_tok + ": " + actions.toString());
        return null;
      } else {
        action = actions.get(0);
      }
      if (action.type === "Reduce") {
        var arity = action.rule.symbols.length;
        var ops = op_stack.splice(-arity, arity);
        state_stack.splice(-arity, arity);
        var new_val = action.rule.action(ops);
        op_stack.push(new_val);
        var new_state = this.gotoTable[state_stack[state_stack.length - 1]][action.rule.name].dest;
        state_stack.push(new_state);
      } else if (action.type === "Shift") {
        op_stack.push(next_tok);
        state_stack.push(action.dest);
        tokensParsed++;
        if (token_source.hasNext()) {
          var old_tok = next_tok;
          next_tok = token_source.next();
        } else {
          var expected = new OrderedSet([]);
          var tableRow = this.actionTable[state_stack[state_stack.length - 1]];
          for (var name in tableRow) {
            if (tableRow[name].size() > 0)
              expected.add(name);
          }
          console.log("Expected one of: " + expected);
          return null;
        }
      } else if ((action.type === "Accept") && (!token_source.hasNext()) && (next_tok === EOF)) {
        return op_stack.pop();
      } else {
        console.log("Parse error at token #" + tokensParsed + ": " + next_tok)
        console.log("Current state is " + state_stack[state_stack.length - 1])
        console.log("Current action is " + JSON.stringify(action));
        return null;
      }
    }
  },


  //////////////////////////////////
  // The GLR/Elkhound algorithm
  // Following McPeak '04 Technical Report, with errata from 
  // http://scottmcpeak.com/elkhound/reduceViaPath_bug.html
  //////////////////////////////////
  topmost: [],
  pathQueue: undefined,
  parseGLR: function(token_source) {
    var start = new StackNode(0, 1);
    this.topmost = [start];
    this.pathQueue = new Queue([]);
    this.curColumn = 0;
    while (token_source.hasNext()) {
      var next_token = token_source.next();
      // console.log("\nGLR Parsing token #" + this.curColumn + ": " + next_token.toString(true));
      // console.log("Phase 1: reductions");
      this.doReductions(next_token);
      // console.log("Phase 3: shifts");
      this.doShifts(next_token);
      this.curColumn++;
    }
    
    // Assumes that the main rule is start : something EOF
    if (this.topmost.length !== 1) {
      console.log("Somehow, didn't parse the start rule correctly, and finished with multiple active parse heads");
      console.log(JSON.stringify(this.topmost));
      return null;
    }
    var last = this.topmost[0];
    if (last.links.length !== 1) {
      console.log("Somehow, the root rule of the grammar didn't parse uniquely");
      return null;
    }
    var lastVal = last.links[0].val;
    if (lastVal.name !== "EOF") {
      console.log("Somehow, we didn't parse the root rule as expected!");
      return null;
    }
    var main = last.links[0].prev;
    if (main.links.length !== 1) {
      console.log("Somehow, the main rule of the grammar wasn't of the form expected");
      return null;
    }
    return main.links[0].val;
  },
  getActions: function(state, t) {
    var tableRow = this.actionTable[state];
    var actions = tableRow[t];
    if ((actions === undefined) || (actions.size() === 0)) {
      if (!(t instanceof Lit) && (t !== EOF)) {
        // console.log("next_tok = " + next_tok.toString(true));
        var new_t = new Lit(t.value);
        actions = tableRow[new_t];
      }
    }
    return actions;
  },
  doReductions: function(t) {
    for (var i = 0; i < this.topmost.length; i++) {
      var current = this.topmost[i];
      var actions = this.getActions(current.state, t);
      if (actions === undefined) continue;
      for (var j = 0; j < actions.size(); j++) {
        var action = actions.get(j);
        if (action instanceof ReduceAction) {
          var rule = action.rule
          var len = rule.symbols.length;
          var thiz = this;
          // console.log("Found an active reduce action: " + action + ", len = " + len);
          // console.log("Current links = " + JSON.stringify(current.links, null, "  "));
          current.forPathsOfLength(len, function(p) {
            // console.log("1. Enqueuing path {" + p.links + ", " + p.leftSib.state + "} for rule " + rule);
            thiz.addToQueue(p, rule);
          });
        }
      }
    }

    // console.log("Phase 2: process worklist");
    //for (var idx = 0; idx < this.pathQueue.length; idx++) {
    while (this.pathQueue.length > 0) {
      var pq = this.pathQueue.shift();
      // console.log("Processing item " + pq.id);
      // console.log("Processing path {" + pq.path.links + ", " + pq.path.leftSib.state + "}")
      this.reduceViaPath(pq.path, pq.rule, t);
    }
  },
  doShifts: function(t) {
    var prevTops = this.topmost;
    this.topmost = [];
    var nothingFound = (prevTops.length > 0);
    var lit_t = undefined;
    for (var i = 0; i < prevTops.length; i++) {
      var current = prevTops[i];
      var cur_t = t;
      var actions = this.actionTable[current.state][cur_t];
      if (((cur_t instanceof Token) && (cur_t !== EOF) & (!(cur_t instanceof Lit))) &&
          ((actions === undefined) || (actions.size() === 0))) {
        if (lit_t === undefined) {
          if (t.value === undefined)
            console.log("Incomplete token? " + t.toString(true));
          lit_t = new Lit(t.value);
          lit_t.pos = t.pos;
        }
        cur_t = lit_t
        actions = this.actionTable[current.state][cur_t];
      }
      if (actions === undefined) continue;
      for (var j = 0; j < actions.size(); j++) {
        var action = actions.get(j);
        if (action instanceof ShiftAction) {
          nothingFound = false;
          var dest = action.dest;
          var rightSib = undefined;
          for (var k = 0; k < this.topmost.length; k++) {
            if (this.topmost[k].state === dest) {
              rightSib = this.topmost[k];
              break;
            }
          }
          if (rightSib === undefined) {
            rightSib = new StackNode(dest, current.determineDepth + 1);
            this.topmost.push(rightSib);
          }
          t.startColumn = this.curColumn;
          this.addLink(current, rightSib, cur_t);
        }
      }
    }
    if (nothingFound) {
      if (t.hasOwnProperty("pos"))
        console.log("Parse error at " + t.pos.toString(true) + ": unexpected token " + t.toString(true));
      else
        console.log("Parse error: unexpected token " + t.toString(true));
      var expected = new OrderedSet([]);
      var tableRow = this.actionTable[current.state];
      for (var name in tableRow) {
        if (tableRow[name].size() > 0)
          expected.add(name);
      }
      console.log("Expected one of: " + expected);
    }
  },
  reduceViaPath: function(path, rule, t) {
    var vals = [];
    for (var i = 0; i < path.links.length; i++) {
      vals[i] = path.links[i].val;
      path.links[i].reduced = true;
    }
    var newSemanticValue = rule.action(vals);
    // console.log("Constructed " + newSemanticValue + "\n");
    var leftSib = path.leftSib;
    var rightSib = undefined;
    var gotoState = this.gotoTable[leftSib.state][rule.name];
    if (gotoState) {
      gotoState = gotoState.dest;
    } else {
      console.log("Couldn't find a goto rule for " + leftSib.state + " and " + rule.name);
      return;
    }
    for (var i = 0; i < this.topmost.length; i++) {
      if (gotoState == this.topmost[i].state) {
        rightSib = this.topmost[i];
        break;
      }
    }
    if (rightSib !== undefined) {
      var link = undefined;
      for (var i = 0; i < rightSib.links.length; i++) {
        if (rightSib.links[i].prev === leftSib) {
          link = rightSib.links[i];
          break;
        }
      }
      if (link !== undefined) {
        if (link.reduced)
          console.log("YYYYYYYYYYYYYYYYYY merging into a link that's already been reduced!")
        // console.log("vvv Merging new value " + newSemanticValue + " into link " 
        //             + leftSib.id + ":" + leftSib.state + " <-- " + rightSib.id + ":" + rightSib.state)
        // console.log("^^^ Link already has value " + link.val)
        link.val = this.mergeAmbiguous(rule.name, link.val, newSemanticValue);
      } else {
        link = this.addLink(leftSib, rightSib, newSemanticValue, rule.name);
        // console.log("### enqueueLimitedReductions needed");
        if (link) this.enqueueLimitedReductions(link, t);
      }      
    } else {
      rightSib = new StackNode(gotoState, leftSib.determineDepth + 1);
      this.addLink(leftSib, rightSib, newSemanticValue, rule.name);
      this.topmost.push(rightSib);
      // Addendum from http://scottmcpeak.com/elkhound/reduceViaPath_bug.html
      var actions = this.getActions(gotoState, t);
      if (actions === undefined) return null;
      for (var i = 0; i < actions.size(); i++) {
        var action = actions.get(i);
        if (action instanceof ReduceAction) {
          var rule = action.rule
          var len = rule.symbols.length;
          var thiz = this;
          rightSib.forPathsOfLength(len, function(p) {
            // console.log("2. Enqueuing path {" + p.links + ", " + p.leftSib.state + "} for rule " + rule);
            thiz.addToQueue(p, rule);
          });
        }
      }
    }
  },
  addToQueue: function(p, rule) {
    var p_start = (p.links.length > 0 ? p.links[p.links.length - 1].val.startColumn : this.curColumn);
    var rule_ordinal = this.nontermOrdinals[rule.name];
    // console.log("p_start = " + p_start + " and rule_ordinal = " + rule_ordinal);
    for (var i = 0; i < this.pathQueue.length; i++) {
      // console.log("pq[" + i + "].startColumn = " + this.pathQueue.get(i).startColumn
      //             + " and rule_ordinal is " + this.nontermOrdinals[this.pathQueue.get(i).rule.name]);
      if (p_start > this.pathQueue.get(i).startColumn ||
          rule_ordinal < this.nontermOrdinals[this.pathQueue.get(i).rule.name]) {
        // console.log("   Enqueuing id " + this.nextQid + " at index " + i);
        this.pathQueue.insertAt(i, {id: this.nextQid++, startColumn: p_start, path: p, rule: rule});
        return;
      }
    }
    // console.log("   Enqueuing id " + this.nextQid + " at end");
    this.pathQueue.push({id: this.nextQid++, startColumn: p_start, path: p, rule: rule});
  },
  nextQid: 0,
  mergeAmbiguous: function(rule_name, old_val, new_val) {
    var start = old_val.startColumn;
    if (new_val.startColumn < start)
      start = new_val.startColumn;
    if (old_val.name === "--CHOICE--" && new_val.name === "--CHOICE--") {
      // console.log("Consolidating " + old_val.kids + " and " + new_val.kids)
      return { name: "--CHOICE--", kids: old_val.kids.concat(new_val.kids), 
               startColumn: start, toString: Rule.defaultASTToString };
    } else if (old_val.name === "--CHOICE--") {
      // console.log("Pushing new kid " + new_val + " onto " + old_val.kids);
      return { name: "--CHOICE--", kids: old_val.kids.concat([new_val]), 
               startColumn: start, toString: Rule.defaultASTToString };
    } else if (new_val.name === "--CHOICE--") {
      // console.log("Pushing old kid " + old_val + " onto " + new_val.kids);
      return { name: "--CHOICE--", kids: new_val.kids.concat([old_val]), 
               startColumn: start, toString: Rule.defaultASTToString };
    } else {
      // console.log("Merging " + old_val + " and " + new_val);
      return { name: "--CHOICE--", kids: [old_val, new_val], 
               startColumn: start, toString: Rule.defaultASTToString };
    }
  },
  addLink: function(leftSib, rightSib, val, name) {
    var link = undefined;
    for (var i = 0; i < rightSib.links.length; i++) {
      if (rightSib.links[i].prev === leftSib) {
        link = rightSib.links[i];
        break;
      }
    }
    if (link === undefined) {
      var link = new Link(leftSib, val);
      rightSib.links.push(link);
      // console.log("Linking " + leftSib.id + ":" + leftSib.state + " <-- " + rightSib.id + ":" + rightSib.state + " with val " + val + "@" + val.startColumn);
      return link;
    } else {
      if (link.reduced)
        console.log("XXXXXXXXXXXXXXXXXXX merging into a link that's already been reduced!")
      link.val = this.mergeAmbiguous(name, link.val, val);
      return null;
    }
  },
  enqueueLimitedReductions: function(link, t) {
    // console.log("** EnqueueLimitedReductions for t = " + t + " and link = " + JSON.stringify(link));
    for (var i = 0; i < this.topmost.length; i++) {
      var n = this.topmost[i];
      var actions = this.getActions(n.state, t);
      if (actions === undefined) continue;
      for (var j = 0; j < actions.size(); j++) {
        var action = actions.get(j);
        if (action instanceof ReduceAction) {
          var rule = action.rule
          var len = rule.symbols.length;
          var thiz = this;
          n.forPathsOfLengthUsing(len, function(p) {
            // var s = "";
            // for (var i = 0; i < p.links.length; i++) {
            //   if (s === "")
            //     s += p.links[i].val + "@" + p.links[i].val.startColumn;
            //   else
            //     s += "," + p.links[i].val + "@" + p.links[i].val.startColumn;
            // }
            // console.log("3. Enqueuing path {" + s + ", " + p.leftSib.state + "} for rule " + rule);
            thiz.addToQueue(p, rule);
          }, link);
        }
      }
    }
  }
}

var allStackNodes = []

function StackNode(state, determineDepth) {
  this.id = StackNode.nextId++;
  this.state = state;
  this.links = [];
  this.determineDepth = determineDepth;
  this.referenceCount = 0;
  allStackNodes.push(this);
}
StackNode.nextId = 0;
StackNode.prototype.forPathsOfLength = function(len, callback) {
  if (len === 0) {
    callback({links: [], leftSib: this}); 
  } else {
    for (var i = 0; i < this.links.length; i++)
      pathLengthHelp(this.links[i], len, [], ["" + this.id + ":" + this.state], callback);
  }
}
StackNode.prototype.forPathsOfLengthUsing = function(len, callback, link) {
  if (len === 0) {
    return; // No way to have zero path-length while using link
  } else {
    var found = false;
    for (var i = 0; i < this.links.length; i++) {
      if (this.links[i] === link) {
        found = true;
        break;
      }
    }
    if (!found) return;
    // console.log("In fPOLUsing for state " + this.id + ":" + this.state
    //             + ", link.prev = " + link.prev.id + ":" + link.prev.state
    //             + ", link.val = " + link.val + "@" + link.val.startColumn
    //             + " and len = " + len);
    var links = [];
    var stack = ["" + this.id + ":" + this.state];
    pathLengthHelp(link, len, links, stack, callback);
  }
}
  
function Link(prev, val) {
  this.prev = prev;
  this.val = val;
  prev.referenceCount++;
}
Link.prototype.toString = function() {
  return this.prev.state + "<--" + this.val;
}

function pathLengthHelp(link, len, links, stack, callback) {
  links[len - 1] = link;
  stack.push("" + link.prev.id + ":" + link.prev.state)
  if (len == 1) { 
    // console.log("Constructed path via [" + stack + "]");
    // console.log("Calling callback with [" + links + "] and leftSib.state " + link.prev.state);
    callback({links: links, leftSib: link.prev}); 
  } else {
    var prev_links = link.prev.links;
    for (var i = 0; i < prev_links.length; i++)
      pathLengthHelp(prev_links[i], len - 1, links.slice(0), stack.slice(0), callback);
  }
}



exports.Atom = Atom
exports.Nonterm = Nonterm
exports.Token = Token
exports.Lit = Lit
exports.Rule = Rule
exports.Grammar = Grammar
exports.OrderedSet = OrderedSet
exports.EOF = EOF
exports.EPSILON = EPSILON
