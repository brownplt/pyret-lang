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
OrderedSet.fromJSON = function(obj, f) {
  var ret = new OrderedSet([], eval(obj.comparison));
  for (var i = 0; i < obj.items.length; i++) {
    if (f)
      ret.add(f(obj.items[i]));
    else
      ret.add(obj.items[i]);
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
OrderedSet.prototype.toString = function(sep_lines) {
  var s = "";
  for (var i = 0; i < this.ordered.length; i++) {
    if (s == "")
      s += "{" + this.ordered[i].toString();
    else
      s += (sep_lines ? ",\n " : ", ") + this.ordered[i].toString();
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
    }
  } else {
    this.elements[key] = [item];
    this.ordered.push(item);
  }
  return this;
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
  for (var i = 0; i < that.ordered.length; i++)
    this.add(that.ordered[i]);
  return this;
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
OrderedSet.prototype.toJSON = function(f) {
  var ret = {items: [], comparison: this.comparison.toString()};
  for (var i = 0; i < this.ordered.length; i++) {
    if (f)
      ret.items[i] = f(this.ordered[i]);
    else
      ret.items[i] = this.ordered[i];
  }
  return ret;
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
Atom.fromJSON = function(obj) {
  if (obj === undefined) return undefined;
  if (obj.type === "Nonterm") return new Nonterm(obj.name);
  if (obj.type === "Token") return new Token(obj.name, obj.value);
  if (obj.type === "Lit") return new Lit(obj.str);
  if (obj.type === "EOF") return EOF;
  if (obj.type === "EPSILON") return EPSILON;
  return null;
}
function Nonterm(name) {
  this.name = name;
}
Nonterm.prototype = Object.create(Atom.prototype);
Nonterm.prototype.toString = function() { return this.name; }
function Token(name, value) {
  this.name = name;
  this.value = value || name;
}
Nonterm.prototype.toJSON = function() { return {type: "Nonterm", name: this.name}; }
Token.prototype = Object.create(Atom.prototype);
Token.prototype.toString = function(showVal) { 
  if (showVal)
    return "('" + this.name + " " + JSON.stringify(this.value) + ")";
  else
    return "'" + this.name; 
}
Token.prototype.toJSON = function() { return {type: "Token", name: this.name, value: this.value}; }
function Lit(str) {
  this.str = str;
  this.asString = '"' + this.str.toString().replace(/[\\"']/g, '\\$&') + '"';
}
Lit.prototype = Object.create(Token.prototype);
Lit.prototype.toString = function() { return this.asString; }
Lit.prototype.toJSON = function() { return {type: "Lit", str: this.str}; }

const EOF = Object.create(Token.prototype, 
                          {name: {enumerable: true, value: "EOF"}, 
                           toString: {value: function() { return "$"; }},
                           toJSON: {value: function() { return {type:"EOF"}; } }});
const EPSILON = Object.create(Atom.prototype, 
                              {name: {enumerable: true, value: "EPSILON"}, 
                               toString: {value: function() { return "ε"; }},
                               toJSON: {value: function() { return {type:"EPSILON"}; } }});

//////////////////////////////////
//////////// Actions /////////////
//////////////////////////////////

function Action() { }
Action.equals = function actionEquals(thiz, that) {
  return (thiz.type === that.type && thiz.rule === that.rule && thiz.dest == that.dest);
}
Action.equals.toString = function() { return "Action.equals"; }
Action.fromJSON = function(obj) {
  if (obj === undefined) return undefined;
  if (obj.type === "Reduce") return new ReduceAction(Rule.fromJSON(obj.rule));
  if (obj.type === "Shift") return new ShiftAction(obj.dest);
  if (obj.type === "Goto") return new GotoAction(obj.dest);
  if (obj.type === "Accept") return new AcceptAction();
  return null;
}
Action.prototype.toJSON = function() { return this; }
function ReduceAction(rule) {
  this.type = "Reduce";
  this.rule = rule;
}
ReduceAction.prototype = Object.create(Action.prototype);
ReduceAction.prototype.toString = function() { return "Reduce " + this.rule.coreString; }
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
    else useful_kids.push(kids[i]);
  }
  var start = (kids.length > 0 ? kids[kids.length - 1].startColumn : undefined);
  return { name: this.name, kids: useful_kids, toString: Rule.defaultASTToString, startColumn: start };
}
Rule.defaultAction.toString = function() { return "Rule.defaultAction"; }

Rule.fromJSON = function(obj) {
  var sym = [];
  sym.length = obj.symbols.length;
  for (var i = 0; i < obj.symbols.length; i++)
    sym[i] = Atom.fromJSON(obj.symbols[i]);
  return new Rule(obj.name, sym, Atom.fromJSON(obj.lookahead), obj.position, eval(obj.action));
}

Rule.NextRuleId = 0;
Rule.equals = function(thiz, that) {
  return (thiz.id === that.id) ||
    (thiz.name === that.name && 
     thiz.symbols === that.symbols && 
     thiz.lookahead === that.lookahead && 
     thiz.action === that.action && 
     thiz.position === that.position);
}
Rule.equalsCore = function(thiz, that) {
  return (thiz.id === that.id) ||
    (thiz.name === that.name && 
     thiz.symbols === that.symbols && 
     thiz.action === that.action && 
     thiz.position === that.position);
}
Rule.prototype = {
  toString: function() { return this.asString; },
  withLookahead: function(lookahead) { 
    return new Rule(this.name, this.symbols, lookahead, this.position, this.action); 
  },
  toJSON: function() {
    var ret = {};
    ret.name = this.name;
    if (this.lookahead)
      ret.lookahead = this.lookahead.toJSON();
    if (this.action)
      ret.action = this.action.toString();
    ret.position = this.position;
    ret.symbols = [];
    for (var i = 0; i < this.symbols.length; i++)
      ret.symbols[i] = this.symbols[i].toJSON();
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

Grammar.fromJSON = function(obj) {
  var g = new Grammar(obj.name, obj.start);
  for (var i = 0; i < obj.rules.length; i++) {
    g.addRule(Rule.fromJSON(obj.rules[i]));
  }
  g.actionTable = [];
  for (var i = 0; i < obj.actionTable.length; i++) {
    var tableRow = obj.actionTable[i];
    var newRow = g.actionTable[i] = {};
    for (var name in tableRow)
      newRow[name] = OrderedSet.fromJSON(tableRow[name], Action.fromJSON)
  }
  g.gotoTable = [];
  for (var i = 0; i < obj.gotoTable.length; i++) {
    var tableRow = obj.gotoTable[i];
    var newRow = g.gotoTable[i] = {};
    for (var name in tableRow)
      newRow[name] = Action.fromJSON(tableRow[name])
  }
  g.acceptStates = obj.acceptStates;
  g.derivable = obj.derivable;
  g.mode = obj.mode;
  return g;
}

Grammar.prototype = {
  initializeParser: function(forceGLR) {
    var new_start;
    if (!this.initialized) {
      new_start = "START";
      while (this.rules.hasOwnProperty(new_start))
        new_start += "_";
      this.addRule(new_start, [new Nonterm(this.start)]);
      this.start = new_start;
      this.initialized = true;
    }
    this.computeFirstSets();
    this.computeFollowSets();
    this.computeStates();
    this.mode = "LALR";
    if (forceGLR || this.checkForLALRAmbiguity().length > 0) {
      var orig_start = this.rules[this.start][0].symbols[0];
      this.rules[this.start] = []
      this.addRule(this.start, [orig_start, EOF]);
      this.computeFirstSets();
      this.computeFollowSets();
      this.computeDerivability();
      this.topoSortNonterms();
      this.computeStates();
      this.mode = "GLR";
    }
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

  toJSON: function() {
    var ret = {};
    ret.start = this.start;
    ret.name = this.name;
    ret.acceptStates = this.acceptStates;
    ret.mode = this.mode;
    ret.derivable = this.derivable;
    ret.rules = [];
    for (var name in this.rules) {
      for (var i = 0; i < this.rules[name].length; i++) {
        ret.rules.push(this.rules[name][i].toJSON());
      }
    }
    ret.actionTable = [];
    for (var i = 0; i < this.actionTable.length; i++) {
      ret.actionTable[i] = {};
      var tableRow = this.actionTable[i];
      for (var name in tableRow) {
        if (tableRow.hasOwnProperty(name))
          ret.actionTable[i][name] = tableRow[name].toJSON(Action.toJSON);
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
            _goto = _goto.toJSON();
          ret.gotoTable[i][name] = _goto;
        }
      }
    }
    return ret;
  },

  addRule: function(name, symbols, lookahead, position, action) {
    var new_rule;
    if (name instanceof Rule) {
      new_rule = name;
      name = new_rule.name;
      symbols = new_rule.symbols;
    } else {
      new_rule = new Rule(name, symbols, lookahead, position, action);
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
      for (var name in this.rules) {
        var rules = this.rules[name]
        for (var i = 0; i < rules.length; i++) {
          var rule = rules[i];
          if (rule.symbols.length === 0) {
            changed = changed || (!this.derivable[name][EPSILON]);
            this.derivable[name][EPSILON] = true; // Rule <name> is nullable
          }
          for (var j = 0; j < rule.symbols.length; j++) {
            var sym = rule.symbols[j]
            if ((sym instanceof Token) && (sym !== EOF))
              break; // A token in the RHS of a rule means it can't derive a nonterminal on its own
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
              changed = changed || (!this.derivable[name][sym]);
              this.derivable[name][sym] = true;
            }
            if ((!this.derivable[sym]) || this.derivable[sym][EPSILON] !== true) {
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
          for (var k = 0; k < this.nonterms.size(); k++) {
            var term_k = this.nonterms.get(k);
            if (j == k || !this.derivable[term_j][term_k]) continue;
            changed = changed || (!this.derivable[term_i][term_k])
            this.derivable[term_i][term_k] = true;
          }
        }
      }
    }
  },
  nontermOrdinals: {},
  topoSortNonterms: function() {
    var index = this.nonterms.size();
    const thiz = this;
    function help(nonterm) {
      for (var name in thiz.derivable[nonterm]) {
        if (!thiz.nontermOrdinals.hasOwnProperty(name))
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
    for (var name in this.rules) {
      if (this.rules.hasOwnProperty(name)) {
        this.first[name] = {};
        for (var i = 0; i < this.rules[name].length; i++)
          if (this.rules[name][i].symbols.length == 0)
            addFirst(name, EPSILON);
      }
    }
    while (changed) {
      changed = false;
      for (var name in this.rules) {
        if (this.rules.hasOwnProperty(name)) {
          var name_rules = this.rules[name];
          for (var i = 0; i < name_rules.length; i++) {
            var name_rule = name_rules[i];
            for (var j = 0; j < name_rule.symbols.length; j++) {
              if (name_rule.symbols[j] instanceof Nonterm) {
                changed = merge(name, name_rule.symbols[j]) || changed;
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


    var rule_worklist = [];
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
    var ret = new OrderedSet([], Rule.equals);
    var start_pos = (pos !== undefined ? pos : rule.position);
    for (var i = start_pos; i < rule.symbols.length; i++) {
      if (rule.symbols[i] instanceof Token) { // Tokens aren't nullable, so we're done
        ret.add(rule.symbols[i]);
        return ret;
      } else {
        var first = this.first[rule.symbols[i]];
        for (var name in first) {
          if (first[name] !== EPSILON) {
            ret.add(first[name]);
          }
        }
        if (first[EPSILON] === undefined) { // This nonterminal isn't nullable, so we're done
          return ret;
        }
      }
    }
    ret.add(rule.lookahead);
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
    var worklist = ret.ordered.slice(0);
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
              var new_rule = new Rule(rule_to_add.name, rule_to_add.symbols, first.get(k), 0, rule_to_add.action)
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
        var new_rule = new Rule(rule.name, rule.symbols, rule.lookahead, rule.position + 1, rule.action);
        // console.log("Pushing " + symbol + " over in rule " + rule + " ==> " + new_rule);
        ret.add(new_rule);
      }
    }
    // console.log("After pushing dot, new state has size " + ret.size() + " and is " + ret.toString(true));
    this.completeClosure(ret, true);
    // console.log("After closure, new state has size " + ret.size() + " and is " + ret.toString(true));
    return ret;
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
    
    var worklist = [];
    for (var i = 0; i < this.states.size(); i++) {
      worklist.push({i: i, state: this.states.get(i)})
    }
    while (worklist.length > 0) {
      var temp = worklist.shift();
      var cur_state = temp.state;
      var i = temp.i;
      initTables(i);
      for (var j = 0; j < this.atoms.size(); j++) {
        var new_state = this.completeGoto(cur_state, this.atoms.get(j));
        if (new_state.size() > 0) {
          state_num = findMatchingCore(this.states, new_state);
          if (state_num == -1) {
            state_num = this.states.size();
            this.states.add(new_state);
            worklist.push({i: state_num, state: new_state});
          } else { // Merge
            var state_to_merge = this.states.get(state_num)
            new_state.subtract(state_to_merge);
            worklist.push({i: state_num, state: new_state}); // Make sure to re-enqueue the new rules
            // in case any other sets might wind up growing
            new_state = state_to_merge.merge(new_state);
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
      // console.log("Parsing token " + next_tok.toString(true));
      // console.log("State_stack = [" + state_stack + "]")
      // console.log("op_stack = [" + op_stack + "]");
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
  pathQueue: [],
  parseGLR: function(token_source) {
    var start = new StackNode(0, 1);
    this.topmost = [start];
    this.pathQueue = [];
    this.curColumn = 0;
    while (token_source.hasNext()) {
      var next_token = token_source.next();
      // console.log("\nGLR Parsing token #" + this.curColumn + ": " + next_token);
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
        actions = tableRow[new Lit(t.value)];
      }
      if ((actions === undefined) || (actions.size() === 0)) {
        console.log("Parse error at token #" + tokensParsed + ", unexpected token " + t);
        var expected = new OrderedSet([]);
        for (var name in tableRow) {
          if (tableRow[name].size() > 0)
            expected.add(name);
        }
        console.log("Expected one of: " + expected);
        return null;
      }
    }
    return actions;
  },
  doReductions: function(t) {
    for (var i = 0; i < this.topmost.length; i++) {
      var current = this.topmost[i];
      var actions = this.getActions(current.state, t);
      if (actions === null) return null;
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
    for (var i = 0; i < prevTops.length; i++) {
      var current = prevTops[i];
      var actions = this.getActions(current.state, t);
      for (var j = 0; j < actions.size(); j++) {
        var action = actions.get(j);
        if (action instanceof ShiftAction) {
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
          this.addLink(current, rightSib, t);
        }
      }
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
    var gotoState = this.gotoTable[leftSib.state][rule.name].dest;
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
      if (actions === null) return null;
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
      // console.log("pq[" + i + "].startColumn = " + this.pathQueue[i].startColumn
      //             + " and rule_ordinal is " + this.nontermOrdinals[this.pathQueue[i].rule.name]);
      if (p_start > this.pathQueue[i].startColumn ||
          rule_ordinal < this.nontermOrdinals[this.pathQueue[i].rule.name]) {
        // console.log("   Enqueuing id " + this.nextQid + " at index " + i);
        this.pathQueue.splice(i, 0, {id: this.nextQid++, startColumn: p_start, path: p, rule: rule});
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
      var actions = this.actionTable[n.state][t];
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
