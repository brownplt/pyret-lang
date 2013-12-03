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
OrderedSet.equals = function(thiz, that) {
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
        (!this.comparison && items[i] == item)) {
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
  




function Atom() {}
Atom.equals = function(thiz, that) {
  if (thiz === that) return true;
  if (thiz instanceof Nonterm && that instanceof Nonterm && thiz.name == that.name) return true;
  if (thiz instanceof Token && that instanceof Token && thiz.name == that.name) return true;
  if (thiz instanceof Lit && that instanceof Lit && thiz.str == that.str) return true;
  return false;
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
Token.prototype = Object.create(Atom.prototype);
Token.prototype.toString = function(showVal) { 
  if (showVal)
    return "'" + this.name + "(" + this.value + ")";
  else
    return "'" + this.name; 
}
function Lit(str) {
  this.str = str;
  this.asString = '"' + this.str.toString().replace(/[\\"']/g, '\\$&') + '"';
}
Lit.prototype = Object.create(Token.prototype);
Lit.prototype.toString = function() { return this.asString; }

const EOF = Object.create(Token.prototype, 
                          {name: {enumerable: true, value: "EOF"}, 
                           toString: {value: function() { return "$"; }}});
const EPSILON = Object.create(Atom.prototype, 
                              {name: {enumerable: true, value: "EPSILON"}, 
                               toString: {value: function() { return "ε"; }}});



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
  if (action === undefined) {
    this.action = function(kids) {
      var rev_kids = [];
      for (var i = kids.length - 1; i >= 0; i--) {
        if (kids[i] instanceof Lit) continue;
        else rev_kids.push(kids[i]);
      }
      var toStr = name + "(";
      for (var i = 0; i < rev_kids.length; i++) {
        if (i === 0)
          toStr += rev_kids[i].toString(true);
        else
          toStr += ", " + rev_kids[i].toString(true);
      }
      toStr += ")";
      return { name: name, kids: rev_kids, toString: function() { return toStr; }};
    }
  }
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
  }
}

function Grammar(name, start) {
  this.name = name;
  this.rules = {};
  this.start = start;
  this.atoms = new OrderedSet([EOF, EPSILON], Atom.equals);
  this.tokens = new OrderedSet([EOF, EPSILON], Atom.equals);
  this.nonterms = new OrderedSet([], Atom.equals);
}
Grammar.prototype = {
  toString: function() {
    var s = "Grammar " + this.name + ": (initial rule " + this.start + ")\n";
    for (var name in this.rules) {
      var rules = this.rules[name];
      for (var i = 0; i < rules.length; i++)
        s += rules[i].toString() + "\n";
    }
    return s;
  },

  addRule: function(name, symbols, lookahead, position, action) {
    if (!(this.rules.hasOwnProperty(name)))
      this.rules[name] = [];
    var new_rule = new Rule(name, symbols, lookahead, position, action);
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
        if (!(this.first[name][i] instanceof Atom)) {
          console.log("This.first[" + name + "][" + i + "] = " + this.first[name][i])
        }
      }
    }
  },

  // Following rules from http://www.cs.uaf.edu/~cs331/notes/FirstFollow.pdf
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
        if ((!skipEpsilon) || source[tok] !== EPSILON)
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


  computeStates: function() {
    var init_rule = this.rules[this.start][0].withLookahead(EOF);
    this.init_set = this.completeClosure(new OrderedSet([init_rule], Rule.equals), true);
    this.states = new OrderedSet([this.init_set], OrderedSet.equals);
    this.actionTable = [];
    this.gotoTable = [];
    const thiz = this;
    function action(type, info) {
      var ret = {type: type}
      if (type === "Reduce") {
        ret.rule = info;
        ret.toString = function() { return "Reduce " + info.coreString; }
      } else if (type === "Accept") {
        ret.toString = function() { return "Accept"; }
      } else {
        ret.dest = info;
        ret.toString = function() { return type + " " + info; }
      }
      return ret;
    }
    function actionEquals(thiz, that) {
      return (thiz.type === that.type && thiz.rule === that.rule && thiz.dest == that.dest);
    }        
    function initTables(index) {
      if (thiz.actionTable[index] === undefined) {
        thiz.actionTable[index] = {};
        for (var k = 0; k < thiz.tokens.size(); k++)
          if (thiz.actionTable[index][thiz.tokens.get(k)] === undefined)
            thiz.actionTable[index][thiz.tokens.get(k)] = new OrderedSet([], actionEquals);
      }
      if (thiz.gotoTable[index] === undefined) {
        thiz.gotoTable[index] = {};
        for (var k = 0; k < thiz.nonterms.size(); k++)
          if (thiz.gotoTable[index][thiz.nonterms.get(k)] === undefined)
            thiz.gotoTable[index][thiz.nonterms.get(k)] = new OrderedSet([], actionEquals);
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
            this.actionTable[i][this.atoms.get(j)].add(action("Shift", state_num));
          } else {
            this.gotoTable[i][this.atoms.get(j)].add(action("Goto", state_num));
          }
          for (var k = 0; k < new_state.size(); k++) {
            var item = new_state.get(k);
            if (item.position == item.symbols.length) {
              if (item.name == this.start) {
                this.actionTable[state_num][EOF].add(action("Accept"));
              } else {
                this.actionTable[state_num][item.lookahead].add(action("Reduce", item));
              }
            }
          }
        }
      }
    }
  },


  printTables: function() {
    for (var i = 0; i < this.states.size(); i++) {
      var str_action = ""
      for (var j = 0; j < this.tokens.size(); j++) {
        action = this.actionTable[i][this.tokens.get(j)]
        if (action && action.size() > 0)
          str_action += "\n    On " + this.tokens.get(j) + ", " + action;
      }
      var str_goto = ""
      for (var j = 0; j < this.nonterms.size(); j++) {
        _goto = this.gotoTable[i][this.nonterms.get(j)]
        if (_goto && _goto.size() > 0)
          str_goto += "\n    On " + this.nonterms.get(j) + ", " + _goto;
      }
      var s = "In state #" + i + ":";
      if (str_action)
        s += "\n  Actions:" + str_action;
      if (str_goto)
        s += "\n  Gotos:" + str_goto;
      console.log(s);
    }
  },

  parseLALR: function(token_source) {
    var state_stack = [0];
    var op_stack = [];
    var tokensParsed = 0;
    var next_tok = token_source.next();
    while (true) {
      // console.log("Parsing token " + next_tok.toString(true));
      // console.log("State_stack = [" + state_stack + "]")
      // console.log("op_stack = [" + op_stack + "]");
      var actions = this.actionTable[state_stack[state_stack.length - 1]][next_tok];
      if (actions.size() == 0 && !next_tok instanceof Lit) {
        actions = this.actionTable[state_stack[state_stack.length - 1]][new Lit(next_tok.value)];
      }
      if (actions.size() === 0) {
        console.log("No actions found for state #" + state_stack[state_stack.length - 1] + " and " + next_tok);
        return null;
      }
      var action;
      if (actions.size() > 1) {
        console.log("Conflict in actions: " + actions.toString());
        action = actions.get(0);
      } else {
        action = actions.get(0);
      }
      if (action.type === "Reduce") {
        var arity = action.rule.symbols.length;
        var ops = op_stack.slice(-arity);
        op_stack.splice(-arity);
        state_stack.splice(-arity);
        var new_val = action.rule.action(ops);
        op_stack.push(new_val);
        var new_state = this.gotoTable[state_stack[state_stack.length - 1]][action.rule.name].get(0).dest;
        state_stack.push(new_state);
      } else if (action.type === "Shift") {
        op_stack.push(next_tok);
        state_stack.push(action.dest);
        tokensParsed++;
        if (token_source.hasNext()) {
          var old_tok = next_tok;
          next_tok = token_source.next();
        } else {
          console.log("Parse error at token #" + tokensParsed + ": needed a new token but none remain")
          console.log("Current state is " + state_stack[state_stack.length - 1])
          console.log("Current action is " + JSON.stringify(action));
          return null;
        }
      } else if (action.type === "Accept" && !token_source.hasNext() && next_tok === EOF) {
        return op_stack.pop();
      } else {
        console.log("Parse error at token #" + tokensParsed + ": " + next_tok)
        console.log("Current state is " + state_stack[state_stack.length - 1])
        console.log("Current action is " + JSON.stringify(action));
        return null;
      }
    }
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
