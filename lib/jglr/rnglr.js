define(["./cyclicJSON"], function(cycle) {
  function SrcLoc(startRow, startCol, startChar, endRow, endCol, endChar) {
    this.startRow = startRow;
    this.startCol = startCol;
    this.startChar = startChar;
    this.endRow = endRow;
    this.endCol = endCol;
    this.endChar = endChar;
  }
  SrcLoc.cache = {};
  SrcLoc.reset = function() { SrcLoc.cache = {}; }
  SrcLoc.make = function(startRow, startCol, startChar, endRow, endCol, endChar) {
    var byStartChar = SrcLoc.cache[startChar];
    if (byStartChar === undefined)
      byStartChar = SrcLoc.cache[startChar] = {};
    var byEndChar = byStartChar[endChar];
    if (byEndChar === undefined)
      byEndChar = byStartChar[endChar] = new SrcLoc(startRow, startCol, startChar, endRow, endCol, endChar);
    return byEndChar;
  }
  SrcLoc.prototype.toString = function(show_span) {
    var ret = this.startRow + ":" + this.startCol;
    if (show_span)
      ret += "-" + this.endRow + ":" + this.endCol;
    return ret;
  }
  SrcLoc.prototype.combine = function(that) {
    if (this.startChar < that.startChar) {
      if (this.endChar < that.endChar) {
        return SrcLoc.make(this.startRow, this.startCol, this.startChar, that.endRow, that.endCol, that.endChar);
      } else {
        return SrcLoc.make(this.startRow, this.startCol, this.startChar, this.endRow, this.endCol, this.endChar);
      }
    } else {
      if (this.endChar < that.endChar) {
        return SrcLoc.make(that.startRow, that.startCol, that.startChar, that.endRow, that.endCol, that.endChar);
      } else {
        return SrcLoc.make(that.startRow, that.startCol, that.startChar, this.endRow, this.endCol, this.endChar);
      }
    }
  }
  SrcLoc.prototype.posAtStart = function() { 
    return SrcLoc.make(this.startRow, this.startCol, this.startChar, this.startRow, this.startCol, this.startChar);
  }
  SrcLoc.prototype.posAtEnd = function() { 
    return SrcLoc.make(this.endRow, this.endCol, this.endChar, this.endRow, this.endCol, this.endChar);
  }

  //////////////////////////////////
  ////////// Various Sets //////////
  //////////////////////////////////
  function IntSet(items) {
    this.elements = {};
    this.ordered = [];
    if (items instanceof IntSet) {
      for (var i = 0; i < items.ordered.length; i++)
        this.add(items.ordered[i]);
    } else if (items) {
      for (var i = 0; i < items.length; i++)
        this.add(items[i]);
    }
  }
  IntSet.prototype.add = function(item) {
    if (this.elements[item] === undefined) {
      this.elements[item] = item;
      this.ordered.push(item);
      return true;
    }
    return false;
  }
  IntSet.prototype.contains = function(item) { return this.elements[item] !== undefined; }
  IntSet.prototype.size = function() { return this.ordered.length; }
  IntSet.prototype.get = function(index) { return this.ordered[index]; }
  IntSet.prototype.toString = function() {
    var items = this.ordered.slice(0);
    items.sort();
    return "{" + JSON.stringify(items) + "}";
  }
  IntSet.prototype.equals = function(that) {
    if (this.size() !== that.size()) return false;
    if (!(that instanceof IntSet)) return false;
    for (var i = 0; i < this.ordered.length; i++)
      if (that.elements[this.ordered[i]] === undefined)
        return false;
    return true;
  }


  function KeyedSet(key, items) {
    this.elements = {};
    this.ordered = [];
    this.uniqueId = "KS" + (++KeyedSet.nextSetId);
    this.byKey = key;
    if (key instanceof KeyedSet) {
      items = key;
      this.byKey = items.byKey;
      for (var i = 0; i < items.ordered.length; i++)
        this.add(items.ordered[i]);
    } else if (items) {
      for (var i = 0; i < items.length; i++)
        this.add(items[i]);
    }
  }
  KeyedSet.nextSetId = 0;
  KeyedSet.equals = function(thiz, that) { return thiz.equals(that); }
  KeyedSet.prototype.add = function(item) {
    var key = item[this.byKey];
    if (this.elements[key] === undefined) {
      this.elements[key] = item;
      this.ordered.push(item);
      return true;
    }
    return false;
  }
  KeyedSet.prototype.size = function() { return this.ordered.length; }
  KeyedSet.prototype.contains = function(item) { return this.elements[item[this.byKey]] !== undefined; }
  KeyedSet.prototype.equals = function(that) {
    if (this.size() !== that.size()) return false;
    if (this.byKey !== that.byKey) return false;
    for (var i = 0; i < this.ordered.length; i++)
      if (that.elements[this.ordered[i][this.byKey]] === undefined)
        return false;
    return true;
  }
  KeyedSet.prototype.get = function(index) { return this.ordered[index]; }
  KeyedSet.prototype.itemForKey = function(key) { return this.elements[key]; }
  KeyedSet.prototype.merge = function(that) {
    var ret = false;
    for (var i = 0; i < that.ordered.length; i++)
      ret = this.add(that.ordered[i]) || ret;
    return ret;
  }
  KeyedSet.prototype.toString = function() {
    var keys = [];
    for (var i = 0; i < this.ordered.length; i++)
      keys.push(this.ordered[i][this.byKey]);
    keys.sort();
    var s = "";
    for (var i = 0; i < keys.length; i++) {
      if (s !== "") s += ", ";
      s += this.elements[keys[i]];
    }
    return "{" + s + "}";
  }

  function LR1Set(rules) {
    this.uniqueId = "LR" + ++LR1Set.nextId;
    this.ruleIdx = {}; // maps rules-as-strings to indices in coreRules
    this.coreRules = []; // array of core rules
    this.core = []; // maps indices to sets of lookahead tokens
    this.noLook = []; // true iff we have an LR0 item in this set
    if (rules instanceof KeyedSet) {
      for (var i = 0; i < rules.size(); i++)
        this.addRule(rules.get(i));
    } else if (rules instanceof Array) {
      for (var i = 0; i < rules.length; i++)
        this.addRule(rules[i]);
    } else if (rules instanceof LR1Set) {
      this.coreRules = rules.coreRules.slice(0);
      this.noLook = rules.noLook.slice(0);
      for (var r in rules.ruleIdx)
        this.ruleIdx[r] = rules.ruleIdx[r];
      for (var i = 0; i < rules.core.length; i++)
        this.core[i] = new KeyedSet(rules.core[i]);
    }
  }
  LR1Set.nextId = 0;
  LR1Set.prototype.exhaustive = function() {
    var ret = [];
    for (var i = 0; i < this.coreRules.length; i++) {
      if (this.noLook[i])
        ret.push(this.coreRules[i]);
      for (var j = 0; j < this.core[i].size(); j++) {
        ret.push(this.coreRules[i].withLookahead(this.core[i].get(j)));
      }
    }
    return ret;
  }
  LR1Set.prototype.coreSize = function() { return this.coreRules.length; }
  LR1Set.prototype.getCore = function(i) { return this.coreRules[i]; }
  LR1Set.prototype.getLookaheads = function(i) { return this.core[i]; }
  LR1Set.prototype.addRule = function(rule) {
    var ret = false;
    var noLookahead = rule.withLookahead(undefined);
    ruleKey = noLookahead.toString();
    var idx = this.ruleIdx[ruleKey];
    if (idx === undefined) {
      ret = true;
      idx = this.ruleIdx[ruleKey] = this.coreRules.push(noLookahead) - 1;
    }
    if (rule.lookahead === undefined) {
      ret = ret || !this.noLook[idx];
      this.noLook[idx] = true;
    } else {
      if (this.core[idx] === undefined)
        this.core[idx] = new KeyedSet("key");
      ret = this.core[idx].add(rule.lookahead) || ret;
    }
    return ret; // Return true if something's been added
  }
  LR1Set.prototype.addRuleWithLookaheads = function(rule, lookaheads) {
    var noLookahead = rule.withLookahead(undefined);
    ruleKey = noLookahead.toString();
    var idx = this.ruleIdx[ruleKey];
    if (idx === undefined) {
      idx = this.ruleIdx[ruleKey] = this.coreRules.push(noLookahead) - 1;
    }
    if (lookaheads.size() === 0) {
      this.noLook[idx] = true;
    } else {
      if (this.core[idx] === undefined)
        this.core[idx] = new KeyedSet("key");
      this.core[idx].merge(lookaheads);
    }
  }
  LR1Set.prototype.toString = function(multiLine) {
    var s = "";
    var sep = (multiLine ? "\n" : ", ");
    var leader = (multiLine ? "  " : "");
    for (var i = 0; i < this.coreRules.length; i++) {
      if (this.noLook[i]) {
        if (s !== "") s += sep;
        s += leader + this.coreRules[i].toString();
      }
      if (this.core[i] !== undefined) {
        if (s !== "") s += sep;
        s += leader + "[" + this.coreRules[i].toString() + ", " + this.core[i].toString() + "]";
      }
    }
    return "{" + (multiLine ? "\n" : "") + s + "}";
  }
  LR1Set.equals = function(thiz, that) { return thiz.equalsCore(that); }
  LR1Set.prototype.equalsCore = function(that) {
    ret = true
    if (this.core.length !== that.core.length) ret = false;
    for (var r in this.ruleIdx)
      if (that.ruleIdx[r] === undefined) ret = false;
    return ret;
  }


  function SetOfSets(items, comparison) {
    this.ordered = [];
    this.comparison = comparison
    if (items instanceof SetOfSets) {
      if (!this.comparison)
        this.comparison = items.comparison;
      for (var i = 0; i < items.ordered.length; i++)
        this.add(items.ordered[i]);
    } else if (items) {
      for (var i = 0; i < items.length; i++)
        this.add(items[i]);
    }
  }
  SetOfSets.prototype.toString = function(sep_lines, sorted) {
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
  SetOfSets.prototype.add = function(item) {
    var key = item.toString();
    if (this.indexOf(item) === -1) {
      this.ordered.push(item);
      return true;
    }
    return false;
  }
  SetOfSets.prototype.indexOf = function(item) {
    return this.indexOfHelp(this.ordered, item);
  }
  SetOfSets.prototype.indexOfHelp = function(items, item) {
    for (var i = 0; i < items.length; i++) {
      if ((this.comparison && this.comparison(items[i], item)) ||
          ((!this.comparison) && (items[i] == item))) {
        return i;
      }
    }
    return -1;
  }  
  SetOfSets.prototype.size = function() { return this.ordered.length; }
  SetOfSets.prototype.get = function(index) { return this.ordered[index]; }

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
  Queue.prototype.debugPrint = function() { 
    console.log("********** Debug print of queue: start = " + this.start + ", end = " + this.end + ", length = " + this.length);
    for (var i = this.start; i < this.end; i++) {
      if (this.end === i) console.log("<<-- END");
      if (this.start === i) console.log("-->> START");
      if (this.items[i].hasOwnProperty("debugPrint"))
        console.log(i + ": " + this.items[i].debugPrint());
      else
        console.log(i + ": " + this.items[i].toString(true));
    }
    if (this.items.length === this.start) console.log("-->> START");
    if (this.items.length === this.end) console.log("<<-- END");
  }


  //////////////////////////////////
  ///////////// Atoms //////////////
  //////////////////////////////////

  function Atom() {}
  Atom.equals = function(thiz, that) {
    if (thiz === that) return true;
    if ((thiz instanceof Nonterm) && (that instanceof Nonterm) && (thiz.name == that.name)) return true;
    if ((thiz instanceof Token) && (that instanceof Token) && (thiz.name == that.name)) return true;
    return false;
  }
  Atom.equals.toString = function() { return "Atom.equals" };
  Atom.fromSerializable = function(obj) {
    if (obj === undefined) return undefined;
    if (obj.type === "Nonterm") return new Nonterm(obj.name);
    if (obj.type === "Token") return new Token(obj.name, obj.value);
    if (obj.type === "EOF") return EOF;
    if (obj.type === "EPSILON") return EPSILON;
    if (obj.type === "HASH") return HASH;
    return null;
  }
  function Nonterm(name) {
    this.name = name;
    this.key = "@" + this.name;
  }
  Nonterm.prototype = Object.create(Atom.prototype);
  Nonterm.prototype.toString = function() { return this.name; }
  function Token(name, value) {
    this.name = name;
    if (value !== undefined)
      this.value = value;
    else
      this.value = name;
    this.key = "'" + this.name + ":" + this.value;
  }
  Nonterm.prototype.toSerializable = function() { return {type: "Nonterm", name: this.name}; }
  Token.prototype = Object.create(Atom.prototype);
  Token.prototype.toString = function(showVal) { 
    if (showVal && this.name !== this.value)
      return "('" + this.name + " " + JSON.stringify(this.value) + ")";
    else
      return "'" + this.name; 
  }
  Token.prototype.toSerializable = function() { 
    if (this.name !== this.value)
      return {type: "Token", name: this.name, value: this.value};
    else
      return {type: "Token", name: this.name};
  }

  const EOF = Object.create(Token.prototype, 
                            {name: {enumerable: true, value: "EOF"}, 
                             toString: {value: function(showVal) { 
                               if (showVal) return "<end of file>";
                               return "$"; 
                             }},
                             key: {enumerable: true, value: "$"},
                             toSerializable: {value: function() { return {type:"EOF"}; } }});
  const EPSILON = Object.create(Atom.prototype, 
                                {name: {enumerable: true, value: "EPSILON"}, 
                                 toString: {value: function() { return "ε"; }},
                                 key: {enumerable: true, value: "ε"},
                                 toSerializable: {value: function() { return {type:"EPSILON"}; } }});
  const HASH = Object.create(Atom.prototype, 
                             {name: {enumerable: true, value: "HASH"}, 
                              toString: {value: function() { return "#"; }},
                              key: {enumerable: true, value: "#"},
                              toSerializable: {value: function() { return {type:"HASH"}; } }});

  //////////////////////////////////
  //////////// Actions /////////////
  //////////////////////////////////

  function Action() { }
  Action.equals = function actionEquals(thiz, that) {
    return (thiz.type === that.type && Rule.equals(thiz.rule, that.rule) && thiz.dest == that.dest);
  }
  Action.equals.toString = function() { return "Action.equals"; }
  function ReduceAction(rule, f) {
    this.type = "Reduce";
    this.rule = rule;
    this.f = f;
    //assert.notEqual(f, undefined, "Bad f value for rule " + rule);
    this.key = rule.id + "#" + f
  }
  ReduceAction.prototype = Object.create(Action.prototype);
  ReduceAction.prototype.toString = function(hideRule) { 
    if (hideRule)
      return "Reduce " + this.rule.id;
    else
      return "Reduce(" + this.rule.id + ":" + this.rule.asString + ", m" + this.rule.position + ", f" + this.f + ")";
  }
  ReduceAction.prototype.equals = function(that) { return (that instanceof ReduceAction) && (this.rule == that.rule); }

  //////////////////////////////////
  ///////////// Rules //////////////
  //////////////////////////////////

  // name :: string
  // symbols :: array of Atoms
  // lookahead :: Atom or undefined
  // position :: Number or undefined
  function Rule(name, symbols, lookahead, position) {
    this.id = Rule.NextRuleId++;
    this.name = name;
    this.symbols = symbols;
    this.lookahead = lookahead
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
  Rule.defaultASTToString = function(show_pos) {
    var toStr = "(" + this.name; 
    if (show_pos)
      toStr += "@" + this.pos.toString(true);
    for (var i = 0; i < this.kids.length; i++) {
      toStr += " " + this.kids[i].toString(true);
    }
    toStr += ")";
    return toStr; 
  }
  Rule.defaultAction = function(kids, curPos, semActions) {
    var useful_kids = [];
    for (var i = 0; i < kids.length; i++) {
      if (kids[i].shouldInline === true) useful_kids = useful_kids.concat(kids[i].kids);
      else useful_kids.push(kids[i]);
    }
    var pos = (kids.length > 0 ? kids[0].pos.combine(kids[kids.length  - 1].pos) : curPos);
    if (semActions !== undefined && semActions[this.name] !== undefined)
      return semActions[this.name](useful_kids, pos);
    else
      return { name: this.name, kids: useful_kids, toString: Rule.defaultASTToString, pos: pos };
  }
  Rule.defaultAction.toString = function() { return "Rule.defaultAction"; }

  Rule.ListCons = function(hd, tl, shouldInline) {
    var ret = function(kids, curPos, ignoredSemActions) {
      var useful_kids = [];
      for (var i = 0; i < kids.length; i++) {
        if (kids[i].name === hd) {
          if (kids[i].shouldInline === true)
            useful_kids = useful_kids.concat(kids[i].kids);
          else
            useful_kids.push(kids[i]);
        } else if (kids[i].name === tl) useful_kids = useful_kids.concat(kids[i].kids); 
      }
      var pos = (kids.length > 0 ? kids[0].pos.combine(kids[kids.length - 1].pos) : curPos);
      return { name: tl, kids: useful_kids, toString: Rule.defaultASTToString, pos: pos,
               shouldInline: shouldInline };
    }
    if (shouldInline) {
      ret.toString = function() { return "Rule.ListCons(" + JSON.stringify(hd) + ", " + JSON.stringify(tl) + ", true)"; }
    }
    else {
      ret.toString = function() { return "Rule.ListCons(" + JSON.stringify(hd) + ", " + JSON.stringify(tl) + ")"; }
    }
    return ret;
  }

  Rule.Inline = function(kids, curPos, semActions) {
    var ret = Rule.defaultAction.call(this, kids, curPos, semActions);
    ret.shouldInline = true;
    return ret;
  }
  Rule.Inline.toString = function() { return "Rule.Inline"; }

  Rule.KeepOnly = function(names, shouldInline) {
    var name_dict = {};
    for (var i = 0; i < names.length; i++)
      name_dict[names[i]] = true;
    var ret = function(kids, curPos, ignoredSemActions) {
      var useful_kids = [];
      for (var i = 0; i < kids.length; i++) {
        if (name_dict[kids[i].name] === true) {
          if (kids[i].shouldInline === true)
            useful_kids = useful_kids.concat(kids[i].kids);
          else
            useful_kids.push(kids[i]);
        }
      }
      var pos = (kids.length > 0 ? kids[0].pos.combine(kids[kids.length - 1].pos) : curPos);
      return { name: this.name, kids: useful_kids, toString: Rule.defaultASTToString, pos: pos, 
               shouldInline: shouldInline };
    }
    if (shouldInline)
      ret.toString = function() { return "Rule.KeepOnly(" + JSON.stringify(names) + ", true)"; };
    else
      ret.toString = function() { return "Rule.KeepOnly(" + JSON.stringify(names) + ")"; };
    return ret;
  }

  var RuleFactory = {};
  RuleFactory.cache = {};
  RuleFactory.byId = {};
  RuleFactory.make = function(name, symbols, lookahead, pos) {
    pos = pos || 0;
    var findByName = RuleFactory.cache[name];
    if (findByName) {
      var findBySymbols = findByName[symbols];
      if (findBySymbols) {
        var findByLookahead = findBySymbols[lookahead];
        if (findByLookahead) {
          var findByPos = findByLookahead[pos];
          if (findByPos) return findByPos;
          else {
            var rule = new Rule(name, symbols, lookahead, pos);
            RuleFactory.byId[rule.id] = rule;
            findByLookahead[pos] = rule;
            return rule;
          }
        } else {
          var rule = new Rule(name, symbols, lookahead, pos);
          RuleFactory.byId[rule.id] = rule;
          var byPos = {}; byPos[pos] = rule;
          findBySymbols[lookahead] = byPos;
          return rule;
        }
      } else {
        var rule = new Rule(name, symbols, lookahead, pos);
        RuleFactory.byId[rule.id] = rule;
        var byPos = {}; byPos[pos] = rule;
        var byLookahead = {}; byLookahead[lookahead] = byPos;
        findByName[symbols] = byLookahead;
        return rule;
      }
    } else {
      var rule = new Rule(name, symbols, lookahead, pos);
      RuleFactory.byId[rule.id] = rule;
      var byPos = {}; byPos[pos] = rule;
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
      var pos = obj.position;
      return RuleFactory.make(base.name, base.symbols, Atom.fromSerializable(lookahead), pos);
    } else {
      var sym = [];
      for (var i = 0; i < obj.symbols.length; i++) {
        sym[i] = Atom.fromSerializable(obj.symbols[i]);
      }
      return RuleFactory.make(obj.name, sym, Atom.fromSerializable(obj.lookahead), obj.position);
    }
  }

  Rule.NextRuleId = 0;
  Rule.equals = function(thiz, that) {
    if (thiz === that) return true;
    if (thiz === undefined || that === undefined) return false;
    if (thiz.id === that.id) return true;
    if (!(thiz.name == that.name && 
          thiz.symbols == that.symbols && 
          thiz.lookahead == that.lookahead && 
          thiz.position == that.position)) return false;
    return true;
  }
  Rule.equalsCore = function(thiz, that) {
    if (thiz === that) return true;
    if (thiz === undefined || that === undefined) return false;
    if (!(thiz.name == that.name && 
          thiz.symbols == that.symbols && 
          thiz.position == that.position)) return false;
    return true;
  }
  Rule.prototype = {
    toString: function() { return this.asString; },
    withPosition: function(pos) {
      return RuleFactory.make(this.name, this.symbols, this.lookahead, pos);
    },
    withLookahead: function(lookahead) { 
      return RuleFactory.make(this.name, this.symbols, lookahead, this.position); 
    },
    withLookaheadPosition: function(lookahead, pos) {
      return RuleFactory.make(this.name, this.symbols, lookahead, pos);
    },
    toSerializable: function() {
      var ret = {};
      if (this.lookahead) {
        ret.lookahead = this.lookahead.toSerializable();
        ret.position = this.position;
        ret.like = this.withLookaheadPosition(undefined, 0).id;
        return ret;
      }
      ret.name = this.name;
      ret.position = this.position;
      ret.symbols = [];
      for (var i = 0; i < this.symbols.length; i++)
        ret.symbols[i] = this.symbols[i].toSerializable();
      return ret;
    }
  }



  function Link(prev, val) {
    this.prev = prev;
    this.val = val;
    this.linkId = Link.NextLinkId++;
  }
  Link.NextLinkId = 0;
  Link.prototype.toString = function() {
    return this.prev.state + "<-(" + this.linkId + (this.reduced ? "r" : "") + ")-" + this.val;
  }



  function ShiftPair(gssNode, state) {
    this.gssNode = gssNode;
    this.state = state;
  }
  ShiftPair.prototype.toString = function() { return "Shift(" + this.gssNode.toString() + ", " + this.state + ")"; }
  function ReductionItem(gssNode, rule, m, f, label) {
    this.gssNode = gssNode;
    this.rule = rule;
    this.m = m;
    this.f = f;
    this.label = label;
  }
  ReductionItem.prototype.toString = function() {
    return "RedItem(" + this.gssNode + ", " + this.rule + ", " + this.m + ", " + this.f + ", " + this.label + ")";
  }
  function GSSNode(label) {
    this.gssId = GSSNode.NextNodeId++;
    this.label = label;
    this.links = [];
    // GSSNode.allNodes.push(this);
  }
  GSSNode.NextNodeId = 0;
  // GSSNode.allNodes = [];
  // GSSNode.reset = function() {
  //   GSSNode.NextNodeId = 0;
  //   GSSNode.allNodes = [];
  // }
  GSSNode.prototype.toString = function() { 
    return "GSSNode(#" + this.gssId + ",s" + this.parentSetId + ":st" + this.label + ")"; 
  }
  GSSNode.prototype.forPathsOfLength = function(len, callback) {
    if (len === 0) {
      callback({labels: [], leftSib: this}); 
    } else {
      //assert(this.links.length > 0, "Can't find any paths of length " + len + " from " + this)
      for (var i = 0; i < this.links.length; i++)
        pathLengthHelp(this.links[i], len, [], [this.toString()], callback);
    }
  }
  function pathLengthHelp(link, len, labels, stack, callback) {
    labels[len - 1] = link.val;
    stack.push(link.prev.toString());
    if (len == 1) { 
      // console.log("Constructed path via [" + stack + "]");
      // console.log("Calling callback with [" + labels + "] and leftSib.state " + link.prev.label);
      callback({labels: labels, leftSib: link.prev}); 
    } else {
      var prev_links = link.prev.links;
      //assert(prev_links.length > 0, "Can't find any paths of length " + len + " from " + this)
      for (var i = 0; i < prev_links.length; i++)
        pathLengthHelp(prev_links[i], len - 1, labels, stack, callback);
    }
    stack.pop();
  }
  


  var allowNull = false;
  function SPPFNode(label, pos) {
    //if (!allowNull)
      //assert(pos !== undefined && pos !== null);
    this.sppfId = SPPFNode.NextId++;
    this.label = label;
    this.pos = pos;
    this.rule = undefined;
    this.kids = undefined;
    this.ambig = undefined;
    // SPPFNode.allNodes.push(this);
  }
  SPPFNode.NextId = 0;
  // SPPFNode.allNodes = [];
  // SPPFNode.reset = function() {
  //   SPPFNode.NextId = 0;
  //   SPPFNode.allNodes = [];
  // }
  SPPFNode.prototype.toString = function() {
    var posString = "null";
    if (this.pos)
      posString = this.pos.toString();
    var ruleAmbig = "";
    if (this.rule)
      ruleAmbig = this.rule.toString();
    else if (this.ambig)
      ruleAmbig = "ambig";
    else if (this.label instanceof Token)
      ruleAmbig = "<Token " + this.label.toString(true) + ">";
    else
      ruleAmbig = "<unknown>";
    return "SPPFNode(" + this.sppfId + ", " + this.label.toString(true) + "@" + posString + " " + ruleAmbig + ")";
  }
  SPPFNode.prototype.addChildren = function(rule, kids, grammar) {
    if (this.kids === undefined && this.ambig === undefined) {
      this.kids = kids;
      this.rule = rule;
    } else if (this.ambig === undefined) {
      this.ambig = [];
      this.ambig.push({kids: this.kids, rule: this.rule}); 
      this.ambig.push({kids: kids, rule: rule});
      delete this.kids; delete this.rule;
    } else {
      var ruleFlags = grammar.getFlags(rule);
      var sortingFlag = grammar.ambiguitySortingFlag;
      if (sortingFlag === undefined || ruleFlags === undefined || ruleFlags[sortingFlag] === undefined) {
        this.ambig.push({kids: kids, rule: rule});
      } else {
        var newFlag = ruleFlags[sortingFlag];
        for (var i = 0; i < this.ambig.length; i++) {
          var flags = grammar.getFlags(this.ambig[i].rule);
          var relevantFlag = flags ? flags[sortingFlag] : undefined;
          if (relevantFlag > newFlag) {
            this.ambig.splice(i, 0, {kids: kids, rule: rule});
            return;
          }
        }
        this.ambig.push({kids: kids, rule: rule});
      }
    }
  }

  //////////////////////////////////
  //////////// Grammars ////////////
  //////////////////////////////////

  function Grammar(name, start) {
    this.name = name;
    this.rules = {};
    this.actions = {};
    this.flags = {};
    this.start = start;
    this.rnTable = [];
    this.acceptStates = [];
    this.atoms = new KeyedSet("key", [EOF, EPSILON]);
    this.tokens = new KeyedSet("key", [EOF, EPSILON]);
    this.nonterms = new KeyedSet("key");
  }

  Grammar.fromSerializable = function(obj) {
    var g = new Grammar(obj.name, obj.start);
    var rulesByOldId = {};
    var actionsByOldId = {};
    var flagsByOldId = {};
    GSSNode.NextNodeId = 0;
    Link.NextLinkId = 0;
    SPPFNode.NextId = 0
    for (var id in obj.rulesByOldId) {
      rulesByOldId[id] = Rule.fromSerializable(obj.rulesByOldId, id);
      actionsByOldId[id] = obj.actionsByOldId[id] ? eval(obj.actionsByOldId[id]) : undefined;
      flagsByOldId[id] = obj.flagsByOldId[id];
    }
    for (var i = 0; i < obj.rules.length; i++) {
      g.addRule(rulesByOldId[obj.rules[i]], actionsByOldId[obj.rules[i]], flagsByOldId[obj.rules[i]]);
    }
    g.rnTable = [];
    for (var i = 0; i < obj.rnTable.length; i++) {
      var tableRow = obj.rnTable[i];
      var newRow = g.rnTable[i] = {};
      for (var j = 0; j < g.atoms.size(); j++) {
        var atom = g.atoms.get(j);
        newRow[atom] = {push: undefined, reductions: new KeyedSet("key")};
        if (atom in tableRow) {
          if (tableRow[atom].accept)
            newRow[atom].accept = true;
          if (tableRow[atom].push) {
            //assert.equal(newRow[atom].push, undefined, "Already have a push action for atom " + atom);
            newRow[atom].push = tableRow[atom].push;
          }
          if (tableRow[atom].reductions) {
            for (var k = 0; k < tableRow[atom].reductions.length; k++) {
              var red = tableRow[atom].reductions[k];
              newRow[atom].reductions.add(new ReduceAction(rulesByOldId[red[0]], red[1]));
            }
          }
        }
      }
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
    g.ambiguitySortingFlag = obj.ambiguitySortingFlag;
    g.mode = obj.mode;
    g.eSPPFs = [];
    g.I = obj.I;
    var Iinv = {};
    for (var name in obj.I)
      Iinv[obj.I[name]] = name;
    allowNull = true;
    for (var i = 0; i < obj.eSPPFs.length; i++) {
      var label = obj.eSPPFs[i].label;
      if (label === "EPSILON")
        label = EPSILON;
      g.eSPPFs[i] = {null: new SPPFNode(label, null)};
    }
    allowNull = false;
    for (var i = 0; i < obj.eSPPFs.length; i++) {
      var src = obj.eSPPFs[i];
      var dest = g.eSPPFs[i].null;
      if (src.kids) {
        var kids = [];
        for (var j = 0; j < src.kids.length; j++)
          kids.push(g.eSPPFs[src.kids[j]].null);
        dest.addChildren(rulesByOldId[src.rule], kids, this);
      }
    }
    return g;
  }



  Grammar.prototype = {
    resetParser: function() {
      this.rnTable = [];
      this.acceptStates = [];
      delete this.first;
      delete this.nontermFirst;
      delete this.states;
      this.firstStringCache = {};
      this.closureCache = {};
    },
    initializeParser: function() {
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
      var orig_start = this.rules[this.start][0].symbols[0];
      console.log("Grammar productions:");
      for (var name in this.rules)
        for (var i = 0; i < this.rules[name].length; i++)
          console.log("  " + this.rules[name][i].toString(true));
      console.log("Computing first sets");
      this.resetParser();
      this.computeFirstSets();
      console.log("Computing derivability");
      this.computeDerivability();
      console.log("Computing states");
      this.computeStateKernels();
      console.log("Computing required nullable parts");
      this.computeRequiredNullableParts();
      console.log("Computing RN Table");
      this.computeRNTable();
      // console.log(this.printTables());
      this.mode = "RNGLR";
      console.log("Done initializing " + this.name);
    },
    parse: function(token_stream) {
      this.resetESPPFs();
      if (this.mode === "RNGLR")
        return this.parseRNGLR(token_stream);
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
    sortAmbiguitiesBy: function(s) { 
      this.ambiguitySortingFlag = s; 
    },
    toSerializable: function() {
      var ret = {};
      ret.start = this.start;
      ret.name = this.name;
      ret.acceptStates = [];
      for (var i = 0; i < this.acceptStates.length; i++)
        if (this.acceptStates[i])
          ret.acceptStates.push(i);
      if (this.ambiguitySortingFlag)
        ret.ambiguitySortingFlag = this.ambiguitySortingFlag;
      ret.mode = this.mode;
      ret.derivable = {};
      for (var name in this.derivable) {
        ret.derivable[name] = [];
        for (var der in this.derivable[name])
          ret.derivable[name].push(der);
      }
      ret.rulesByOldId = {};
      ret.actionsByOldId = {};
      ret.flagsByOldId = {};
      ret.rules = [];
      for (var name in this.rules) {
        for (var i = 0; i < this.rules[name].length; i++) {
          var rule = this.rules[name][i];
          var id = rule.id;
          ret.rulesByOldId[id] = rule.toSerializable();
          ret.rules.push(id);
          var rule_noLookahead = rule.withLookahead(undefined);
          ret.rulesByOldId[rule_noLookahead.id] = rule_noLookahead.toSerializable();
          ret.actionsByOldId[rule_noLookahead.id] = this.getAction(rule_noLookahead);
          if (ret.actionsByOldId[rule_noLookahead.id] !== undefined) {
            ret.actionsByOldId[rule_noLookahead.id] = ret.actionsByOldId[rule_noLookahead.id].toString();
          }
          ret.flagsByOldId[rule_noLookahead.id] = this.getFlags(rule_noLookahead);
        }
      }
      ret.rnTable = []; // TODO: Fix this
      for (var i = 0; i < this.rnTable.length; i++) {
        var tableRow = this.rnTable[i];
        ret.rnTable[i] = {};
        for (var name in tableRow) {
          if (tableRow[name].push === undefined && !tableRow[name].accept && tableRow[name].reductions.size() === 0) {
            //ret.rnTable[i][name] = "empty";
          } else {
            var entry = tableRow[name];
            var dest = ret.rnTable[i][name] = {};
            if (entry.accept) dest.accept = true;
            if (entry.push !== undefined) {
              dest.push = entry.push;
            }
            if (entry.reductions.size() > 0) {
              dest.reductions = [];
              for (var j = 0; j < entry.reductions.size(); j++) {
                var red = entry.reductions.get(j)
                dest.reductions.push([red.rule.id, red.f]);
                ret.rulesByOldId[red.rule.id] = red.rule.toSerializable();
                var rule_noLookahead = red.rule.withLookahead(undefined);
                ret.rulesByOldId[rule_noLookahead.id] = rule_noLookahead.toSerializable();
              }
            }
          }
        }
      }
      ret.I = this.I;
      ret.eSPPFs = [];
      for (var i = 0; i < this.eSPPFs.length; i++) {
        var null_eSPPF = this.eSPPFs[i].null;
        var label = null_eSPPF.label;
        if (label === EPSILON) {
          label = "EPSILON";
        }
        if (null_eSPPF.kids) {
          var kids = [];
          for (var j = 0; j < null_eSPPF.kids.length; j++)
            kids.push(null_eSPPF.kids[j].sppfId);
          if (null_eSPPF.rule)
            ret.eSPPFs[i] = {label: label, kids: kids, rule: null_eSPPF.rule.id};
          else
            ret.eSPPFs[i] = {label: label, kids: kids};
        } else {
          if (null_eSPPF.rule)
            ret.eSPPFs[i] = {label: label, rule: null_eSPPF.rule.id};
          else
            ret.eSPPFs[i] = {label: label};
        }
      }
      return ret;
    },

    addRule: function(name, symbols, action, flags) {
      var new_rule;
      if (name instanceof Rule) {
        new_rule = name;
        name = new_rule.name;
        flags = action; // slide arguments over by one
        action = symbols;
        symbols = new_rule.symbols;
      } else {
        new_rule = RuleFactory.make(name, symbols, undefined, undefined);
      }
      if (action === undefined)
        action = Rule.defaultAction;
      this.actions[new_rule.id] = action;
      this.flags[new_rule.id] = flags;
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

    getAction: function(rule) {
      var baseRule = rule.withLookaheadPosition(undefined, 0);
      var action = this.actions[baseRule.id];
      return action;
    },
    applyAction: function(rule, kids, pos, semActions) {
      var action = this.getAction(rule);
      return action.call(rule, kids, pos, semActions);
    },
    getFlags: function(rule) {
      return this.flags[rule.withLookaheadPosition(undefined, 0).id];
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
              for (var k = j + 1; k < rule.symbols.length; k++) {
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
          if (thiz.first[source][tok] !== EPSILON)
            ret = addFirst(dest, thiz.first[source][tok]) || ret;
        return ret;
      }
      this.first = {};
      this.nontermFirst = {};
      for (var name in this.rules) {
        if (this.rules.hasOwnProperty(name)) {
          this.first[name] = {};
          this.nontermFirst[name] = new KeyedSet("key", [new Nonterm(name)]);
          for (var i = 0; i < this.rules[name].length; i++)
            if (this.rules[name][i].symbols.length === 0)
              addFirst(name, EPSILON);
        }
      }
      while (changed) {
        changed = false;
        for (var name in this.rules) {
          var name_rules = this.rules[name];
          for (var i = 0; i < name_rules.length; i++) {
            var name_rule = name_rules[i];
            var allNullable = true;
            for (var j = 0; j < name_rule.symbols.length; j++) {
              if (name_rule.symbols[j] instanceof Nonterm) {
                changed = merge(name, name_rule.symbols[j]) || changed;
                //if (this.nontermFirst[name_rule.symbols[j]] === undefined)
                  //assert(false, "Couldn't find a nontermFirst for " + name_rule.symbols[j]);
                this.nontermFirst[name].merge(this.nontermFirst[name_rule.symbols[j]]);
                if (this.first[name_rule.symbols[j]][EPSILON] !== EPSILON) {
                  allNullable = false;
                  break;
                }
              } else {
                changed = addFirst(name, name_rule.symbols[j]) || changed;
                allNullable = false;
                break;
              }
            }
            if (allNullable) {
              addFirst(name, EPSILON);
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
      if (rule.lookahead)
        return this.computeFirstOfStrings(rule.symbols.slice(pos).concat(rule.lookahead));
      else
        return this.computeFirstOfStrings(rule.symbols.slice(pos));
    },

    //////////////////////////////////
    // Lifts the first relation from non-terminals to strings of grammar symbols
    computeFirstOfStrings: function(string) {
      var ret = this.firstStringCache[string];
      if (ret !== undefined) return ret;
      ret = this.firstStringCache[string] = new KeyedSet("key");
      var nullable = true;
      // console.log("syms[" + i + "] = [" + string + "], length = " + string.length);
      for (var j = 0; j < string.length; j++) {
        if (string[j] instanceof Token || string[j] === HASH) { // Tokens aren't nullable, so we're done
          ret.add(string[j]);
          nullable = false;
          break;
        } else {
          var first = this.first[string[j]];
          if (first === undefined) {
            console.log("WTF? string[j] = " + string[j] + " but first is undefined");
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
      if (nullable)
        ret.add(EPSILON);
      return ret;
    },

    //////////////////////////////////
    // Computes the LR(1) closure of a rule set: 
    // For a given rule [A -> a.Bb, x] in the set, it adds [B -> .g, y]
    // for all [B -> g] in the grammar, and all y in FIRST(bx).
    // 
    // If inline = true, then it mutates the provided argument
    // otherwise it constructs a new set and returns that.
    // Dragon book 2ed, p261
    completeClosure: function(lrSet, inline) {
      var ret = inline ? lrSet : new LR1Set(lrSet);
      var worklist = new Queue(ret.exhaustive());
      while (worklist.length > 0) {
        var rule = worklist.shift();
        if (rule.position < rule.symbols.length) {
          var next_symbol = rule.symbols[rule.position];
          if (next_symbol instanceof Nonterm) {
            var next = this.computeFollowAtPosition(rule, rule.position + 1);
            // console.log("    next(" + rule + ") = " + next);
            var rules_to_add = this.rules[next_symbol]
            if (rules_to_add === undefined) {
              throw("No rules known for symbol " + next_symbol);
            }
            for (var j = 0; j < rules_to_add.length; j++) {
              var rule_to_add = rules_to_add[j];
              for (var k = 0; k < next.size(); k++) {
                var new_rule = RuleFactory.make(rule_to_add.name, rule_to_add.symbols, next.get(k), 0)
                if (ret.addRule(new_rule)) {
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
    // Computes the kernel of the LR(1) Goto set for a given kernel and symbol
    // Dragon book 2ed, p261
    computeGotoKernel: function(setNum, lrSet, symbol) {
      // console.log("--> Rule_set #" + setNum + " = " + lrSet + ", symbol = " + symbol);
      var ret = new LR1Set();
      for (var i = 0; i < lrSet.coreSize(); i++) {
        var rule = lrSet.getCore(i);
        // console.log("    Processing rule " + rule);
        if (rule.position < rule.symbols.length && rule.symbols[rule.position].toString() == symbol) {
          // console.log("      Pushing " + symbol + " over in rule " + rule);
          var new_rule = rule.withPosition(rule.position + 1);
          // console.log("      new_rule = " + new_rule + " and lookaheads = " + lrSet.getLookaheads(i));
          ret.addRuleWithLookaheads(new_rule, lrSet.getLookaheads(i));
        }
      }
      this.completeClosure(ret, true);
      // console.log("<-- Result = " + ret);
      return ret;
    },

    reducer: function(U, R, Q, N, i, cur_tok, skipped_WS_loc) {
      // cur_tok = token #i+1: i starts at index 0, and tokens start at index 1
      // U is an array of sets of GSS nodes
      // R is the worklist of pending reductions
      // Q is the set of (GSS node, token) pairs of pending shifts
      // N is the set of SPPF nodes created in this step
      // cur_tok is the current token
      var item = R.shift();
      // console.log("In reducer, i = " + i + ", cur_tok = " + cur_tok.toString(true) + ", and item = " + item);
      var v = item.gssNode; // GSS node from which the reduction is to be applied
      var X = item.rule.name; // the name of the reduction rule
      var m = item.m; // the length of the RHS of the reduction rule (maybe just item.rule.position?)
      var f = item.f; // the index of the required nullable part at the righthand end of the reduction
      // or 0 if the reduction isn't right-nullable
      var y = item.label; // the SPPF node labels the first edge of the path down which the reduction is applied
      // (if m = 0 then y = epsilon) [sic]
      var y_m = (m !== 0 ? y : undefined)
      var U_i = U[i]
      const thiz = this;
      v.forPathsOfLength(m > 0 ? (m - 1) : 0, function(p) {
        // console.log("Found path " + JSON.stringify(p, null, "  "));
        // p.labels contain the edge labels of the path
        var u = p.leftSib;
        var k = u.label;
        var l = thiz.rnTable[k][X].push;
        if (l !== undefined) {
          // console.log("k = " + k + ", X = " + X + ", l = " + l);
          var z = undefined;
          if (m === 0)
            z = thiz.getEpsilonSPPF(f, cur_tok.pos.posAtStart());
          else {
            var c = u.parentSetId;
            var nodes_X = N[X];
            if (nodes_X === undefined)
              nodes_X = N[X] = {};
            z = nodes_X[c];
            if (z === undefined) {
              var pos;
              if (skipped_WS_loc !== undefined) {
                pos = skipped_WS_loc.posAtStart();
              } else {
                pos = cur_tok.pos;
              }
              if (p.labels.length > 0) {
                pos = pos.combine(p.labels[0].pos);
              }
              z = nodes_X[c] = new SPPFNode(X, pos);
            }
          }
          // console.log("z = " + z); // XXX Serializing of eSPPFs isn't working
          var w = U_i.itemForKey(l);
          if (w !== undefined) {
            // console.log("1. Adding link from " + w + " to " + u + " labelled " + z);
            if (thiz.addLink(/*from*/w, /*to*/u, /*labelled*/z)) {
              if (m !== 0) {
                var actions = thiz.getActions(l, cur_tok);
                if (actions !== undefined) {
                  var reductions = actions.reductions;
                  for (var r = 0; r < reductions.size(); r++) {
                    var red = reductions.get(r);
                    if (red.rule.position !== 0)
                      R.push(new ReductionItem(u, red.rule, red.rule.position, red.f, z));
                  }
                } else {
                  // Possibly shouldn't throw?
                  // throw("No actions found in state " + l + " for token " + next_tok.toString(true) + " at " +
                  //      next_tok.pos.toString(true));
                }
              }
            }
          } else {
            w = new GSSNode(/*labelled*/l);
            w.parentSetId = U_i.id;
            U_i.add(w);
            // console.log("Added " + w + " to U[" + i + "] ==> " + U_i);
            // console.log("2. Adding link from " + w + " to " + u + " labelled " + z);
            thiz.addLink(/*from*/w,/*to*/u,/*labelled*/z);
            var actions = thiz.getActions(l, cur_tok);
            if (actions !== undefined) {
              var ph = actions.push;
              if (ph !== undefined)
                Q.push(new ShiftPair(w, ph));
              var reductions = actions.reductions;
              // console.log("reductions(" + l + ", " + cur_tok.toString(true) + ") = " + reductions);
              for (var r = 0; r < reductions.size(); r++) {
                var red = reductions.get(r);
                if (red.rule.position === 0)
                  // the label here is irrelevant, because m == 0 means the rest of the rule derives EPSILON
                  R.push(new ReductionItem(w, red.rule, 0, red.f, EPSILON)); 
              }
              if (m !== 0) {
                for (var r = 0; r < reductions.size(); r++) {
                  var red = reductions.get(r);
                  if (red.rule.position !== 0)
                    R.push(new ReductionItem(u, red.rule, red.rule.position, red.f, z));
                }
              }
            } else {
              // Possibly shouldn't throw?
              // throw("No actions found in state " + l + " for token " + next_tok.toString(true) + " at " +
              //      next_tok.pos.toString(true));
            }
          }
          // console.log("R.length = " + R.length);
          // R.debugPrint();
          if (m !== 0) {
            var labels = p.labels.slice(0);
            labels.push(y_m);
            thiz.addChildren(item.rule, z, labels, f, skipped_WS_loc !== undefined ? skipped_WS_loc.posAtStart() : cur_tok.pos.posAtStart());
          }
        }
      });
    },

    addLink: function(rightSib, leftSib, val) {
      var link = undefined;
      for (var i = 0; i < rightSib.links.length; i++) {
        if (rightSib.links[i].prev === leftSib) {
          link = rightSib.links[i];
          break;
        }
      }
      if (link === undefined) {
        link = new Link(leftSib, val);
        rightSib.links.push(link);
        // console.log("Linking " + leftSib.toString() + " <-- " + rightSib.toString() + " with val " + val + "@" + val.pos);
        return link;
      } else {
        // console.log("Link already exists between " + leftSib.toString() + " <-- " + rightSib.toString());
        return undefined;
      }
    },

    shifter: function(U, R, Q, N, i, cur_tok, next_tok) {
      // cur_tok = token #i+1: i starts at index 0, and tokens start at index 1
      // next_tok = token #i+2
      // U is an array of sets of GSS nodes
      // R is the worklist of pending reductions
      // Q is the set of (GSS node, token) pairs of pending shifts
      // N is the set of SPPF nodes created in this step
      var success = false;
      if (cur_tok !== EOF) { // if i != d, in the paper, where d is the last token number
        // Q.debugPrint();
        var Qprime = new Queue([]);
        var z = new SPPFNode(cur_tok, cur_tok.pos);
        var U_i1 = U[i+1] = new KeyedSet("label");
        U_i1.id = i + 1;
        while (Q.length > 0) {
          var item = Q.shift();
          var v = item.gssNode;
          var k = item.state;
          var w = U_i1.itemForKey(k);
          // console.log("item = " + item + ", k = " + k + " and therefore w = " + w);
          if (w !== undefined) {
            success = true;
            this.addLink(/*from*/w, /*to*/v, /*labelled*/z);
            var actions = this.getActions(k, next_tok);
            if (actions !== undefined) {
              var reductions = actions.reductions;
              for (var r = 0; r < reductions.size(); r++) {
                var red = reductions.get(r);
                if (red.rule.position !== 0)
                  R.push(new ReductionItem(v, red.rule, red.rule.position, red.f, z));
              }
            } else {
              // Possibly shouldn't throw?
              // throw("No actions found in state " + k + " for token " + next_tok.toString(true) + " at " +
              //      next_tok.pos.toString(true));
            }
          } else {
            w = new GSSNode(/*labelled*/k);
            w.parentSetId = U_i1.id;
            U_i1.add(w);
            // console.log("Added " + w + " to U[" + (i + 1) + "] ==> " + U_i1);
            this.addLink(/*from*/w, /*to*/v, /*labelled*/z);

            var actions = this.getActions(k, next_tok);
            // console.log("next_tok = " + next_tok.toString(true) + ", k = " + k + ", actions = " + JSON.stringify(actions));
            if (actions !== undefined) {
              success = true;
              var ph = actions.push;
              if (ph !== undefined) {
                var sp = new ShiftPair(w, ph);
                // console.log("Pushing " + sp + " onto Q'");
                Qprime.push(sp);
              }
              var reductions = actions.reductions;
              // console.log("Reductions for k = " + k + " next_tok = " + next_tok + " are " + reductions);
              for (var r = 0; r < reductions.size(); r++) {
                var red = reductions.get(r);
                // console.log("red.f = " + red.f + ", red.rule.position = " + red.rule.position);
                if (red.rule.position !== 0) {
                  R.push(new ReductionItem(v, red.rule, red.rule.position, red.f, z));
                }
              }
              for (var r = 0; r < reductions.size(); r++) {
                var red = reductions.get(r);
                if (red.rule.position === 0) {
                  // the label here is irrelevant, because m == 0 means the rest of the rule derives EPSILON
                  R.push(new ReductionItem(w, red.rule, 0, red.f, EPSILON)); 
                }
              }
              // console.log("R = ");
              // R.debugPrint();
            } else {
              // Possibly shouldn't throw?
              // throw("No actions found in state " + k + " for token " + next_tok.toString(true) + " at " +
              //      next_tok.pos.toString(true));
            }
          }
        }
        while (Qprime.length > 0)
          Q.push(Qprime.shift());
        // console.log("At the end of shifter(" + i + "), Q = ");
        // Q.debugPrint();
      }
      return success;
    },

    addChildren: function(rule, y, kids, f, pos) {
      var lambda = kids.slice(0);
      if (f !== 0)
        lambda.push(this.getEpsilonSPPF(f, pos));
      // Find lambda in the kids of y
      function examine(y_rule, y_kids) {
        if (y_rule !== rule) return false;
        if (y_kids.length !== lambda.length) return false;
        for (var i = 0; i < y_kids.length; i++)
          if (y_kids[i] !== lambda[i]) return false;
        return true;
      }
      var found = false;
      if (y.kids) {
        found = examine(y.rule, y.kids);
      } else if (y.ambig) {
        for (var i = 0; i < y.ambig.length; i++)
          if (examine(y.ambig[i].rule, y.ambig[i].kids)) {
            found = true;
            break;
          }
      }
      if (!found)
        y.addChildren(rule, lambda, this);
    },

    getActions: function(state, tok) {
      var tableRow = this.rnTable[state];
      var actions1 = tableRow[tok];
      return actions1;
    },
    /*
    printSPPFasDot: function() {
      var ret = [];
      ret.push("digraph " + this.name + "_SPPF {");
      ret.push("  rankdir=RL; clusterrank=local;");
      ret.push("  // Nodes");
      for (var i = 0; i < SPPFNode.allNodes.length; i++) {
        var node = SPPFNode.allNodes[i];
        var style = "";
        if (node.inline)
          style = ", style=dashed";
        var label = "SPPFNode #" + node.sppfId;
        if (node.inline)
          label += "\nInlined e-SPPF for " + node.label;
        else
          label += "\nLabel: " + node.label.toString(true);
        if (node.rule)
          label += "\nRule: " + node.rule.toString();
        else if (node.ambig)
          label += "\nMultiple derivations";

        ret.push("  " + node.sppfId + " [label=" + JSON.stringify(label) + style + "];");
        if (node.ambig !== undefined) {
          for (var j = 0; j < node.ambig.length; j++) {
            ret.push("  \"" + node.sppfId + "_" + j 
                     + "\" [label=" + JSON.stringify(node.ambig[j].rule.toString()) +", shape=box];");
          }
        }
      }
      ret.push("  // Edges");
      for (var i = 0; i < SPPFNode.allNodes.length; i++) {
        var node = SPPFNode.allNodes[i];
        if (node.kids !== undefined)
          for (var j = 0; j < node.kids.length; j++)
            ret.push("  " + node.sppfId + " -> " + node.kids[j].sppfId
                     + " [label=\"" + j + "\"];");
        else if (node.ambig !== undefined) {
          for (var j = 0; j < node.ambig.length; j++) {
            ret.push("  " + node.sppfId + " -> \"" + node.sppfId + "_" + j + "\";");
            for (var k = 0; k < node.ambig[j].kids.length; k++)
              ret.push("  \"" + node.sppfId + "_" + j + "\" -> " + node.ambig[j].kids[k].sppfId 
                       + " [label=\"" + k + "\"];");
          }
        }
      }
      ret.push("}");
      return ret.join("\n");
    },

    printGSSasDot: function() {
      var ret = [];
      ret.push("digraph " + this.name + "_GSS {");
      ret.push("  rankdir=RL; clusterrank=local;");
      ret.push("  // Nodes");
      for (var i = 0; i < GSSNode.allNodes.length; i++) {
        var node = GSSNode.allNodes[i];
        ret.push("  " + node.gssId + "[label=" + JSON.stringify(node.toString()) + "];");
      }
      ret.push("  // Ranks");
      for (var i = 0; i < this.U.length; i++) {
        var s = "";
        for (var j = 0; j < this.U[i].size(); j++)
          s += " " + this.U[i].get(j).gssId + ";";
        ret.push("  subgraph cluster" + i + " {");
        ret.push("    rank = same; color=black;");
        if (i > 0)
          ret.push("    label=\"Token " + i + "\"");
        ret.push("    " + s);
        ret.push("  }");
      }
      ret.push("  // Edges");
      for (var i = 0; i < GSSNode.allNodes.length; i++) {
        var node = GSSNode.allNodes[i];
        for (var j = 0; j < node.links.length; j++)
          ret.push("  " + node.gssId + " -> " + node.links[j].prev.gssId 
                   + "[label=" + JSON.stringify(node.links[j].val.toString()) + "]"
                   //+ "[label=" + JSON.stringify(JSON.decycle(node.links[j].val), null, "  ") + "]"
                   + ";");
      }
      ret.push("}");
      return ret.join("\n");
    },
    */

    //////////////////////////////////
    // The RNGLR algorithm
    // Following Scott & Johnstone '06, TOPLAS 28:4
    //////////////////////////////////
    parseRNGLR: function(token_source) {
      if (token_source.isEmpty()) {
        if (this.acceptStates[0])
          return this.getEpsilonSPPF(this.I[this.start], SrcLoc.make(0,0,0,0,0,0));
        else {
          console.log("There are zero tokens available, but the grammar does not accept the empty string");
          return undefined;
        }
      } else {
        var v0 = new GSSNode(0);
        var U0 = new KeyedSet("label", [v0]);
        U0.id = 0;
        v0.parentSetId = U0.id;
        this.U = [];
        this.U[0] = U0;
        var R = new Queue([]);
        var Q = new Queue([]);
        var hasNext = true;
        var cur_tok = token_source.next(); // need to peek at first token
        var actions = this.getActions(0, cur_tok);
        // console.log("Actions[0][" + cur_tok.toString(true) + "] = " + JSON.stringify(actions));
        if (actions !== undefined) {
          var pk = actions.push;
          if (pk !== undefined)
            Q.push(new ShiftPair(v0, pk));
          // console.log("Q = ");
          // Q.debugPrint();
          var reductions = actions.reductions;
          for (var r = 0; r < reductions.size(); r++) {
            var red = reductions.get(r);
            if (red.rule.position === 0)
              // Again, label shouldn't matter because length is 0
              R.push(new ReductionItem(v0, red.rule, 0, red.f, EPSILON));
          }
        }
        // console.log("R = ");
        // R.debugPrint();
        var i = 0;
        // try {
          while (hasNext && this.U[i].size() > 0) {
            var N = new Queue([]);
            // console.log("Phase 1: reducing due to token #" + i + ": " + cur_tok.toString(true));
            while (R.length > 0) {
              this.reducer(this.U, R, Q, N, i, cur_tok, token_source.skippedWhitespace);
            }
            hasNext = token_source.hasNext();
            var next_tok = token_source.next();
            // console.log("Phase 2: shifting token #" + i + ": " + cur_tok.toString(true));
            hasNext = this.shifter(this.U, R, Q, N, i, cur_tok, next_tok) && hasNext;
            if (hasNext) {
              cur_tok = next_tok;
            } else {
              token_source.unshiftToken(cur_tok);
            }
            i++;
          }
          // console.log("DONE WITH LOOP, i = " + i 
          //             + ", last token = " + cur_tok.toString(true) + "@" + cur_tok.pos.toString(true)
          //             + ", hasNext = " + hasNext + ", final set emptied out: " + (this.U[i].size() == 0));
          if (!hasNext) i--;
          //        console.log("Finalizing: i = " + i + " and U[i] = " + this.U[i]);
          for (var acc = 0; acc < this.acceptStates.length; acc++) {
            if (this.acceptStates[acc]) {
              //console.log("Searching for " + acc);
              var t = this.U[i].itemForKey(acc);
              if (t !== undefined) {
                //console.log("Parse success!");
                var link = undefined;
                for (var j = 0; j < t.links.length; j++) {
                  if (t.links[j].prev === v0) {
                    link = t.links[j];
                    break;
                  }
                }
                if (link !== undefined) {
                  return link.val;
                } else
                  console.log("Couldn't find correct link in " + JSON.stringify(JSON.decycle(t), null, "  "));
              } else {
                //console.log("Parse failure");
              }
            }
          }
          // // Collect error information
          // if (i > 0) {
          //   var acc = undefined;
          //   for (var j = 0; j < this.U[i - 1].size(); j++) {
          //     var head = this.U[i - 1].get(j);
          //     console.log("For token " + i + ", " + cur_tok + ", head is in state " + head.label);
          //     acc = this.plausibleTokensForState(head.label, acc);
          //   }
          //   throw(acc);
          // }
        // } catch(e) {
        //   console.log(e);
        //   return undefined;
        // }
      }
    },

    plausibleTokensForState: function(state, ret) {
      if (ret === undefined)
        ret = {};
      for (var j = 0; j < this.tokens.size(); j++) {
        var tok = this.tokens.get(j);
        var actions = this.getActions(state, tok);
        if (tok !== EPSILON && actions !== undefined && 
            (actions.push !== undefined || (actions.reductions !== undefined && actions.reductions.size() > 0))) {
          console.log("In state " + state + " for token " + tok + ", actions = " + JSON.stringify(actions.reductions.elements));
          ret[tok] = true;
        }
      }
      return ret;
    },

    constructAllParses: function(sppfNode, semActions) {
      if (sppfNode === undefined) return undefined;
      if (sppfNode.label instanceof Token) {
        return [sppfNode.label];
      }
      var options = undefined;
      if (sppfNode.ambig)
        options = sppfNode.ambig;
      else
        options = [{kids: sppfNode.kids, rule: sppfNode.rule}];
      var ret = [];
      for (var i = 0; i < options.length; i++) {
        var kids = options[i].kids;
        var rule = options[i].rule;
        var kidsParses = [];
        for (var j = 0; j < kids.length; j++) {
          if (kids[j].rule === undefined && kids[j].ambig === undefined && kids[j].inline === true) {
            for (var k = 0; k < kids[j].kids.length; k++) {
              kidsParses.push([this.applyAction(kids[j].kids[k].rule, kids[j].kids[k].kids, kids[j].kids[k].pos, semActions)]);
            }
          } else if (kids[j].rule === undefined && kids[j].kids !== undefined && kids[j].label.indexOf(',') > -1) {
            for (var k = 0; k < kids[j].kids.length; k++) {
              kidsParses.push(this.constructAllParses(kids[j].kids[k], semActions));
            }
          } else {
            kidsParses.push(this.constructAllParses(kids[j], semActions));
          }
        }
        const thiz = this;
        ret = ret.concat(this.cartesian(kidsParses, 0, [], 
                                        function(kids) { return thiz.applyAction(rule, kids, sppfNode.pos, semActions); }));
      }
      return ret;
    },
    cartesian: function(arrays, idx, arr, callback) {
      if (arrays.length == idx) return callback(arr);
      var ret = [];
      for (var i = 0; i < arrays[idx].length; i++) {
        ret = ret.concat(this.cartesian(arrays, idx+1, arr.concat(arrays[idx][i]), callback));
      }
      return ret;
    },

    countAllParses: function(sppfNode) {
      if (sppfNode === undefined) return 0;
      var todo = [[[sppfNode]]]; // stack of alternate lists of children
      var count = [[[]], [[]]]; // stack of sums of factors
      while (todo.length > 0) {
        var top = todo[todo.length - 1];
        var topCount = count[count.length - 1];
        if (top.length === 0) { // done with all parses of a given node
          todo.pop();
          count.pop();
          var tot = 0;
          for (var i = 0; i < topCount.length; i++) {
            var prod = 1;
            for (var j = 0; j < topCount[i].length; j++)
              prod *= topCount[i][j];
            tot += prod;
          }
          var newTopCount = count[count.length - 1];
          newTopCount[newTopCount.length - 1].push(tot);
        } else if (top[top.length - 1].length === 0) { // done with one alternative
          top.pop();
          if (top.length > 0) {
            topCount.push([]);
          }
        } else { 
          var node = top[top.length - 1].pop();
          if (node.label instanceof Token) {
            topCount[topCount.length - 1].push(1);
          } else if (node.ambig && node.ambig.length > 0) {
            count.push([[]]);
            todo.push(node.ambig.map(function(d) { return d.kids.slice(0); }));
          } else if (node.kids && node.kids.length > 0) {
            count.push([[]]);
            todo.push([node.kids.slice(0)]);
          }
        }
      }
      if (count.length > 0 && count[0].length > 0 && count[0][0].length > 0)
        return count[0][0][0];
      else
        throw count;
    },

    constructUniqueParse: function(sppfNode, semActions) {
      if (sppfNode === undefined) return undefined;
      // Each stack item is a pair {todo, done}, where todo is a list of children
      // in reverse order (so that pop goes from left-to-right in actual tree order)
      // and done is a list of children in tree order
      // If there are nodes todo:
      //   We examine the pop()ed node from stack[TOP].todo:
      //     if it has kids, we push a new stack item
      //     otherwise, we move it over
      //   Otherwise we reduce
      var stack = [{todo: [sppfNode], done: []}];
      while (stack.length > 0 && stack[0].todo.length > 0) {
        var curr = stack[stack.length - 1];
        if (curr.todo.length > 0) { 
          var next = curr.todo[curr.todo.length - 1];
          if (next.label instanceof Token) {
            curr.todo.pop();
            curr.done.push(next.label);
          } else if (next.ambig) {
            throw("Not a unique parse");
          } else if (next.rule === undefined && next.label.indexOf(',') > -1) {
            curr.todo.pop();
            for (var k = next.kids.length - 1; k >= 0; k--)
              curr.todo.unshift(next.kids[k]);
          } else if (next.kids.length == 0) {
            // Optimization for nullary nonterminals
            curr.todo.pop();
            var pos = curr.done.length > 0 ? curr.done[curr.done.length - 1].pos : next.pos;
            curr.done.push(this.applyAction(next.rule, [], pos, semActions))
          } else {
            stack.push({todo: next.kids.slice(0).reverse(), done: []});
          }
        } else {
          if (stack.length > 1) {
            stack.pop();
            var prev = stack[stack.length - 1];
            var toReduce = prev.todo.pop();
            if (toReduce.rule === undefined) {
              function jsonPos(key, value) {
                if (key === "pos") { return "SrcLoc(" + value.toString(true) + ")"; }
                else { return value; }
              }
              throw("Cannot find a reduction rule on " + JSON.stringify(toReduce, jsonPos, "  "));
            } else {
              prev.done.push(this.applyAction(toReduce.rule, curr.done, toReduce.pos, semActions));
            }
          }
        }
      }
      return stack[0].done[0];
    },

    constructNextParse: function(sppfNode, semActions, directions) {
      if (sppfNode === undefined) return undefined;
      // Directions contains an array of pairs {choice, of}
      // saying which index choice (out of how many choices) at an ambig node
      // was taken by the previous parse.
      // To compute the next parse, remove all trailing pairs for which choice == of,
      // then increment the last remaining choice
      var newDirs = [];
      if (directions) {
        var lastIndex = directions.length - 1;
        while (lastIndex >= directions[lastIndex].choice == directions[lastIndex].of)
          lastIndex--;
        if (lastIndex < 0) return undefined;
        for (var i = 0; i < lastIndex; i++)
          newDirs[i] = directions[i];
        newDirs[lastIndex] = {choice: directions[i].choice + 1, of: directions[i].of};
      }
      var stack = [{todo: [sppfNode], done: []}];
      var curDir = 0;
      while (stack.length > 0 && stack[0].todo.length > 0) {
        var curr = stack[stack.length - 1];
        if (curr.todo.length > 0) {
          var next = curr.todo[curr.todo.length - 1];
          if (next.label instanceof Token) {
            curr.todo.pop();
            curr.done.push(next.label);
          } else if (next.rule === undefined && next.label.indexOf(',') > -1) {
            curr.todo.pop();
            for (var k = next.kids.length - 1; k >= 0; k--)
              curr.todo.unshift(next.kids[k]);
          } else if (next.ambig) {
            if (curDir < newDirs.length) {
              next = next.ambig[newDirs[curDir++].choice];
            } else {
              newDirs.push({choice: 0, of: next.ambig.length})
              next = next.ambig[0];
            }
            if (next.kids.length == 0) {
              // Optimization for nullary nonterminals
              curr.todo.pop();
              var pos = curr.done.length > 0 ? curr.done[curr.done.length - 1].pos : next.pos;
              curr.done.push(this.applyAction(next.rule, [], pos, semActions))
            } else {
              stack.push({todo: next.kids.slice(0).reverse(), done: []});
            }
          } else if (next.kids.length == 0) {
            // Optimization for nullary nonterminals
            curr.todo.pop();
            var pos = curr.done.length > 0 ? curr.done[curr.done.length - 1].pos : next.pos;
            curr.done.push(this.applyAction(next.rule, [], pos, semActions))
          } else {
            stack.push({todo: next.kids.slice(0).reverse(), done: []});
          }
        } else {
          if (stack.length > 1) {
            stack.pop();
            var prev = stack[stack.length - 1];
            var toReduce = prev.todo.pop();
            if (toReduce.rule === undefined) {
              function jsonPos(key, value) {
                if (key === "pos") { return "SrcLoc(" + value.toString(true) + ")"; }
                else { return value; }
              }
              throw("Cannot find a reduction rule on " + JSON.stringify(toReduce, jsonPos, "  "));
            } else {
              prev.done.push(this.applyAction(toReduce.rule, curr.done, toReduce.pos, semActions));
            }
          }
        }
      }
      return {parse: stack[0].done[0], directions: newDirs};
    },
    checkPositionContainment: function(sppfNode) {
      if (sppfNode === undefined) return undefined;
      var violations = [];
      //var worklist = [sppfNode];
      function helper(node) {
        var pos = node.pos;
        if (node.kids) {
          for (var i = 0; i < node.kids.length; i++) {
            var kidPos = node.kids[i].pos;
            if (kidPos.startChar < pos.startChar || kidPos.endChar > pos.endChar) {
              violations.push(node);
              break;
            }
          }
        } else if (node.ambig) {
          for (var i = 0; i < node.ambig.length; i++) {
            var kids = node.ambig[i].kids;
            var shouldBreak = false
            for (var j = 0; j < kids.length; j++) {
              var kidPos = kids[j].pos;
              if (kidPos.startChar < pos.startChar || kidPos.endChar > pos.endChar) {
                violations.push(node);
                shouldBreak = true;
                break;
              }
            }
            if (shouldBreak) break;
          }
        }
      }
      helper(sppfNode);
      if (violations.length > 0) return violations;
      return false;
    },

    computeRequiredNullableParts: function() {
      // The I dictionary is an index of all nullable nonterminals and (nonempty) strings of 
      // nonterminals that appear as right-hand tails in rules in the grammar.
      // The eSPPFs table is a dictionary indexed first by I value and second by position.
      // We already ensure that position objects are singletons, and this second-level
      // dictionary ensures that e-SPPFs are singletons too, for each position in the 
      // parse tree for which they are needed.  This helps ensure that finding existing links
      // doesn't create duplicates and therefore does't cause duplicate parses, later.
      allowNull = true;
      this.I = {};
      this.I[EPSILON] = 0;
      this.eSPPFs = [];
      this.eSPPFs[0] = {null: new SPPFNode(EPSILON, null)};
      // Initialize all eSPPFs to empty nodes
      for (var name in this.rules) {
        if (this.derivable[name][EPSILON] !== true) continue;
        var name_eSPPF = new SPPFNode(name, null);
        this.I[name] = this.eSPPFs.length;
        this.eSPPFs.push({null: name_eSPPF});
      }
      // Compute the e-closure of the rules
      for (var name in this.rules) {
        if (this.derivable[name][EPSILON] !== true) continue;
        var rules_name = this.rules[name];
        var name_eSPPF = this.eSPPFs[this.I[name]].null;
        for (var i = 0; i < rules_name.length; i++) {
          var rule = rules_name[i];
          if (this.computeFirstOfStrings(rule.symbols).contains(EPSILON)) {
            var kids = [];
            for (var j = 0; j < rule.symbols.length; j++) {
              kids.push(this.eSPPFs[this.I[rule.symbols[j]]].null);
            }
            name_eSPPF.addChildren(rule, kids, this);
          }
        }
      }
      // Compute required nullable parts
      for (var name in this.rules) {
        var rules_name = this.rules[name];
        for (var i = 0; i < rules_name.length; i++) {
          var rule = rules_name[i];
          for (var j = 1/*note: not 0*/; j < rule.symbols.length; j++) {
            var slice = rule.symbols.slice(j)
            if (this.computeFirstOfStrings(slice).contains(EPSILON) && this.I[slice] === undefined) {
              var eSPPF_slice = new SPPFNode("" + slice, null);
              eSPPF_slice.inline = true;
              var kids = [];
              for (var k = 0; k < slice.length; k++)
                kids[k] = this.eSPPFs[this.I[slice[k]]].null;
              eSPPF_slice.kids = kids;
              this.I[slice] = this.eSPPFs.length;
              this.eSPPFs.push({null: eSPPF_slice});
            }
          }
        }
      }
      allowNull = false;
    },
    getI: function(rule) {
      var ret = undefined;
      if (rule.position === 0) ret = this.I[rule.name];
      else if (rule.position === rule.symbols.length) ret = this.I[EPSILON];
      else ret = this.I[rule.symbols.slice(rule.position)];
      return ret;
    },
    getEpsilonSPPF: function(i, pos) {
      function find(node, cache) {
        for (var i = 0; i < cache.length; i++) {
          if (cache[i].key === node) {
            return cache[i].value;
          }
        }
        return undefined;
      }
      function clone(node, cache) {
        var ret = find(node, cache);
        if (ret !== undefined) return ret;
        ret = {};
        cache.push({key: node, value: ret});
        ret.sppfId = node.sppfId;
        ret.name = node.name;
        ret.label = node.label;
        ret.pos = pos;
        ret.toString = node.toString;
        ret.rule = node.rule;
        ret.inline = node.inline;
        if (node.kids !== undefined) {
          ret.kids = [];
          for (var i = 0; i < node.kids.length; i++) {
            ret.kids[i] = clone(node.kids[i], cache);
          }
        } else if (node.ambig !== undefined) {
          ret.ambig = [];
          for (var i = 0; i < node.ambig.length; i++) {
            var kids = [];
            ret.ambig.push({kids: kids, rule: node.ambig[i].rule});
            for (var j = 0; j < node.ambig[i].kids.length; j++) {
              kids[j] = clone(node.ambig[i].kids[j], cache);
            }
          }
        }
        return ret;
      }
      var byI = this.eSPPFs[i];
      var ret = byI[pos];
      if (ret === undefined)
        ret = byI[pos] = clone(byI[null], []);
      return ret;
    },
    resetESPPFs: function() {
      for (var i = 0; i < this.eSPPFs.length; i++) {
        this.eSPPFs[i] = {null: this.eSPPFs[i].null};
      }
    },

    computeRNTable: function() {
      const thiz = this;
      function initTables(index) {
        var tableRow = thiz.rnTable[index];
        if (tableRow === undefined)
          tableRow = thiz.rnTable[index] = {};
        for (var k = 0; k < thiz.atoms.size(); k++) {
          if (tableRow[thiz.atoms.get(k)] === undefined)
            tableRow[thiz.atoms.get(k)] = {push: undefined, reductions: new KeyedSet("key")};
        }
      }
      

      initTables(0);
      // This doesn't seem to be needed, because we always explicitly shift the EOF token
      // if (this.derivable[this.start][EPSILON] === true) {
      //   this.acceptStates[0] = true;
      //   this.rnTable[0][EOF].accept = true;
      // }
      for (var i = 0; i < this.states.size(); i++) {
        var state_i = this.states.get(i);
        initTables(i);
        // console.log("State " + i + " = " + state_i.toString());
        var full_state = this.completeClosure(state_i);
        // console.log("FullState " + i + " = " + full_state.toString());
        for (var j = 0; j < full_state.coreSize(); j++) {
          var rule_j = full_state.getCore(j);
          if (rule_j.name === this.start) {
            if (rule_j.position === rule_j.symbols.length && full_state.getLookaheads(j).contains(EOF)) {
              this.acceptStates[i] = true;
              this.rnTable[i][EOF].accept = true;
            }
          } else {
            if (this.computeFollowAtPosition(rule_j, rule_j.position).contains(EPSILON)) {
              var lookaheads = full_state.getLookaheads(j);
              for (var k = 0; k < lookaheads.size(); k++) {
                if (this.rnTable[i][lookaheads.get(k)] === undefined) {
                  throw("Can't get rnTable actions for i = " + i + " and symbol = " + lookaheads.get(k) + " in rule " + rule_j.toString(true));
                }
                this.rnTable[i][lookaheads.get(k)].reductions.add(new ReduceAction(rule_j, this.getI(rule_j)));
              }
            }
          }
        }
      }
    },

    //////////////////////////////////
    // 


    //////////////////////////////////
    // Algorithm 4.13 (p242-243) in Dragon book
    computeStateKernels: function() {
      var init_rule = this.rules[this.start][0].withLookahead(EOF);
      this.init_set = new LR1Set([init_rule]);
      this.completeClosure(this.init_set, true);
      var kernelStates = new SetOfSets([this.init_set], LR1Set.equals);
      this.rnTable = [];
      // Step 1
      var worklist = new Queue([this.init_set]);
      var state_num = -1;
      while (worklist.length > 0) {
        var set = worklist.shift();
        state_num++;
        this.rnTable[state_num] = undefined;
        for (var j = 0; j < this.atoms.size(); j++) {
          var atom_j = this.atoms.get(j);
          var new_set = this.computeGotoKernel(state_num, set, atom_j);
          if (new_set.coreSize() > 0) {
            var gotoStateNum = undefined;
            if (kernelStates.add(new_set)) {
              gotoStateNum = kernelStates.size() - 1;
              // console.log("\nAdded new_set at index " + gotoStateNum + ": " + new_set.toString() + "\n");
              // console.log("Added new_set at index " + gotoStateNum);
              worklist.push(new_set);
            } else {
              gotoStateNum = kernelStates.indexOf(new_set);
              // console.log("Set already exists at index " + gotoStateNum + ": " + new_set.toString());
            }
            // console.log(state_num + ":" + set + " goes to " + gotoStateNum + ":" + new_set + " via symbol " + atom_j);
            var tableRow = this.rnTable[state_num];
            if (tableRow === undefined)
              tableRow = this.rnTable[state_num] = {};
            var tableCell = tableRow[atom_j];
            if (tableCell === undefined)
              tableCell = tableRow[atom_j] = {push: undefined, reductions: new KeyedSet("key")};
            if (tableCell.push !== undefined && tableCell.push !== gotoStateNum)
              throw ("Already have a push action for atom " + atom_j + " in state " + state_num + ": supposed to goto state " + tableCell.push + " and now need to go to " + gotoStateNum);
            tableCell.push = gotoStateNum;
          }
        }
      }
      console.log("Done with step 1");
      // console.log("There are " + kernelStates.size() + " kernel states");
      // console.log(kernelStates.toString(true));
      // Step 2
      this.states = [];
      var spontLookaheads = {}; // Map from goto state number -> rule id -> setOf(lookaheads)
      var propLookaheads = {}; // Map from state number -> goto state num -> rule id -> setOf(rule ids)
      for (var i = 0; i < kernelStates.size(); i++) {
        this.states.push(new LR1Set());
        spontLookaheads[i] = {};
        propLookaheads[i] = {};
      }
      init_rule = this.rules[this.start][0];
      spontLookaheads[0][init_rule.id] = new KeyedSet("key", [EOF]);
      for (var i = 0; i < kernelStates.size(); i++) {
        // if (i > 0 && i % 100 == 0) { console.log("Processing state " + i); }
        this.applyLookaheads(spontLookaheads, propLookaheads, kernelStates.get(i), i, this.atoms.get(j));
      }
      console.log("Done with step 2");
      // Step 3
      var allLookaheads = {}; // Map from goto state num -> rule id -> setOf(lookaheads)
      for (var i = 0; i < kernelStates.size(); i++) {
        allLookaheads[i] = {};
        for (var j in spontLookaheads[i]) {
          // console.log("i = " + i + ", j = " + j + ", spontLookaheads[i][j] = " + spontLookaheads[i][j]);
          allLookaheads[i][j] = new KeyedSet(spontLookaheads[i][j]);
        }
      }
      console.log("Done with step 3");
      // Step 4
      // console.log("\n\n\n");
      function printSets(k, v) {
        if (v instanceof KeyedSet || v instanceof IntSet || v instanceof LR1Set)
          return v.toString();
        else
          return v;
      }
      // console.log("spontLookaheads = " + JSON.stringify(spontLookaheads, printSets, "  "));
      // console.log("propLookaheads = " + JSON.stringify(propLookaheads, printSets, "  "));
      var changed = true;
      var pass = 0;
      while (changed) {
        changed = false;
        pass++;
        for (var i = 0; i < kernelStates.size(); i++) {
          var state_i = kernelStates.get(i);
          var rnTable_i = this.rnTable[i];
          for (var x in rnTable_i) {
            var gotoState = rnTable_i[x].push;
            if (gotoState !== undefined) {
              for (var j = 0; j < state_i.coreSize(); j++) {
                var rule_j = state_i.getCore(j);
                var prop = propLookaheads[i][gotoState] ? propLookaheads[i][gotoState][rule_j.id] : undefined;
                var look = allLookaheads[i][rule_j.id];
                if (prop !== undefined && look !== undefined) {
                  // console.log("Propagating " + look.toString() + " from state " + i + " to all rules " + prop.toString() + " in state " + gotoState);
                  for (var k = 0; k < prop.size(); k++) {
                    var id_k = prop.get(k);
                    if (allLookaheads[gotoState][id_k] === undefined)
                      allLookaheads[gotoState][id_k] = new KeyedSet("key");
                    changed = allLookaheads[gotoState][id_k].merge(look) || changed;
                  }
                }
              }
            }
          }
        }
      }
      console.log("Done with step 4");
      // console.log("allLookaheads = " + JSON.stringify(allLookaheads, printSets, "  "));
      // console.log("All rules:")
      // for (var r in RuleFactory.byId)
      //   console.log(r + ": " + RuleFactory.byId[r]);
      // Step 5 -- expand
      for (var i = 0; i < kernelStates.size(); i++) {
        var kernelState_i = kernelStates.get(i);
        var state_i = this.states[i];
        for (var j = 0; j < kernelState_i.coreSize(); j++) {
          var rule_j = kernelState_i.getCore(j);
          var lookahead = allLookaheads[i][rule_j.id];
          // console.log("Lookaheads in state " + i + " for rule #" + rule_j.id + " " + rule_j.toString(true) + ": " + lookahead);
          if (lookahead) {
            state_i.addRuleWithLookaheads(rule_j, lookahead);
            // for (var k = 0; k < lookahead.size(); k++)
            //   state_i.addRule(RuleFactory.make(rule_j.name, rule_j.symbols, lookahead.get(k), rule_j.position));
          }
        }
      }
      this.states = new SetOfSets(this.states, LR1Set.equals);
    },

    //////////////////////////////////
    // Algorithm 4.12 (p242) in Dragon book
    applyLookaheads: function(spont, prop, rule_set, set_num) {
      // console.log("-----------------------------\nRuleSet #" + set_num + " = " + rule_set);
      for (var i = 0; i < rule_set.coreSize(); i++) {
        var rule = rule_set.getCore(i);
        var jPrime_rule = RuleFactory.make(rule.name, rule.symbols, HASH, rule.position);
        var jPrime = this.closureCache[jPrime_rule];
        if (jPrime === undefined)
          jPrime = this.closureCache[jPrime_rule] = this.completeClosure(new LR1Set([jPrime_rule]), true);
        // console.log("\nClosure(" + jPrime_rule.toString(true) + ") = " + jPrime.toString());
        for (var a = 0; a < this.atoms.size(); a++) {
          var symbol = this.atoms.get(a);
          if (this.rnTable[set_num] === undefined || this.rnTable[set_num][symbol] === undefined) {
            continue;
          }
          var goto_set_num = this.rnTable[set_num][symbol].push;
          // console.log("GOTO(" + set_num + ", " + symbol + ") = " + goto_set_num);
          
          for (var j = 0; j < jPrime.coreSize(); j++) {
            var rule_j = jPrime.getCore(j);
            if (rule_j.position < rule_j.symbols.length && rule_j.symbols[rule_j.position].toString() == symbol) {
              var lookaheads = jPrime.getLookaheads(j);
              if (lookaheads !== undefined) {
                for (var k = 0; k < lookaheads.size(); k++) {
                  if (lookaheads.get(k) !== HASH) {
                    var new_rule = RuleFactory.make(rule_j.name, rule_j.symbols, undefined, rule_j.position + 1);
                    if (spont[goto_set_num][new_rule.id] === undefined)
                      spont[goto_set_num][new_rule.id] = new KeyedSet("key");
                    if (spont[goto_set_num][new_rule.id].add(lookaheads.get(k))) {
                      // console.log("Added " + lookaheads.get(k) + " to rule " + new_rule + " in set " + goto_set_num);
                    }
                  } else {
                    var new_rule = RuleFactory.make(rule_j.name, rule_j.symbols, undefined, rule_j.position + 1);
                    if (prop[set_num][goto_set_num] === undefined)
                      prop[set_num][goto_set_num] = {};
                    if (prop[set_num][goto_set_num][rule.id] === undefined)
                      prop[set_num][goto_set_num][rule.id] = new IntSet();
                    if (prop[set_num][goto_set_num][rule.id].add(new_rule.id)) {
                      // console.log("Propagating from " + rule + " in state " + set_num + " to " + new_rule + " in state " + goto_set_num);
                    }
                  }
                }
              }
            }
          }
        }
      }
    },




    printTables: function() {
      var ret = "";
      for (var i = 0; i < this.rnTable.length; i++) {
        var str_action = ""
        rnTable_i = this.rnTable[i];
        if (rnTable_i === undefined) {
          var state = this.states.get(i);
          ret += "No actions in state #" + i + ":" + (state ? state.toString() : "undefined!") + "\n";
          continue;
        }
        for (var j = 0; j < this.atoms.size(); j++) {
          actions = this.rnTable[i][this.atoms.get(j)]
          if (actions.accept)
            str_action += "\n    On " + this.atoms.get(j) + ", accept";
          if (actions.push !== undefined)
            str_action += "\n    On " + this.atoms.get(j) + ", " + actions.push;
          if (actions.reductions && actions.reductions.size() > 0)
            str_action += "\n    On " + this.atoms.get(j) + ", " + actions.reductions;
        }
        var s = "In state #" + i + ":" + this.states.get(i).toString();
        if (this.acceptStates[i])
          s += " (ACCEPT STATE)";
        if (str_action)
          s += "\n  Actions:" + str_action;
        if (s !== "") s += "\n";
        ret += s;
      }
      return ret;
    },

    checkForCycles: function() {
      var cyclic = []
      for (var name in this.rules)
        if (this.derivable[name][name])
          cyclic.push(name);
      if (cyclic.length === 0) return false;
      return cyclic;
    }

    // //////////////////////////////////
    // // A LALR(1) ambiguity is a state/token pair that has multiple enabled actions
    // // Returns a list of warning messages
    // //////////////////////////////////
    // checkForLALRAmbiguity: function() {
    //   var ambiguities = []
    //   for (var i = 0; i < this.rnTable.length; i++) {
    //     var tableRow = this.rnTable[i];
    //     for (var name in tableRow) {
    //       if (tableRow.hasOwnProperty(name)) {
    //         var actions = tableRow[name];
    //         if (actions.size() > 1) {
    //           ambiguities.push("In state #" + i + ", conflicting actions on token " + name + ": " + actions);
    //         }
    //       }
    //     }
    //   }
    //   return ambiguities;
    // },

  }


  return {
    Atom: Atom,
    Nonterm: Nonterm,
    Token: Token,
    Rule: Rule,
    Grammar: Grammar,
    SetOfSets: SetOfSets,
    EOF: EOF,
    EPSILON: EPSILON,
    SrcLoc: SrcLoc
  };
});
