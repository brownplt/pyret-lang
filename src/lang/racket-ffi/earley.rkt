#lang pyret

import dequeue as DQ

fun escape(s :: String):
  s-rep = torepr(s)
  s-rep.substring(1, s-rep.length() - 1)
end
  
data Token:
  | tok-str(s :: String) with: _torepr(self): escape(self.s) end,
    tostring(self): self._torepr() end
end

data ProdItem:
  | token(t :: Token) with: _torepr(self): self.t._torepr() end,
    matches(self, t): self.t == t end,
    tostring(self): self._torepr() end,
  | charset(chars :: String) with: _torepr(self): "[" + escape(self.chars) + "]" end,
    matches(self, c): self.chars.contains(c.s) end,
    tostring(self): self._torepr() end,
  | neg-charset(chars :: String) with: _torepr(self): "[^" + escape(self.chars) + "]" end,
    matches(self, c): not self.chars.contains(c.s) end,
    tostring(self): self._torepr() end,
  | nonterm(name :: String) with: _torepr(self): "<" + self.name + ">" end,
    tostring(self): self._torepr() end,
  | literal(s :: String) with: _torepr(self): self.s end,
    tostring(self): self.s end
end
data EarleyProd:
  | prod(name, before-dot :: List<ProdItem>, after-dot :: List<ProdItem>, sem-action) with:
    tostring(self): self._torepr() end,
    _torepr(self):
      self.name + " -> " + self.before-dot.reverse().join-str("") + " . " + self.after-dot.join-str("")
    end,
    _equals(self, other):
      (self.name == other.name) and (self.before-dot == other.before-dot) and (self.after-dot == other.after-dot)
    end,
    next-is-nonterm(self): is-nonterm(self.after-dot.first) end,
    incomplete(self): is-link(self.after-dot) end,
    shift-item(self, char):
      prod(self.name, self.after-dot.first^list.link(self.before-dot),
        self.after-dot.rest, self.sem-action).{matched: char}
    end
end
data EarleyState:
  | earley-state(prod :: EarleyProd, cyclic origin-state-set :: EarleyStateSet,
      pred :: Option<EarleyState>, mutable reductions :: Set<EarleyState>)
    with:
    _equals(self, other):
      (self.prod == other.prod)
      and (self.origin-state-set == other.origin-state-set)
    end,
    _torepr(self): "(" + self.prod._torepr() + ", " + tostring(self.origin-state-set.id) + ")" end,
    tostring(self): self._torepr() end,
    _lessthan(self, other): torepr(self) < torepr(other) end,
    predictor(self, new-state-set :: EarleyStateSet, grammar) -> DQ.MutQueue<EarleyState>:
      B = self.prod.after-dot.first.name
      B-prods = if builtins.has-field(grammar, B): grammar.[B] else: [] end
      cur-states = new-state-set!states
      for list.fold(acc from DQ.empty-q(), b-prod from B-prods):
        new-state = earley-state(b-prod, new-state-set, none, list.to-set([]))
        if cur-states.member(new-state): acc
        else: acc.enqueue(new-state)
        end
      end
    end,
    scanner(self, new-state-set :: EarleyStateSet, char :: Token) -> DQ.MutQueue<EarleyState>:
      if self.prod.after-dot.first.matches(char):
        new-state = earley-state(self.prod.shift-item(char), self.origin-state-set,
          some(self), list.to-set([]))
        if not new-state-set!states.member(new-state): DQ.single-q(new-state)
        else: DQ.empty-q()
        end
      else: DQ.empty-q()
      end
    end,
    completer(self, new-state-set :: EarleyStateSet) -> DQ.MutQueue<EarleyState>:
      ret = DQ.empty-q()
      for list.each(state from self.origin-state-set!states.elems):
        when state.prod.incomplete() 
          and state.prod.next-is-nonterm() 
          and (state.prod.after-dot.first.name == self.prod.name):
          new-prod = state.prod.shift-item("")
          cases(Option<EarleyState>) new-state-set!states.elems.find(
            fun(e): (e.prod == new-prod) end):
            | none =>
              new-state = earley-state(
                new-prod, state.origin-state-set, some(state), list.to-set([self]))
              # print("Found a new completion for " + tostring(state) + " in " + tostring(self))
              ret.enqueue(new-state)
            | some(old-state) =>
              # print("Found another parsing for " + tostring(state) + ":")
              new-state = earley-state(
                new-prod, state.origin-state-set, some(state), old-state!reductions.add(self))
              # print("  via " + torepr(new-state!reductions.elems))
              new-state-set!{states: new-state-set!states.remove(old-state).add(new-state)}
              ret.enqueue(new-state)
          end
        end
      end
      ret
    end,
   
    get-one-parse(self):
      fun help(s :: EarleyState, acc):
        cases(Option<EarleyState>) s.pred:
          | none =>
            # print("Reducing " + tostring(s.prod) + " with " + torepr(acc))
            ans = s.prod.sem-action(acc)
            # print(" ====> " + torepr(ans))
            ans
          | some(prev-s) =>
            cur = cases(List<EarleyState>) s!reductions.elems:
              | empty => s.prod.matched
              | link(red-s, _) => help(red-s, [])
            end
            help(prev-s, cur^list.link(acc))
        end
      end
      help(self, [])
    end            
end
data EarleyStateSet:
  | state-set(id :: Number, mutable states :: Set<EarleyState>) with:
    _torepr(self): self.id.tostring() + ": " + torepr(self!states) end,
    tostring(self): self._torepr() end
end



fun make-grammar(prods):
  for list.fold(outer-acc from {}, ps from prods.reverse()):
    for list.fold(acc from outer-acc, p from ps):
      if (p.name.length() > 4) and (p.name.substring(0, 4) == "tok-"):
        acc.{[p.name]: [p]}
      else if builtins.has-field(acc, p.name):
        acc.{[p.name]: p^list.link(acc.[p.name])}
      else:
        acc.{[p.name]: [p]}
      end
    end
  end
end

data Tree:
  | node(name, children :: List<Tree>) with:
    _torepr(self): self.name + "(" + self.children.join-str(", ") + ")" end,
    tostring(self): self._torepr() end
end

fun leaf(name): node(name, []) end

fun literal-rule(str :: String) -> EarleyProd:
  prod("tok-" + str, [], nonterm("WS")^list.link(builtins.string-to-list(str).map(fun(c): token(tok-str(c)) end) + [nonterm("WS")]),
    fun(_): leaf(str) end)
end
fun rule(name :: String, parts :: List<ProdItem>) -> List<EarleyProd>:
  literals = parts.filter(is-literal).map(fun(t): literal-rule(t.s) end)
  new-parts = for list.map(p from parts):
    if is-literal(p): nonterm("tok-" + p.s)
    else: p
    end
  end
  prod(name, [], new-parts, fun(kids): node(name, kids) end)^list.link(literals)
end
fun rule-action(name :: String, parts :: List<ProdItem>, action):
  literals = parts.filter(is-literal).map(fun(t): literal-rule(t.s) end)
  new-parts = for list.map(p from parts):
    if is-literal(p): nonterm("tok-" + p.s)
    else: p
    end
  end
  prod(name, [], new-parts, action)^list.link(literals)
end

fun parse(grammar, start, str):
  next-id = block:
    var id = -1
    fun():
      id := id + 1
      id
    end
  end

  graph:
  state0 = state-set(next-id(), list.to-set([earley-state(prod("<INITIAL>", [], [nonterm(start)], fun(): raise("NYI2") end), state0, none, list.to-set([]))]))
  end  
  
  var chart = {[tostring(state0.id)]: state0}

  # chart[n] gives the parse state after parsing n characters, i.e.
  # chart[0] includes all immediately possible rules, including <INITIAL> -> . whatever, and
  # if chart[n] includes <INITIAL> -> whatever . then it's a successful parse
  tokens = builtins.string-to-list(str).map(fun(c): tok-str(c) end)
  final-state-set = for list.fold_n(idx from 1, cur-state-set from state0, char from tokens):
    worklist = DQ.empty-q().enqueue-many(cur-state-set!states.elems)

    new-state-set = state-set(next-id(), list.to-set([]))
    chart := chart.{[tostring(idx)]: new-state-set}
    
    for DQ.each(state from worklist):
      if state.prod.incomplete():
        if state.prod.next-is-nonterm():
          # Prediction: For every state in S(k) of the form (X → α • Y β, j)
          # (where j is the origin position as above), add (Y → • γ, k) to S(k)
          # for every production in the grammar with Y on the left-hand side (Y → γ).
          new-states = state.predictor(cur-state-set, grammar)
          for DQ.each(new-state from new-states):
            cur-state-set!{states: cur-state-set!states.add(new-state)}
            # print("Predictor created state " + tostring(new-state) + " for idx " + tostring(idx))
          end
          worklist.append(new-states)
        else:
          # Scanning: If a is the next symbol in the input stream,
          # for every state in S(k) of the form (X → α • a β, j), add (X → α a • β, j) to S(k+1).
          new-states = state.scanner(new-state-set, char)
          for DQ.each(new-state from new-states):
            new-state-set!{states: new-state-set!states.add(new-state)}
            # print("Scanner created state " + tostring(new-state) + " for idx " + tostring(idx + 1))
          end
          # Do not append these items; they're not the worklist for the state-set of this index!
        end
      else:
        # Completion: For every state in S(k) of the form (X → γ •, j),
        # find states in S(j) of the form (Y → α • X β, i) and add (Y → α X • β, i) to S(k).
        new-states = state.completer(cur-state-set)
        for DQ.each(new-state from new-states):
          cur-state-set!{states: cur-state-set!states.add(new-state)}
          # print("Completer created state " + tostring(new-state) + " for idx " + tostring(idx))
        end
        worklist.append(new-states)
      end
      worklist.dequeue() # We're done with state
    end
    new-state-set
  end


  # Now final cleanup: run prediction and completion in final state to make sure any
  # epsilon rules fire, and any pending reductions can occur.
  final-worklist = DQ.empty-q().enqueue-many(final-state-set!states.elems)
  for DQ.each(state from final-worklist):
    if state.prod.incomplete():
      when state.prod.next-is-nonterm():
        new-states = state.predictor(final-state-set, grammar)
        for DQ.each(new-state from new-states):
          final-state-set!{states: final-state-set!states.add(new-state)}
          # print("Predictor created state " + tostring(new-state) + " in final-state-set")
        end
        final-worklist.append(new-states)
      end
    else:
      new-states = state.completer(final-state-set)
      for DQ.each(new-state from new-states):
        final-state-set!{states: final-state-set!states.add(new-state)}
        # print("Completer created state " + tostring(new-state) + " in final-state-set")
      end
      final-worklist.append(new-states)
    end
    final-worklist.dequeue() # We're done with state
  end

  chart
end
          


aexp-grammar = make-grammar([
    rule-action("WS", [nonterm("WS"), charset(" \r\n\t")], fun(_): "" end),
    rule-action("WS", [], fun(_): "" end),
    rule("S", [nonterm("S"), literal("+"), nonterm("M")]),
    rule-action("S", [nonterm("M")], fun(kids): kids.first end),
    rule("M", [nonterm("M"), literal("*"), nonterm("P")]),
    rule-action("M", [nonterm("P")], fun(kids): kids.first end),
    rule-action("P", [literal("("), nonterm("S"), literal(")")], fun(kids): kids.rest.first end),
    rule("P", [nonterm("T")]),
    rule-action("T", [charset("1234567890")], fun(char): char.first.s.tonumber() end),
    rule-action("T", [nonterm("T"), charset("1234567890")], fun(c): (c.first * 10) + c.rest.first.s.tonumber() end)
  ])


check:
  str = "123   +\t56*   (8 + 10)"
  final-chart = parse(aexp-grammar, "S", str)
  # for list.each(idx from builtins.keys(final-chart).sort-by(fun(a,b): a.tonumber() < b.tonumber() end, _ == _)):
  #   print("Idx " + idx + ":")
  #   print(tostring(final-chart.[idx]))
  # end
  for list.each(key from builtins.keys(aexp-grammar)):
    print(torepr(aexp-grammar.[key]))
  end
  parse-succ = final-chart.[tostring(str.length())]!states.elems
    .find(fun(p): (p.prod.name == "S") and (not p.prod.incomplete()) end)
  parse-succ satisfies is-some
  cases(Option<EarleyState>) parse-succ:
    | none => print("Parse failed")
    | some(p) => print(p.get-one-parse())
  end
end
