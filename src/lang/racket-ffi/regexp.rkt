#lang pyret

provide {
  Regexp : Regexp,
  empty : null-reg,
  is-empty : is-null-reg,
  epsilon : epsilon,
  is-epsilon : is-epsilon,
  lit : lit,
  is-lit : is-lit,
  star : star,
  is-star : is-star,
  concat : concat,
  is-concat : is-concat,
  alt : alt,
  is-alt : is-alt,
  char-complement : char-complement,
  is-char-complement : is-char-complement,
  charset : charset,
  is-charset : is-charset,
  optionl : optional,
  is-optional : is-optional,
  repeated : repeated,
  is-repeated : is-repeated
} end
    
import dfa as DFA
import nfa as NFA
import racket as R

fun<T> nub(l :: List<T>) -> List<T>:
  cases(List<T>) l:
    | empty => l
    | link(first, rest) => first^list.link(nub(rest.filter(first <> _)))
  end
end

fun map-append(f, xs):
  fun map-acc(l, acc):
    cases(List) l:
      | empty => acc
      | link(first, rest) => map-acc(rest, f(first)^list.link(acc))
    end
  end
  for list.fold(acc from [], x from map-acc(xs, [])): x + acc end
end

data RE :
  | re-phi with:
    tostring(self): "∅" end,
    is-greedy(self): true end,
    posEpsilon(self): false end,
    is-epsilon(self): false end,
    is-phi(self): true end,
    simplify(self): self end,
    partDerivSub(self, l): [] end,
    sigmaRESub(self, sigma): [] end

  | re-epsilon with:
    tostring(self): "ε" end,
    is-greedy(self): false end,
    posEpsilon(self): true end,
    is-epsilon(self): true end,
    is-phi(self): false end,
    simplify(self): self end,
    partDerivSub(self, c): [] end,
    sigmaRESub(self, sigma): [] end
      
  | re-char(c :: String) with:
    tostring(self): self.c end,
    is-greedy(self): true end,
    posEpsilon(self): false end,
    is-epsilon(self): false end,
    is-phi(self): false end,
    simplify(self): self end,
    partDerivSub(self, c):
      if self.c == c: [re-epsilon]
      else: []
      end
    end,
    sigmaRESub(self, sigma): [self.c] end
    
  | re-choice(r1 :: RE, r2 :: RE, greedy :: Bool) with:
    tostring(self):
      "(" + self.r1.tostring() + "|" + self.r2.tostring() + ")" + (if self.greedy: "" else: "?" end)
    end,
    is-greedy(self): self.greedy end,
    posEpsilon(self): self.r1.posEpsilon() or self.r2.posEpsilon() end,
    is-epsilon(self): self.r1.is-epsilon() and self.r2.is-epsilon() end,
    is-phi(self): self.r1.is-phi() and self.r2.is-phi() end,
    simplify(self):
      r1-simple = self.r1.simplify()
      r2-simple = self.r2.simplify()
      if r1-simple.is-phi(): r2-simple
      else if r2-simple.is-phi(): r1-simple
      else: re-choice(r1-simple, r2-simple, self.greedy)
      end
    end,
    partDerivSub(self, c):
      self.r1.partDerivSub(c) + self.r2.partDerivSub(c)
    end,
    sigmaRESub(self, sigma): self.r1.sigmaRESub(sigma) + self.r2.sigmaRESub(sigma) end
    
  | re-seq(r1 :: RE, r2 :: RE) with:
    tostring(self): self.r1.tostring() + self.r2.tostring() end,
    is-greedy(self): self.r1.is-greedy() or self.r2.is-greedy() end,
    posEpsilon(self): self.r1.posEpsilon() and self.r2.posEpsilon() end,
    is-epsilon(self): self.r1.is-epsilon() and self.r2.is-epsilon() end,
    is-phi(self): self.r1.is-phi() or self.r2.is-phi() end,
    simplify(self):
      r1-simple = self.r1.simplify()
      r2-simple = self.r2.simplify()
      if r1-simple.is-epsilon(): r2-simple
      else if r2-simple.is-epsilon(): r1-simple
      else: re-seq(r1-simple, r2-simple, self.greedy)
      end
    end,
    partDerivSub(self, c):
      if self.r1.posEpsilon():
        s0 = self.r1.partDerivSub(c)
        s1 = for map-append(new-r1 from s0):
          re-seq(new-r1, self.r2)
        end
        s2 = self.r2.partDerivSub(c)
        s1 + s2
      else:
        s0 = self.r1.partDerivSub(c)
        for map-append(new-r1 from s0):
          re-seq(new-r1, self.r2)
        end
      end
    end,
    sigmaRESub(self, sigma): self.r1.sigmaRESub(sigma) + self.r2.sigmaRESub(sigma) end
    
  | re-star(r :: RE, greedy :: Bool) with:
    tostring(self):
      self.r.tostring() + "*" + (if self.greedy: "" else: "?" end)
    end,
    is-greedy(self): self.greedy end,
    posEpsilon(self): true end,
    is-epsilon(self): is-re-phi(self.r) or self.r.is-epsilon() end,
    is-phi(self): false end,
    simplify(self): re-star(self.r.simplify(), self.greedy) end,
    partDerivSub(self, c):
      for map-append(new-r from self.r.partDerivSub(c)):
        re-seq(new-r, self)
      end
    end,
    sigmaRESub(self, sigma): self.r.sigmaRESub(sigma) end
    
      
  | re-any with:
    tostring(self): "." end,
    is-greedy(self): true end,
    posEpsilon(self): false end,
    is-epsilon(self): false end,
    is-phi(self): false end,
    simplify(self): self end,
    partDerivSub(self): [re-epsilon] end,
    sigmaRESub(self, sigma): sigma end
    
  | re-not(chars :: String) with:
    tostring(self): "[^" + self.chars + "]" end,
    is-greedy(self): true end,
    posEpsilon(self): false end,
    is-epsilon(self): false end,
    is-phi(self): false end,
    simplify(self): self end,
    partDerivSub(self, c):
      if self.chars.contains(c): []
      else: [re-epsilon]
      end
    end,
    sigmaRESub(self, sigma): sigma.filter(self.chars.contains) end
    
sharing:
  partDeriv(self, char :: String): nub(self.partDerivSub(char)) end,
  sigmaRE(self): nub(self.sigmaRESub(list.range(32, 127).map(ascii-chr))) end
end

fun ascii-chr(n :: Number) -> String:
  racket = R("racket/base")
  racket("string", racket("integer->char", n))
end
  
data Regexp:
  | null-reg with: tostring(self): "EMPTY" end
  | epsilon with: tostring(self): "" end
  | lit(s :: String) with: tostring(self): "\"" + self.s + "\"" end
  | star(r :: Regexp) with: tostring(self): self.r.tostring() + "*" end
  | concat(rs :: List<Regexp>) with: tostring(self): self.rs.join-str("") end
  | alt(rs :: List<Regexp>) with: tostring(self): "(" + self.rs.join-str("|") + ")" end
  | char-complement(s :: String) with: tostring(self): "[^" + self.s + "]" end
  | charset(s :: String) with: tostring(self): "[" + self.s + "]" end
  | optional(r :: Regexp) with: tostring(self): "(" + self.r.tostring() + ")?" end
  | repeated(r :: Regexp, min :: Number, max :: Number) with:
    tostring(self): "(" + self.r.tostring() + "){" + tostring(self.min) + "," + tostring(self.max) + "}" end,
    simpler-equiv(self):
      min-self = concat(list.repeat(self.min, self.r))
      if self.min == self.max:
        min-self
      else:
        opt-tail = for list.fold(acc from alt([self.r, epsilon]), n from list.range(self.min + 1, self.max)):
          alt([concat([self.r, acc]), epsilon])
        end
        concat([min-self, opt-tail])
      end
    end
sharing:
  nullable(self) -> Bool:
    cases(Regexp) self:
      | null-reg => false
      | epsilon => true
      | lit(s) => s == ""
      | star(_) => true
      | concat(rs) => list.all(fun(r): r.nullable() end, rs)
      | alt(rs) => is-empty(rs) or list.any(fun(r): r.nullable() end, rs)
      | char-complement(_) => false
      | charset(_) => false
      | optional(_) => true
      | repeated(r, min, max) => (min == 0) or r.nullable()
    end
  end,
  deriv(self, str :: String) -> Regexp:
    fun help(reg, start-char):
      cases(Regexp) reg:
        | null-reg => null-reg
        | epsilon => null-reg
        | lit(s) =>
          if s == "": null-reg
          else if s.substring(0, 1) == start-char:
            if (s.length() == 1): epsilon
            else: lit(s.substring(1, s.length()))
            end
          else: null-reg end
        | star(r) => concat(help(r, start-char), self)
        | concat(rs) =>
          cases(List<Regexp>) rs:
            | empty => null-reg
            | link(r, rest) =>
              if is-empty(rest):
                help(r, start-char)
              else:
                if not r.nullable():
                  concat(help(r, start-char)^list.link(rest))
                else:
                  alt([concat(help(r, start-char)^list.link(rest)),
                      help(concat(rest), start-char)])
                end
              end
          end
        | alt(rs) => alt(rs.map(help(_, start-char)))
        | char-complement(s) => if s.contains(start-char): null-reg else: epsilon end
        | charset(s) => if s.contains(start-char): epsilon else: null-reg end
        | optional(r) => help(alt([r, epsilon]), start-char)
        | repeated(_, _, _) => help(reg.simpler-equiv(), start-char)
      end
    end
    for list.fold(der from self, char-idx from list.range(0, str.length())):
      help(der, str.char-at(char-idx))
    end
  end,
  simplify(self):
    fun help(r):
      cases(Regexp) r:
        | null-reg => r
        | epsilon => r
        | lit(_) => r
        | charset(_) => r
        | char-complement(_) => r
        | alt(rs) =>
          simple-rs = rs.map(help)
          if list.all(is-null-reg, simple-rs): null-reg
          else if list.all(is-epsilon, simple-rs): epsilon
          else:
            filtered-rs = nub(simple-rs.filter(fun(new-r): (not is-null-reg(new-r)) end))
            if is-empty(filtered-rs): epsilon
            else if filtered-rs.length() == 1: filtered-rs.first
            else: alt(filtered-rs)
            end
          end
        | concat(rs) =>
          simple-rs = rs.map(help)
          if list.any(is-null-reg, simple-rs): null-reg
          else:
            filtered-rs = simple-rs.filter(fun(new-r): not is-epsilon(new-r) end)
            if is-epsilon(filtered-rs): epsilon
            else if filtered-rs.length() == 1: filtered-rs.first
            else: concat(filtered-rs)
            end
          end
        | optional(inner-r) =>
          simple-r = inner-r.simplify()
          if is-epsilon(simple-r) or is-null-reg(simple-r): epsilon
          else: optional(simple-r)
          end
        | repeated(inner-r, min, max) =>
          simple-r = inner-r.simplify()
          if is-epsilon(simple-r) or is-null-reg(simple-r): epsilon
          else if min == max: simple-r
          else: repeated(simple-r, min, max)
          end
      end
    end
    help(self)
  end,
  to-nfa(self):
    next-id = block:
      var id = 0
      fun():
        id := id + 1
        tostring(id)
      end
    end
    fun add-eps(state :: NFA.NFAState, to :: String) -> NFA.NFAState:
      delta = state.delta
      to-set = list.to-set([to])
      eps = if builtins.has-field(delta, "<eps>"): delta.["<eps>"].union(to-set) else: to-set end
      new-delta = delta.{["<eps>"]: eps}
      new-state = NFA.nfa-state(state.id, new-delta)
      new-state
    end
    fun combine(states1, states2):
      for list.fold(acc from states1, key from builtins.keys(states2)):
        acc.{[key]: states2:[key]}
      end
    end
    fun help(r :: Regexp) -> NFA.NFA:
      # print("Translating sub-regexp " + tostring(r) + " to an NFA")
      cases(Regexp) r:
        | null-reg =>
          start = next-id()
          NFA.nfa({[start]: NFA.nfa-state(start, {})}, start, "NOT A STATE") # finish isn't reachable
        | epsilon =>
          start = next-id()
          NFA.nfa({[start]: NFA.nfa-state(start, {})}, start, start)
        | lit(s) =>
          if s == "": help(epsilon)
          else:
            chars = for list.map(n from list.range(0, s.length())): s.char-at(n) end
            char-ids = list.map(fun(_): next-id() end, chars)
            finish-id = next-id()
            finish-state = NFA.nfa-state(finish-id, {})
            states = for list.fold3(acc from {[finish-id]: finish-state},
                first from char-ids,
                second from char-ids.rest + [finish-id],
                c from chars):
              acc.{[first]: NFA.nfa-state(first, {[c]: list.to-set([second])})}
            end
            NFA.nfa(states, char-ids.first, finish-id)
          end
        | star(inner-r) =>
          inner-r-nfa = help(inner-r)
          new-finish-state = add-eps(inner-r-nfa.states.[inner-r-nfa.finish], inner-r-nfa.start)
          NFA.nfa(inner-r-nfa.states.{[inner-r-nfa.finish]: new-finish-state},
            inner-r-nfa.start, inner-r-nfa.finish)
        | concat(rs) => list.all(fun(inner-r): inner-r.nullable() end, rs)
          cases(List<Regexp>) rs:
            | empty => help(epsilon)
            | link(first, rest) =>
              for list.fold(acc from help(first), next from rest):
                next-nfa = help(next)
                connected-acc-finish-state = add-eps(acc.states.[acc.finish], next-nfa.start)
                NFA.nfa(combine(acc.states.{[acc.finish]: connected-acc-finish-state}, next-nfa.states),
                  acc.start, next-nfa.finish)
              end
          end                
        | alt(rs) =>
          start = next-id()
          rs-nfas = list.map(help, rs)
          finish = next-id()
          start-state = NFA.nfa-state(start,
            {["<eps>"]: list.to-set(list.map(fun(rs-nfa): rs-nfa.start end, rs-nfas))})
          finish-state = NFA.nfa-state(finish, {})
          all-states = for list.fold(acc from {[start]: start-state, [finish]: finish-state}, rs-nfa from rs-nfas):
            new-finish = add-eps(rs-nfa.states.[rs-nfa.finish], finish)
            combine(acc, rs-nfa.states.{[rs-nfa.finish]: new-finish})
          end
          NFA.nfa(all-states, start, finish)
        | char-complement(chars) =>
          start = next-id()
          finish = next-id()
          dead = list.to-set(["DEAD"])
          delta = for list.fold(acc from {["<ELSE>"]: list.to-set([finish])},
              n from list.range(0, chars.length())):
            acc.{[chars.char-at(n)]: dead}
          end
          NFA.nfa({[start]: NFA.nfa-state(start, delta), [finish]: NFA.nfa-state(finish, {})}, start, finish)
        | charset(chars) =>
          start = next-id()
          finish = next-id()
          finish-set = list.to-set([finish])
          delta = for list.fold(acc from {},
              n from list.range(0, chars.length())):
            acc.{[chars.char-at(n)]: finish-set}
          end
          NFA.nfa({[start]: NFA.nfa-state(start, delta), [finish]: NFA.nfa-state(finish, {})}, start, finish)
        | optional(inner-r) =>
          inner-r-nfa = help(inner-r)
          new-start-state = add-eps(inner-r-nfa.states.[inner-r-nfa.start], inner-r-nfa.finish)
          NFA.nfa(inner-r-nfa.states.{[inner-r-nfa.start]: new-start-state}, inner-r-nfa.start, inner-r-nfa.finish)
        | repeated(_, _, _) => help(r.simpler-equiv())
      end
    end
    help(self)
  end
end

fun matches(r, s):
  r-deriv-s = r.deriv(s)
  r-deriv-s.nullable()
end



check:
  
  ab = charset("ab")
  ab36 = concat([repeated(ab, 1, 3), char-complement("ab")])
  
  print(tostring(ab36))
  ab36-nfa = ab36.to-nfa()
  print(ab36-nfa)
  ab36-dfa = ab36-nfa.determinize()
  print(ab36-dfa)
  print(ab36-dfa.to-pyret("match-ab13-notab"))
  
  # matches(ab36, "ac") is false
  # matches(ab36, "abc") is false
  # matches(ab36, "abac") is true
  # matches(ab36, "ababc") is true
  # matches(ab36, "ababac") is true
  # matches(ab36, "abababc") is false
  # matches(ab36, "abababac") is false
  # matches(ab36, "ababababc") is false


  aoptb = concat([repeated(optional(lit("a")), 1, 10), lit("b")])

  matches(aoptb, "b") is true
  # matches(aoptb, "ab") is true
  # matches(aoptb, "aab") is true
  # matches(aoptb, "aaab") is true
  # matches(aoptb, "aaaab") is true
  # matches(aoptb, "c") is false
  # matches(aoptb, "ac") is false
  # matches(aoptb, "aac") is false
  # matches(aoptb, "aaac") is false
  # matches(aoptb, "aaaac") is false
end

