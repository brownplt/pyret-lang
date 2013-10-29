#lang pyret

provide *
import dfa as DFA

data NFAState:
  | nfa-state(id :: String, delta :: Object)
sharing:
  e-closure(self, the-nfa):
    doc: "Returns all states in the provided NFA that are epsilon reachable from the current state"
    fun help(cur-eps, state):
      if cur-eps.member(state.id): cur-eps
      else if not builtins.has-field(state.delta, "<eps>"): cur-eps.add(state.id)
      else:
        eps = state.delta.["<eps>"]
        for list.fold(acc from cur-eps.add(state.id), id from eps.elems):
          help(acc, the-nfa.states.[id])
        end
      end
    end
    help(list.to-set([]), self)
  end,
  tostring(self): self._torepr() end,
  _torepr(self):
    for list.fold(acc from "State " + self.id + ":", char from builtins.keys(self.delta).sort()):
      acc + "\n    " + char + " ==> {" + self.delta.[char].elems.join-str(",") + "}"
    end
  end
end
data NFA:
  | nfa(states :: Object, start :: String, finish :: String)
sharing:
  _torepr(self):
    for list.fold(acc from "NFA: " + tostring(self.start) + " ======> " + tostring(self.finish),
        id from builtins.keys(self.states).sort-by(fun(a,b): a.tonumber() < b.tonumber() end, _ == _)):
      acc + "\n  " + self.states.[id]._torepr()
    end    
  end,
  tostring(self): self._torepr() end,
  determinize(self :: NFA) -> DFA.DFA:
    doc: "Returns a DFA equivalent to the current NFA"
    dead-state-delta = {["<ELSE>"]: list.to-set(["DEAD"])}
    state-names = "DEAD"^list.link(builtins.keys(self.states))
    all-states = for list.fold(acc from {}, id from state-names):
      if id == "DEAD":
        acc.{DEAD: nfa-state("DEAD", dead-state-delta)}
      else:
        state = self.states.[id]
        if builtins.has-field(state.delta, "<ELSE>"): acc.{[id]: state}
        else: acc.{[id]: nfa-state(state.id, state.delta.{["<ELSE>"]: list.to-set(["DEAD"])})}
        end
      end
    end
    e-closure-per-state = for list.fold(acc from {}, id from state-names):
      acc.{[id]: all-states.[id].e-closure(self)}
    end
    fun set-to-name(s :: Set):
      s.to-list().join-str("-")
    end
    fun create-dfa-state-delta(nfa-state-set :: Set) -> Object:
      empty-set = list.to-set([])
      eps-reachable = for list.fold(acc from empty-set, id from nfa-state-set.elems):
        acc.union(e-closure-per-state.[id])
      end
      total-alphabet = (for list.fold(acc from empty-set, id from eps-reachable.elems):
        acc.union(list.to-set(builtins.keys(all-states.[id].delta)))
      end).remove("<eps>")
      new-delta = for list.fold(delta-acc from dead-state-delta, char from total-alphabet.elems):
        reach = for list.fold(acc from empty-set, id from eps-reachable.elems):
          if builtins.has-field(all-states.[id].delta, char):
            delta-char = all-states.[id].delta.[char]
            delta-char-reachable = for list.fold(e from delta-char, id2 from delta-char.elems):
              e.union(e-closure-per-state.[id2])
            end
            acc.union(delta-char-reachable)
          else:
            acc
          end
        end
        delta-acc.{[char]: reach}
      end
      new-delta
    end
    fun help(cur-states, cur-state-set :: Set):
      name = set-to-name(cur-state-set)
      is-final = list.any(self.finish == _, cur-state-set.elems)
      if builtins.has-field(cur-states, name): cur-states
      else:
        new-state-delta = create-dfa-state-delta(cur-state-set)
        # print("NFA-version delta for " + name + " is ")
        # print(new-state-delta)
        chars = builtins.keys(new-state-delta)
        delta = for list.fold(acc from {}, char from chars):
          acc.{[char]: set-to-name(new-state-delta.[char])}
        end
        new-state = DFA.dfa-state(name, delta, is-final)
        # print("New DFA state = " + tostring(new-state))
        for list.fold(acc from cur-states.{[name]: new-state}, char from chars):
          help(acc, new-state-delta.[char])
        end
      end
    end
    new-states = help({DEAD: DFA.dfa-state("DEAD", {["<ELSE>"]: "DEAD"}, false)}, list.to-set([self.start]))
    new-start = set-to-name(e-closure-per-state.[self.start])
    DFA.dfa(new-states, new-start)
  end
end
