#lang pyret

provide *


data DFAState:
  | dfa-state(name :: String, delta :: Object, is-final :: Bool)
sharing:
  tostring(self): self._torepr() end,
  _torepr(self):
    for list.fold(acc from "State " + self.name + " (is-final = " + tostring(self.is-final) + "):",
        char from builtins.keys(self.delta).sort()):
      acc + "\n    " + char + " ==> " + tostring(self.delta.[char])
    end
  end,
  to-pyret-funs(self):
    header =
      "  fun state-" + self.name + "(s :: String) -> Bool:\n"
      + "    if s == '': " + tostring(self.is-final) + "\n"
      + "    else:\n"
      + "      c = s.char-at(0)\n"
      + "      new-s = s.substring(1, s.length())\n"
    keys = builtins.keys(self.delta).sort()
    middle = 
      for list.fold(acc from "", char from keys):
        if char <> "<ELSE>":
          prefix =
            if acc == "": "      if      "
            else:         "      else if "
            end
          acc + prefix + "c == '" + char + "': state-" + self.delta.[char] + "(new-s)\n"
        else: acc
        end
      end
    else-clause =
      if keys.length() == 1: "      state-" + self.delta.["<ELSE>"] + "(new-s)\n"
      else:
        "      else: state-" + self.delta.["<ELSE>"] + "(new-s)\n"
      + "      end\n"
      end
    footer =
      "    end\n" 
      + "  end"
    header + middle + else-clause + footer
  end,
  to-pyret(self):
    header = "    ['" + self.name + "']: {\n"
    middle = (for list.map(key from builtins.keys(self.delta).sort()):
      "      ['" + key + "']: '" + self.delta.[key] + "'"
    end).join-str(",\n") + "\n"
    tail = "    }"
    header + middle + tail
  end
end
data DFA:
  | dfa(states :: Object, start :: String)
sharing:
  _torepr(self):
    for list.fold(acc from "DFA starting at " + tostring(self.start),
        id from builtins.keys(self.states).sort()):
      acc + "\n  " + self.states.[id]._torepr()
    end    
  end,
  tostring(self): self._torepr() end,
  to-pyret-funs(self, name):
    "fun " + name + "(str :: String) -> Bool:\n" +
    for list.fold(acc from "", state from builtins.keys(self.states)):
      acc + self.states.[state].to-pyret-funs() + "\n"
    end +
    "  state-" + self.start + "(str)\n" +
    "end\n"
  end,
  to-pyret(self, name):
    header =
      "fun " + name + "(str :: String) -> Bool:\n"
      + "  final-states = list.to-set([\n"
    final-state-list = (for list.map(state from builtins.keys(self.states).sort()):
        if self.states.[state].is-final: "    '" + state + "'"
        else: nothing
        end
      end).filter(fun(s): not is-nothing(s) end).join-str(",\n") + "\n"
    delta =
      "  ])\n"
      + "  delta = {\n"
    delta-list = (for list.map(state from builtins.keys(self.states).sort()):
      self.states.[state].to-pyret()
    end).join-str(",\n") + "\n  }\n"

    header + final-state-list + delta + delta-list +
    "  last-state = for list.fold(state from '" + self.start + "', char from builtins.string-to-list(str)):\n" +
    "    state-delta = delta.[state]\n" +
    "    if builtins.has-field(state-delta, char): state-delta.[char]\n" +
    "    else: state-delta.['<ELSE>']\n" +
    "    end\n" +
    "  end\n" +
    "  final-states.member(last-state)\n" +
    "end"
  end
end
