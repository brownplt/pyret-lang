#lang pyret

provide {
  LabelledTree: LabelledTree,
  TreeMaker : TreeMaker,
  TreeFactory : TreeFactory.test,
  is-node : is-ltNode,
  align: align,
  push-data: push-data,
  pull-data: pull-data,
  get-one-alignment: get-one-alignment,
  get-all-alignments: get-all-alignments,
  each: each
} end

import math as Math
import "box.arr" as Box
import "pprint.arr" as PP

Mut = Box.Mut
box = Box.box

var next-tree = 0
TreeFactory = brander()
fun TreeMaker(init):
  next-tree := next-tree + 1
  var next-id = init
  fun incr():
    next-id := next-id + 1
    next-id
  end
  fun decorate(node):
    size = 1 + for list.fold(acc from 0, kid from node.children):
      if builtins.has-field(kid, "size"): acc + kid.size
      else: acc
      end
    end
    height = 1 + for list.fold(acc from 0, kid from node.children):
      if builtins.has-field(kid, "height"):
        h = kid.height
        if h < acc: h
        else: acc
        end
      else: acc
      end
    end
    node.{tree-id: next-tree, id: incr(), size: size, height: height}
  end
  factory = {
    node(self, name, label, children :: list.List):
      new-node = decorate(ltNode(name, label, children, box(nothing)))
      for list.each(c from children):
        c.update-parent(new-node)
      end
      new-node
    end,
    node-info(self, name, label, children, info):

      new-node = decorate(ltNode(name, label, children, box(info)))
      for list.each(c from children):
        c.update-parent(new-node)
      end
      new-node
    end,
    leaf(self, name, label):
      new-node = decorate(ltNode(name, label, [], box(nothing)))
      new-node
    end
  }
  TreeFactory.brand(factory)
end

fun general-gamma(no-change, insert, delete, update):
  fun gamma(tea :: TreeEditAction):
    if is-teaInsert(tea): insert
    else if is-teaDelete(tea): delete
    else if is-teaSame(tea): no-change
    else:
      if tostring(tea.oldV.label) == tostring(tea.newV.label): no-change
      else: update
      end
    end
  end
  gamma
end

gamma-edit = general-gamma(0, 1, 1, 1)
gamma-inclusion = general-gamma(0, 0, 1, 1)

data TreeEditAction:
  | teaInsert(v, prevKey) with: tostring(self): "Insert " + tostring(self.v.label) end
  | teaDelete(v, prevKey) with: tostring(self): "Delete " + tostring(self.v.label) end
  | teaRename(oldV, newV, kidsPrevKey, restPrevKey) with:
    tostring(self):
      if self.oldV.label == self.newV.label: "Copy " + tostring(self.oldV.label)
      else: "Rename " + tostring(self.oldV.label) + " to " + tostring(self.newV.label)
      end
    end
  | teaSame with: tostring(self): "Done" end
end

fun<A> transpose(lol :: list.List<list.List<A>>) -> list.List<list.List<A>>:
  for list.fold(ret from [], l from lol.reverse()):
    if list.is-empty(ret):
      l.map(fun(item): [item] end)
    else:
      for list.map2(item from l, acc from ret):
        item^list.link(acc)
      end
    end
  end
end

data LabelledTree:
  | ltNode(
      name,
      label,
      children :: list.List,
      info :: Mut)
sharing:
  mkc(self, cnum): self.children.get(cnum).mk-ast() end,
  is-leaf(self): self.children.length() == 0 end,
  unzip(self):
    transpose(self.children.map(fun(n): n.children end))
  end,
  add-fact(self, name, val):
    cur-info = self.info.get()
    if is-nothing(cur-info): self.info.set({[name]: val})
    else: self.info.set(cur-info.{[name]: val})
    end
    self
  end,
  get-fact(self, name):
    cur-info = self.info.get()
    if is-nothing(cur-info): nothing
    else if builtins.has-field(cur-info, name): cur-info.[name]
    else: nothing
    end
  end,
  has-fact(self, name):
    cur-info = self.info.get()
    (not is-nothing(cur-info)) and builtins.has-field(cur-info, name)
  end,
  has-facts(self, facts):
    for list.fold(acc from true, f from facts):
      acc and self.has-fact(f)
    end
  end,
  copy(self, T):
    fun help(self):
      T.node-info(self.label, self.children.map(help), self.info.get())
    end
    help(self)
  end,
  take-fact(self, fact, other):
    self.add-fact(fact, other.get-fact(fact))
    self
  end,
  take-facts(self, facts, other):
    for list.each(f from facts):
      self.take-fact(f, other)
    end
    self
  end,
  _equals(self, other):
    if LabelledTree(self) and LabelledTree(other):
      if (self.tree-id == other.tree-id) and (self.id == other.id): true
      else:
        labels-match = self.label == other.label
        for list.fold2(acc from labels-match, k1 from self.children, k2 from other.children):
          acc and (k1 == k2)
        end
      end
    else: self == other
    end
  end,
  topprint-show-ids(self, show-ids :: Bool):
    kids = self.children.map(fun(k): PP.group(k.topprint-show-ids(show-ids)) end)
    fun help(self):
      bare-lbl =
        if is-nothing(self.name):
          if LabelledTree(self.label):
            PP.surround(1, 0, PP.lbrace, self.label.topprint-show-ids(show-ids), PP.rbrace)
              + PP.break(0)
          else if builtins.has-field(self.label, "topprint"): self.label.topprint()
          else: PP.string(tostring(self.label))
          end
        else:
          if builtins.has-field(self.name, "topprint"): self.name.topprint()
          else: PP.string(tostring(self.name))
          end
        end
      if show-ids:
        PP.string(tostring(self.tree-id) + "/" + tostring(self.id) + ":") + bare-lbl
      else: bare-lbl
      end
    end
    label = help(self)
    PP.label-align-surround(label, PP.lparen, PP.commabreak, kids, PP.rparen)
  end,
  topprint(self): self.topprint-show-ids(false) end,
  update-parent(self, p):
    self.add-fact("parent", p)
  end,
  get-parent(self): self.get-fact("parent") end,
  nth-child(self,n): self.children.get(n) end,
  pretty(self, show-ids :: Bool, width): self.topprint-show-ids(show-ids).pretty(width) end,
  tostring(self):
    for list.fold(acc from "", line from self.pretty(false, 80)):
      if acc == "": line
      else: acc + "\n" + line
      end
    end
  end,
  tostring-id(self):
    for list.fold(acc from "", line from self.pretty(true, 80)):
      if acc == "": line
      else: acc + "\n" + line
      end
    end
  end,
  hash-key(self):
    "(" + tostring(self.id) + ", "
      + self.children.foldl(fun(c,acc): acc + c.hash-key() end, "") + ")"
  end,
  short-hash-key(self):
    tostring(self.id)
  end,
  print(self, width):
    list.each(print, self.pretty(false, width))
  end,
  print-id(self, width):
    list.each(print, self.pretty(true, width))
  end
end


fun each(f, n):
  f(n)
  n.children.each(fun(c): each(f, c) end)
end

fun push-data(f, self, info, pos):
  new-info = f(self, info, pos)
  for list.each_n(p from 0, c from self.children):
    push-data(f, c, new-info, p)
  end
end
fun pull-data(f, self, info):
  new-infos =
    for list.map(c from self.children):
      pull-data(f, c, info)
    end
  f(self, new-infos)
end



fun edit-distance(self, other, gamma):
  # memoization table
  var table = {}
  # the label-edit cost function
  fun help(f1, f2):
    fun forest-hash(inner-f1, inner-f2):
      fun help2(f): f.foldl( fun(t, acc): t.short-hash-key() + acc end, "") end
      help2(inner-f1) + "," + help2(inner-f2)
    end
    # prune empty leftmost trees right away
    key = forest-hash(f1, f2)
    res =
      if builtins.has-field(table, key): table.[key]
      else:
        if list.is-empty(f2):
          if list.is-empty(f1): # empty tree
            { cost: 0, actions: [teaSame] } 
          else: # (f1, empty)
            new-f1 = f1.first.children.append(f1.rest)
            new-key = forest-hash(new-f1, f2)
            action = teaDelete(f1.first, new-key)
            cost = help(new-f1, f2) + gamma(action)
            { cost: cost, actions: [action] }
          end
        else if list.is-empty(f1): # (empty, f2)
          new-f2 = f2.first.children.append(f2.rest)
          new-key = forest-hash(f1, new-f2)
          action = teaInsert(f2.first, new-key)
          cost = help(f1, new-f2) + gamma(action)
          { cost: cost, actions: [action] }
        else: # (f1, f2)
          new-f1 = f1.first.children.append(f1.rest)
          new-f2 = f2.first.children.append(f2.rest)
          f1-label = tostring(f1.first.label)
          f2-label = tostring(f2.first.label)
          case1-action = teaDelete(f1.first, forest-hash(new-f1, f2))
          case2-action = teaInsert(f2.first, forest-hash(f1, new-f2))
          case3-action = teaRename(f1.first, f2.first,
            forest-hash(f1.first.children, f2.first.children),
            forest-hash(f1.rest, f2.rest))
          case1-cost = help(new-f1, f2) + gamma(case1-action)
          case2-cost = help(f1, new-f2) + gamma(case2-action)
          case3-cost =
            (help(f1.first.children, f2.first.children) +
              help(f1.rest, f2.rest) + gamma(case3-action))
          var ret = [case1-action]
          var min-cost = case1-cost
          if (case2-cost < min-cost):
            ret := [case2-action]
            min-cost := case2-cost
          else if (case2-cost == min-cost):
            ret := case2-action^list.link(ret)
          else: nothing
          end
          if (case3-cost < min-cost):
            ret := [case3-action]
            min-cost := case3-cost
          else if (case3-cost == min-cost):
            ret := case3-action^list.link(ret)
          else: nothing
          end
          {cost: min-cost, actions: ret}
        end
      end
    table := table.{[key]:res}
    res.cost
  end
  help([self], [other])
  table
where:
  eq = checkers.check-equals
  
  fun edit-distance-cost-sym(msg, t1, t2, expected):
    res1 = edit-distance(t1, t2, gamma-edit).[t1.short-hash-key() + "," + t2.short-hash-key()].cost
    res2 = edit-distance(t2, t1, gamma-edit).[t2.short-hash-key() + "," + t1.short-hash-key()].cost
    eq(msg + ": t1 -> t2", res1, expected)
    eq(msg + ": t2 -> t1", res2, expected)
  end
  
  
  T = TreeMaker(0)
  abc = T.node("A", "A", [T.leaf("B", "B"), T.leaf("C", "C")])
  adc = T.node("A", "A", [T.leaf("D", "D"), T.leaf("C", "C")])
  ade = T.node("A", "A", [T.leaf("D", "D"), T.leaf("E", "E")])
  bcb = T.node("B", "B", [T.leaf("C", "C"), T.leaf("B", "B")])
  
  ab = T.node("A", "A", [T.leaf("B", "B")])
  ad = T.node("A", "A", [T.leaf("D", "D")])
  
  edit-distance-cost-sym("A/B", T.leaf("A", "A"), T.leaf("B", "B"), 1)
  edit-distance-cost-sym("B/C", T.leaf("B", "B"), T.leaf("B", "B"), 0)
  edit-distance-cost-sym("A(B,C)/A(B,C)", abc, abc, 0)
  edit-distance-cost-sym("A(D,C)/A(D,C)", adc, adc, 0)
  edit-distance-cost-sym("A(B,C)/A(D,C)", abc, adc, 1)
  edit-distance-cost-sym("A(B,C)/A(D,E)", abc, ade, 2)
  edit-distance-cost-sym("A(B,C)/B(C,B)", abc, bcb, 3)
  edit-distance-cost-sym("A(B,C)/A(B)", abc, ab, 1)
  edit-distance-cost-sym("A(B,C)/A(D)", abc, ad, 2)
  
  edit-distance-cost-sym("A(B(E),C(B),D(G,H,I(J)))/B(B(I,J(A)),C(F),A,D(G,H,I(K)))",
    T.node("A", "A", [
        T.node("B", "B", [T.leaf("E", "E")]),
        T.node("C", "C", [T.leaf("F", "F")]),
        T.node("D", "D", [
            T.leaf("G", "G"),
            T.leaf("H", "H"),
            T.node("I", "I", [T.leaf("J", "J")])])]),
    T.node("B", "B", [
        T.node("B", "B", [
            T.leaf("I", "I"),
            T.node("J", "J", [T.leaf("A", "A")])]),
        T.node("C", "C", [T.leaf("F", "F")]),
        T.leaf("A", "A"),
        T.node("D", "D", [
            T.leaf("G", "G"),
            T.leaf("H", "H"),
            T.node("I", "I", [T.leaf("K", "K")])])]), 6)
end


fun align(t1, t2):
  edit-distance(t1, t2, gamma-inclusion)
end
fun get-one-alignment(t1, t2, edits):
  start-hash = t1.short-hash-key() + "," + t2.short-hash-key()
  fun help(start, alignment):
    actions =
      if builtins.has-field(edits, start): edits.[start].actions
      else: []
      end
    if list.is-empty(actions): {}
    else:
      action = actions.first
      if is-teaRename(action):
        alignment2 = alignment.{[tostring(action.newV.id)]: action.oldV.id}
        alignment3 = help(action.kidsPrevKey, alignment2)
        alignment4 = help(action.restPrevKey, alignment3)
        alignment4
      else if is-teaSame(action): alignment
      else: help(action.prevKey, alignment)
      end
    end
  end
  if edits.[start-hash].cost > 0: {}
  else: help(start-hash, {})
  end
where:
  eq = checkers.check-equals
  
  T1 = TreeMaker(0)
  tree1 =
    T1.node("Root", "Root", [
        T1.node("X", "X", [T1.leaf("X", "X")]),
        T1.node("X", "X", [T1.leaf("X", "X")])])
  T2 = TreeMaker(100)
  tree2 =
    T2.node("Root", "Root", [
        T2.node("X", "X", [
            T2.node("Y", "Y", [T2.node("Y", "Y", [T2.node("Y", "Y", [T2.leaf("X", "X")])])]),
            T2.node("Y", "Y", [T2.node("Y", "Y", [T2.node("Y", "Y", [T2.leaf("X", "X")])])])]),
        T2.node("X", "X", [
            T2.node("Z", "Z", [T2.leaf("X", "X")])])])
  alignment12 = {["113"]: 5, ["109"]: 2, ["101"]: 1, ["110"]: 3, ["112"]: 4}
  T3 = TreeMaker(200)
  tree3 =
    T3.node("Root", "Root", [
        T3.node("X", "X", [T3.leaf("Y", "Y")]),
        T3.node("X", "X", [T3.leaf("X", "X")])])
  T4 = TreeMaker(300)
  tree4 =
    T4.node("Root", "Root", [
        T4.node("X1", "X1", [T4.leaf("Y1", "Y1")]),
        T4.node("X2", "X2", [T4.leaf("Y2", "Y2")]),
        T4.node("X3", "X3", [T4.leaf("Y3", "Y3")])])
  T5 = TreeMaker(400)
  tree5 = 
    T5.node("Root", "Root", [
        T5.node("Z12", "Z12", [
            T5.node("X1", "X1", [T5.leaf("Y1", "Y1")]),
            T5.node("X2", "X2", [T5.leaf("Y2", "Y2")])]),
        T5.node("Z3", "Z3", [
            T5.node("X3", "X3", [T5.leaf("Y3", "Y3")])])])
  alignment45 = {["409"]: 307, ["401"]: 301, ["402"]: 302, ["403"]: 303, ["404"]: 304, ["406"]: 305, ["407"]: 306}
  
  eq(tostring(tree1) + " / " + tostring(tree2) + " aligns",
    builtins.equiv(get-one-alignment(tree1, tree2, align(tree1, tree2)), alignment12), true)
  eq(tostring(tree2) + " / " + tostring(tree1) + " doesn't align",
    builtins.equiv(get-one-alignment(tree2, tree1, align(tree2, tree1)), {}), true)
  eq(tostring(tree3) + " / " + tostring(tree1) + " doesn't align",
    builtins.equiv(get-one-alignment(tree3, tree1, align(tree3, tree1)), {}), true)
  eq(tostring(tree4) + " / " + tostring(tree5) + " aligns",
    builtins.equiv(get-one-alignment(tree4, tree5, align(tree4, tree5)), alignment45), true)
  eq(tostring(tree5) + " / " + tostring(tree4) + " doesn't align",
    builtins.equiv(get-one-alignment(tree5, tree4, align(tree5, tree4)), {}), true)
end

fun map-concat(f, lst):
  fun help(l, acc):
    if list.is-empty(l): acc
    else: help(l.rest, f(l.first).append(acc))
    end
  end
  help(lst, [])
where:
  eq = checkers.check-equals
  eq("[1,2,3] => [3,6,2,4,1,2]", map-concat(fun(x): [x, 2*x] end, [1,2,3]), [3,6,2,4,1,2])
end

fun get-all-alignments(t1, t2, edits):
  start-hash = t1.short-hash-key() + "," + t2.short-hash-key()
  fun help(start, alignment):
    when (edits.[start].cost > 0):
      raise("start = " + tostring(start) + " edits = " + tostring(edits.[start]))
    end
    actions =
      if builtins.has-field(edits, start): edits.[start].actions
      else: []
      end
    for map-concat(action from actions):
      if is-teaRename(action):
        alignment2 = alignment.{[tostring(action.newV.id)]: action.oldV.id}
        for map-concat(alignment3 from help(action.kidsPrevKey, alignment2)):
          help(action.restPrevKey, alignment3)
        end
      else if is-teaSame(action): [alignment]
      else: help(action.prevKey, alignment)
      end
    end
  end
  if edits.[start-hash].cost > 0: []
  else: help(start-hash, {})
  end
where:
  eq = checkers.check-equals
  
  T1 = TreeMaker(0)
  tree1 =
    T1.node("Root", "Root", [
        T1.node("X", "X", [T1.leaf("X", "X")]),
        T1.node("X", "X", [T1.leaf("X", "X")])])
  T2 = TreeMaker(100)
  tree2 =
    T2.node("Root", "Root", [
        T2.node("X", "X", [
            T2.node("Y", "Y", [T2.node("Y", "Y", [T2.node("Y", "Y", [T2.leaf("X", "X")])])]),
            T2.node("Y", "Y", [T2.node("Y", "Y", [T2.node("Y", "Y", [T2.leaf("X", "X")])])])]),
        T2.node("X", "X", [
            T2.node("Z", "Z", [T2.leaf("X", "X")])])])
  
  aligned = align(tree1, tree2)
  alignments = get-all-alignments(tree1, tree2, aligned)
  eq(tostring(tree1) + " / " + tostring(tree2) + " aligns two ways", alignments.length(), 2)
  eq("Alignment 1 is { 110: 3, 112: 4, 101: 1, 113: 5, 109: 2 }",
    builtins.equiv(alignments.get(0), { ["110"]: 3, ["112"]: 4, ["113"]: 5, ["105"]: 1, ["109"]: 2 }),
    true)
  eq("Alignment 2 is { 110: 3, 112: 4, 101: 1, 113: 5, 109: 2 }",
    builtins.equiv(alignments.get(1), { ["110"]: 3, ["112"]: 4, ["113"]: 5, ["101"]: 1, ["109"]: 2 }),
    true)
end



