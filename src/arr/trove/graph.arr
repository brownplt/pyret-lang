provide {
  tree-node: tree-node,
  vertex: vertex,
  edge: edge,
  graph: graph,
  graph-options: graph-options,
  edge-options: edge-options,
  vertex-options: vertex-options,
  show-graph: show-graph,
  show-tree: show-tree
} end

provide-types {
  Vertex:: Vertex,
  Edge:: Edge,
  Graph:: Graph,
  Tree:: Tree
}

import image-structs as I
import graph-structs as G
import graph-lib as J

type TreeOptions = G.TreeOptions
type VertexOptions = G.VertexOptions
type EdgeOptions = G.EdgeOptions
type GraphOptions = G.GraphOptions
type Graph = G.Graph
type Vertex = G.Vertex
type Edge = G.Edge
type Tree = G.Tree

edge = G.edge
vertex = G.vertex
graph = G.graph
tree-node = G.tree-node

tree-options = {
  node-printer: lam(v :: Tree) -> String: tostring(v.value);
}

vertex-options = {
  color: I.gray
}

edge-options = {
  color: I.gray,
  directed: true
}

graph-options :: GraphOptions = {
  vertex-printer: lam(v :: Vertex) -> String: tostring(v.value);,
  edge-printer: lam(v :: Edge) -> String: tostring(v.value);
}

####################
### tree-diagram ###
####################

fun show-tree(tree :: Tree, options :: TreeOptions) -> Tree:
  doc: "Show the structure of Tree as a tree"
  J.tree-diagram(tree, options)
  tree
end

####################
### force-layout ###
####################

fun show-graph<a, b>(g :: Graph<a, b>) -> Graph<a, b>:
  doc: "Show the structure of Graph"
  J.force-layout(g)
  g
end

fun test-graph():
  blue-castle = vertex("Blue Castle", vertex-options.{color: I.blue})
  red-castle = vertex("Red Castle", vertex-options.{color: I.red})
  church = vertex("Church", vertex-options.{color: I.white})
  inn = vertex("Inn", vertex-options.{color: I.yellow})
  e1 = edge(blue-castle, inn, "bridge 1", edge-options.{color: I.black, directed: false})
  e2 = edge(blue-castle, inn, "bridge 2", edge-options.{color: I.red, directed: false})
  e3 = edge(blue-castle, church, "bridge 3", edge-options.{color: I.green})
  e4 = edge(inn, church, "bridge 4", edge-options.{color: I.blue})
  e5 = edge(red-castle, inn, "bridge 5", edge-options.{color: I.gray})
  e6 = edge(red-castle, inn, "bridge 6", edge-options.{color: I.gold})
  e7 = edge(red-castle, church, "bridge 7", edge-options.{color: I.yellow})
  show-graph(graph([list: church, blue-castle, red-castle, inn], [list: e1, e2, e3, e4, e5, e6, e7], graph-options))
end

fun test-tree():
  A = tree-node("Node A", empty)
  F = tree-node("Node F", empty)
  E = tree-node("Node E", [list: F])
  B = tree-node("Node B", [list: E])
  C = tree-node("Node C", [list: E, B])
  root = tree-node("This is the root", [list: A, B, C])
  show-tree(root, tree-options)
end
