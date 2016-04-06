provide *
provide-types *

import image-structs as I

type TreeOptions = {
  node-printer :: (Tree -> String)
}

type VertexOptions = {
  color :: I.Color
}

type EdgeOptions = {
  color :: I.Color,
  directed :: Boolean
}

type GraphOptions = {
  vertex-printer :: (Vertex -> String),
  edge-printer :: (Edge -> String)
}

data Tree<a>:
  | node(value :: a, children :: List<Tree<a>>)
end

data Vertex<a>:
  | vertex(value :: a, options :: VertexOptions)
end

data Edge<a, b>:
  | edge(
      source :: Vertex<a>,
      target :: Vertex<a>,
      value :: b,
      options :: EdgeOptions)
end

data Graph<a, b>:
  | graph(
      vertices :: List<Vertex<a>>,
      edges :: List<Edge<a, b>>)
end
