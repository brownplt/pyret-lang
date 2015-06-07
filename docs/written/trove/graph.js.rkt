#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")
@(require (only-in scribble/core delayed-block))

@(define (type T) (a-id T (xref "graph" T)))
@(define (Color) (a-id "Color" (xref "image-structs" "Color")))
@(define (SD-of typ) (a-app (a-id "StringDict" (xref "string-dict" "StringDict")) typ))
@(define (Tree-of typ) (a-app (a-id "Tree" (xref "graph" "Tree")) typ))
@(define (Graph-of typ1 typ2) (a-app (a-id "Graph" (xref "graph" "Graph")) typ1 typ2))
@(define (Vertex-of typ) (a-app (a-id "Vertex" (xref "graph" "Vertex")) typ))
@(define (Edge-of typ1 typ2) (a-app (a-id "Edge" (xref "graph" "Edge")) typ1 typ2))
@(define (no-variant T v) @list{@type-spec[T (list)]
  There are no variants for @pyret-id[T], and programs cannot use @pyret{cases} statements with @pyret-id[T].
  Instead, a @pyret-id[T] value named @tt[v] is provided, and a new @pyret-id[T]
  can be constructed via the @;@secref{"s:extend-expr"}
  Extend Expression.})

@(append-gen-docs
  `(module "graph"
    (path "src/js/base/runtime-anf.js")
    (fun-spec (name "show-graph"))
    (fun-spec (name "show-tree"))
    (type-spec (name "TreeOptions"))
    (type-spec (name "GraphOptions"))
    (type-spec (name "VertexOptions"))
    (type-spec (name "EdgeOptions"))
    (data-spec
      (name "Graph")
      (variants ("graph")))
    (data-spec
      (name "Vertex")
      (variants ("vertex")))
    (data-spec
      (name "Edge")
      (variants ("vertex")))
    (data-spec
      (name "Tree")
      (variants ("tree-node")))
  ))

@docmodule["graph"]{
  The Pyret Graph library. It consists of graph and tree visualization tools.

  @section{The Vertex Type}

  @data-spec2["Vertex" (list "a") (list
  @constructor-spec["Vertex" "vertex" (list `("value" ("type" "normal") ("contract" ,"a"))
                                            `("options" ("type" "normal") ("contract" ,(type "VertexOptions"))))])]

  @nested[#:style 'inset]{
  @constructor-doc["Vertex" "vertex" (list `("value" ("type" "normal") ("contract" ,"a"))
                                           `("options" ("type" "normal") ("contract" ,(type "VertexOptions")))) (Vertex-of "a")]{
    A vertex of a graph
    @member-spec["value" #:type "normal" #:contract "a"]{
      Arbitrary data.
    }
    @member-spec["options" #:type "normal" #:contract (type "VertexOptions")]
  }
  }

  @section{The VertexOptions Type and vertex-options Value}
  @no-variant["VertexOptions" "vertex-options"]

  @pyret-id{VertexOptions} consists of the following fields:
  @a-record[(a-field "color" (Color))]

  The value of @tt{vertex-options} is @tt{{color: gray}}

  @examples{
    import image-structs as I
    my-vertex-options = vertex-options.{color: I.red}
  }

  @section{The Edge Type}

  @data-spec2["Edge" (list "a" "b") (list
  @constructor-spec["Edge" "edge" (list `("source" ("type" "normal") ("contract" ,(Vertex-of "a")))
                                        `("target" ("type" "normal") ("contract" ,(Vertex-of "a")))
                                        `("value" ("type" "normal") ("contract" ,"a"))
                                        `("options" ("type" "normal") ("contract" ,(type "EdgeOptions"))))])]

  @nested[#:style 'inset]{
  @constructor-doc["Edge" "edge" (list `("source" ("type" "normal") ("contract" ,(Vertex-of "a")))
                                       `("target" ("type" "normal") ("contract" ,(Vertex-of "a")))
                                       `("value" ("type" "normal") ("contract" ,"a"))
                                       `("options" ("type" "normal") ("contract" ,(type "EdgeOptions")))) (Edge-of "a" "b")]{
    An edge of a graph. This could be both undirected edge or directed edge, depending on its `options`.
    @member-spec["source" #:type "normal" #:contract (Vertex-of "a")]{
      A vertex. This must a reference of an existing @pyret-id{Vertex}. If the edge's `options` specifies that this edge
      is directed, this vertex is the source of the edge.
    }
    @member-spec["target" #:type "normal" #:contract (Vertex-of "a")]{
      A vertex. This must a reference of an existing @pyret-id{Vertex}. If the edge's `options` specifies that this edge
      is directed, this vertex is the target of the edge.
    }
    @member-spec["value" #:type "normal" #:contract "b"]{
      Arbitrary data.
    }
    @member-spec["options" #:type "normal" #:contract (type "EdgeOptions")]
  }

  }

  @section{The EdgeOptions Type and edge-options Value}
  @no-variant["EdgeOptions" "edge-options"]

  @pyret-id{EdgeOptions} consists of the following fields:
  @a-record[(a-field "color" (Color)) (a-field "directed" B)]

  The value of @tt{vertex-options} is @tt{{color: gray, directed: true}}

  @examples{
    import image-structs as I
    undirected-edge-options = edge-options.{directed: false}
  }

  @section{The Graph Type}

  @data-spec2["Graph" (list "a" "b") (list
  @constructor-spec["Graph" "graph" (list `("vertices" ("type" "normal") ("contract" ,(L-of (Vertex-of "a"))))
                                          `("edges" ("type" "normal") ("contract" ,(L-of (Edge-of "a" "b"))))
                                          `("options" ("type" "normal") ("contract" ,(type "GraphOptions"))))])]

  @nested[#:style 'inset]{
  @constructor-doc["Graph" "graph" (list `("vertices" ("type" "normal") ("contract" ,(L-of (Vertex-of "a"))))
                                         `("edges" ("type" "normal") ("contract" ,(L-of (Edge-of "a" "b"))))
                                         `("options" ("type" "normal") ("contract" ,(type "GraphOptions")))) (Graph-of "a" "b")]{
    A graph.
    @member-spec["vertices" #:type "normal" #:contract (L-of (Vertex-of "a"))]{
      A list of vertices.
    }
    @member-spec["edges" #:type "normal" #:contract (L-of (Edge-of "a" "b"))]{
      A list of edges. Edges' `source` and `target` must be references of @pyret-id{Vertex} in the `vertices` list. In other words,
      For all `a` which is Edges' `source` and `target`, there must be a @pyret-id{Vertex} `b` in `vertices` such that
      @pyret{identical(a, b)}.
    }
  }
  }

  @section{The GraphOptions Type and graph-options Value}
  @no-variant["GraphOptions" "graph-options"]

  @pyret-id{GraphOptions} consists of the following fields:
  @a-record[(a-field "vertex-printer" (a-arrow (type "Vertex") S)) (a-field "edge-printer" (a-arrow (type "Edge") S))]

  The value of @tt{graph-options} is @tt{{vertex-printer: lam(v :: Vertex) -> String: tostring(v.value);,
                                          edge-printer: lam(v :: Edge) -> String: tostring(v.value);}}

  @examples{
    edge-printer = lam(e):
        'From: ' + graph-options.vertex-printer(e.source) + '\n' +
        'To: ' + graph-options.vertex-printer(e.target) + '\n' +
        'Value: ' + graph-options.edge-printer(e)
      end
    my-graph-options = graph-options.{edge-printer: edge-printer}
  }

  @section{The Tree Type}

  @data-spec2["Tree" (list "a") (list
  @constructor-spec["Tree" "tree-node" (list `("value" ("type" "normal") ("contract" ,"a"))
                                             `("children" ("type" "normal") ("contract" ,(L-of (Tree-of "a")))))])]

  @nested[#:style 'inset]{
  @constructor-doc["Tree" "tree-node" (list `("value" ("type" "normal") ("contract" ,"a"))
                                            `("children" ("type" "normal") ("contract" ,(L-of (Tree-of "a"))))) (Tree-of "a")]{
    A node of a tree.
    @member-spec["value" #:type "normal" #:contract "a"]{
      Arbitrary data.
    }
    @member-spec["children" #:type "normal" #:contract (L-of (Tree-of "a"))]{
      A list of the node's children
    }
  }
  }

  @section{The TreeOptions Type and tree-options Value}
  @no-variant["TreeOptions" "tree-options"]

  @pyret-id{TreeOptions} consists of the following fields:
  @a-record[(a-field "node-printer" (a-arrow (type "Tree") S))]

  The value of @tt{tree-options} is @tt{{node-printer: lam(v :: Node) -> String: tostring(v.value);}}

  @section{Graph Functions}

  @function["show-graph"
    #:contract (a-arrow (Graph-of "a" "b") (Graph-of "a" "b"))
    #:args (list (list "g" #f))
    #:return (Graph-of "a" "b")
  ]{

  Display the graph

  }

  @function["show-tree"
    #:contract (a-arrow (Tree-of "a") (type "TreeOptions") (Tree-of "a"))
    #:args (list (list "tree" #f) (list "options" #f))
    #:return (Tree-of "a")
  ]{

  Display the tree in tree structure.
  }
}
