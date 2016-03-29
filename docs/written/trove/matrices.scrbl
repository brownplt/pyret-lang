#lang scribble/base
@(require "../../scribble-api.rkt"
          (except-in "../abbrevs.rkt" L-of))
@(require (only-in scribble/core delayed-block)
          (only-in scribble/manual math))

@; Adapted from http://con.racket-lang.org/2011/pr-slides.pdf
@; by Prabhakar Ragde
@(require scribble/html-properties
         scribble/base
         scribble/core
         (prefix-in net: net/url))
 
@(provide mathjax-style math-in math-disp $ $$)
 
@(define mathjax-source
@(net:string->url "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"))

@(define mathjax-style
    (style #f (list (js-addition mathjax-source))))
@(define (mymath start end . strs)
@(make-element (make-style "relax" '(exact-chars)) `(,start ,@strs ,end)))
 
@(define (math-in . strs)
@(apply mymath "\\(" "\\)" strs))
 
@(define (math-disp . strs)
@(apply mymath "\\[" "\\]" strs))

@; Creates a LaTeX inline environment
@(define (math-in-env name . strs)
@(apply mymath (string-append "\\(\\begin{" name "}") (string-append "\\end{" name "}\\)") strs))

@; Creates a LaTeX environment
@(define (math-disp-env name . strs)
@(apply mymath (string-append "\\[\\begin{" name "}") (string-append "\\end{" name "}\\]") strs))

@; Creates a Displayed Matrix
@(define (math-mtx . strs)
@(apply math-disp-env (cons "bmatrix" strs)))

@; Creates an Inlined Matrix
@(define (math-imtx . strs)
@(apply mymath "\\(\\left[\\begin{smallmatrix}" "\\end{smallmatrix}\\right]\\)" strs))
 
@(define $ math-in)
@(define $$ math-disp) 




@(define (matrix-method name #:args (args #f) #:return (return #f) #:contract (contract #f))
  (method-doc "Matrix" "matrix" name #:alt-docstrings "" #:args args #:return return #:contract contract))

@(define mtx-type (a-id "Matrix" (xref "matrices" "Matrix")))
@(define vec-type (a-id "Vector" (xref "matrices" "Vector")))
@(define (L-of typ) `(a-app (a-id "List" (xref "lists" "List")) ,typ))

@(append-gen-docs
  `(module "matrices"
    (path "src/js/base/runtime-anf.js")
    (fun-spec
      (name "is-matrix")
      (args ("val"))
      (arity 1)
      (return ,B)
      (contract (a-arrow ,A ,B)))
    (fun-spec
      (name "vector")
      (arity 1)
      (args ("elts"))
      (return ,vec-type)
      (contract (a-arrow (a-id "Number..." (xref "<globals>" "Number")) ,vec-type)))
    (fun-spec
      (name "dot")
      (arity 2)
      (args ("left" "right"))
      (return ,N)
      (contract (a-arrow ,vec-type ,vec-type ,N)))
    (fun-spec
      (name "cross")
      (arity 2)
      (args ("v" "w"))
      (return ,vec-type)
      (contract (a-arrow ,vec-type ,vec-type ,vec-type)))
    (fun-spec
      (name "vector-add")
      (arity 2)
      (args ("left" "right"))
      (return ,vec-type)
      (contract (a-arrow ,vec-type ,vec-type ,vec-type)))
    (fun-spec
      (name "vector-sub")
      (arity 2)
      (args ("left" "right"))
      (return ,vec-type)
      (contract (a-arrow ,vec-type ,vec-type ,vec-type)))
    (fun-spec
      (name "magnitude")
      (arity 1)
      (args ("vec"))
      (return ,N)
      (contract (a-arrow ,vec-type ,N)))
    (fun-spec
      (name "normalize")
      (arity 1)
      (args ("vec"))
      (return ,vec-type)
      (contract (a-arrow ,vec-type ,vec-type)))
    (fun-spec
      (name "scale")
      (arity 2)
      (args ("vec" "factor"))
      (return ,vec-type)
      (contract (a-arrow ,vec-type ,N ,vec-type)))
    (fun-spec
      (name "is-row-matrix")
      (arity 1)
      (args ("mtx"))
      (return ,B)
      (contract (a-arrow ,mtx-type ,B)))
    (fun-spec
      (name "is-col-matrix")
      (arity 1)
      (args ("mtx"))
      (return ,B)
      (contract (a-arrow ,mtx-type ,B)))
    (fun-spec
      (name "is-square-matrix")
      (arity 1)
      (args ("mtx"))
      (return ,B)
      (contract (a-arrow ,mtx-type ,B)))
    (fun-spec
      (name "identity-matrix")
      (arity 1)
      (args ("n"))
      (return ,mtx-type)
      (contract (a-arrow ,N ,mtx-type)))
    (fun-spec
      (name "make-matrix")
      (arity 3)
      (args ("rows" "cols" "elt"))
      (return ,mtx-type)
      (contract (a-arrow ,N ,N ,N ,mtx-type)))
    (fun-spec
      (name "build-matrix")
      (arity 3)
      (args ("rows" "cols" "proc"))
      (return ,mtx-type)
      (contract (a-arrow ,N ,N (a-arrow ,N ,N ,N) ,mtx-type)))
    (fun-spec
      (name "vector-to-matrix")
      (arity 1)
      (args ("v"))
      (return ,mtx-type)
      (contract (a-arrow ,vec-type ,mtx-type)))
    (fun-spec
      (name "list-to-matrix")
      (arity 3)
      (args ("rows" "cols" "lst"))
      (return ,mtx-type)
      (contract (a-arrow ,N ,N (a-app (a-id "List" (xref "lists" "List")) ,N) ,mtx-type)))
    (fun-spec
      (name "list-to-row-matrix")
      (arity 1)
      (args ("lst"))
      (return ,mtx-type)
      (contract (a-arrow (a-app (a-id "List" (xref "lists" "List")) ,N) ,mtx-type)))
    (fun-spec
      (name "list-to-col-matrix")
      (arity 1)
      (args ("lst"))
      (return ,mtx-type)
      (contract (a-arrow (a-app (a-id "List" (xref "lists" "List")) ,N) ,mtx-type)))
    (fun-spec
      (name "lists-to-matrix")
      (arity 1)
      (args ("lst"))
      (return ,mtx-type)
      (contract (a-arrow (a-app (a-id "List" (xref "lists" "List")) ,N) ,mtx-type)))
    (fun-spec
      (name "vectors-to-matrix")
      (arity 1)
      (args ("lst"))
      (return ,mtx-type)
      (contract (a-arrow (a-app (a-id "List" (xref "lists" "List")) ,vec-type) ,mtx-type)))
    (fun-spec
      (name "matrix-within")
      (arity 1)
      (args ("delta"))
      (return (a-arrow ,mtx-type ,mtx-type ,B))
      (contract (a-arrow ,N (a-arrow ,mtx-type ,mtx-type ,B))))
    (data-spec
      (name "Vector"))
    (data-spec
      (name "Matrix")
      (variants ("matrix"))
      (constr-spec
        (name "matrix")
        (members
          (("rows" (type normal) (contract N))
           ("cols" (type normal) (contract N))
           ("elts" (type normal) (contract RA))))
        (with-members (
          (method-spec 
            (name "rc-to-index")
            (arity 3)
            (args ("self" "i" "j"))
            (return ,N)
            (contract (a-arrow ,mtx-type ,N ,N ,N)))
          (method-spec 
            (name "get")
            (arity 3)
            (args ("self" "i" "j"))
            (return ,N)
            (contract (a-arrow ,mtx-type ,N ,N ,N)))
          (method-spec 
            (name "to-list")
            (arity 1)
            (args ("self"))
            (return ,(L-of N))
            (contract (a-arrow ,mtx-type ,(L-of N))))
          (method-spec 
            (name "to-vector")
            (arity 1)
            (args ("self"))
            (return ,vec-type)
            (contract (a-arrow ,mtx-type ,vec-type)))
          (method-spec 
            (name "to-lists")
            (arity 1)
            (args ("self"))
            (return ,(L-of (L-of N)))
            (contract (a-arrow ,mtx-type ,(L-of (L-of N)))))
          (method-spec 
            (name "to-vectors")
            (arity 1)
            (args ("self"))
            (return ,(L-of vec-type))
            (contract (a-arrow ,mtx-type ,(L-of vec-type))))
          (method-spec 
            (name "row")
            (arity 2)
            (args ("self" "i"))
            (return ,mtx-type)
            (contract (a-arrow ,mtx-type ,N ,mtx-type)))
          (method-spec 
            (name "col")
            (arity 2)
            (args ("self" "j"))
            (return ,mtx-type)
            (contract (a-arrow ,mtx-type ,N ,mtx-type)))
          (method-spec 
            (name "submatrix")
            (arity 3)
            (args ("self" "loi" "loj"))
            (return ,mtx-type)
            (contract (a-arrow ,mtx-type ,(L-of N) ,(L-of N) ,mtx-type)))
          (method-spec 
            (name "transpose")
            (arity 1)
            (args ("self"))
            (return ,mtx-type)
            (contract (a-arrow ,mtx-type ,mtx-type)))
          (method-spec 
            (name "diagonal")
            (arity 1)
            (args ("self"))
            (return ,mtx-type)
            (contract (a-arrow ,mtx-type ,mtx-type)))
          (method-spec 
            (name "upper-triangle")
            (arity 1)
            (args ("self"))
            (return ,mtx-type)
            (contract (a-arrow ,mtx-type ,mtx-type)))
          (method-spec 
            (name "lower-triangle")
            (arity 1)
            (args ("self"))
            (return ,mtx-type)
            (contract (a-arrow ,mtx-type ,mtx-type)))
          (method-spec 
            (name "row-list")
            (arity 1)
            (args ("self"))
            (return ,(L-of mtx-type))
            (contract (a-arrow ,mtx-type ,(L-of mtx-type))))
          (method-spec 
            (name "col-list")
            (arity 1)
            (args ("self"))
            (return ,(L-of mtx-type))
            (contract (a-arrow ,mtx-type ,(L-of mtx-type))))
          (method-spec 
            (name "map")
            (arity 2)
            (args ("self" "func"))
            (return ,mtx-type)
            (contract (a-arrow ,mtx-type (a-arrow ,N ,N) ,mtx-type)))
          (method-spec 
            (name "row-map")
            (arity 2)
            (args ("self" "func"))
            (return ,mtx-type)
            (contract (a-arrow ,mtx-type (a-arrow ,mtx-type ,mtx-type) ,mtx-type)))
          (method-spec 
            (name "col-map")
            (arity 2)
            (args ("self" "func"))
            (return ,mtx-type)
            (contract (a-arrow ,mtx-type (a-arrow ,mtx-type ,mtx-type) ,mtx-type)))
          (method-spec 
            (name "augment")
            (arity 2)
            (args ("self" "other"))
            (return ,mtx-type)
            (contract (a-arrow ,mtx-type ,mtx-type ,mtx-type)))
          (method-spec 
            (name "stack")
            (arity 2)
            (args ("self" "other"))
            (return ,mtx-type)
            (contract (a-arrow ,mtx-type ,mtx-type ,mtx-type)))
          (method-spec 
            (name "trace")
            (arity 1)
            (args ("self"))
            (return ,N)
            (contract (a-arrow ,mtx-type ,N)))
          (method-spec 
            (name "scale")
            (arity 2)
            (args ("self" "factor"))
            (return ,mtx-type)
            (contract (a-arrow ,mtx-type ,N ,mtx-type)))
          (method-spec 
            (name "dot")
            (arity 2)
            (args ("self" "other"))
            (return ,N)
            (contract (a-arrow ,mtx-type ,mtx-type ,N)))
          (method-spec 
            (name "_plus")
            (arity 2)
            (args ("self" "other"))
            (return ,mtx-type)
            (contract (a-arrow ,mtx-type ,mtx-type ,mtx-type)))
          (method-spec 
            (name "_minus")
            (arity 2)
            (args ("self" "other"))
            (return ,mtx-type)
            (contract (a-arrow ,mtx-type ,mtx-type ,mtx-type)))
          (method-spec 
            (name "_times")
            (arity 2)
            (args ("self" "other"))
            (return ,mtx-type)
            (contract (a-arrow ,mtx-type ,mtx-type ,mtx-type)))
          (method-spec 
            (name "expt")
            (arity 2)
            (args ("self" "power"))
            (return ,mtx-type)
            (contract (a-arrow ,mtx-type ,N ,mtx-type)))
          (method-spec 
            (name "determinant")
            (arity 1)
            (args ("self"))
            (return ,N)
            (contract (a-arrow ,mtx-type ,N)))
          (method-spec 
            (name "is-invertible")
            (arity 1)
            (args ("self"))
            (return ,B)
            (contract (a-arrow ,mtx-type ,B)))
          (method-spec 
            (name "rref")
            (arity 1)
            (args ("self"))
            (return ,mtx-type)
            (contract (a-arrow ,mtx-type ,mtx-type)))
          (method-spec 
            (name "inverse")
            (arity 1)
            (args ("self"))
            (return ,mtx-type)
            (contract (a-arrow ,mtx-type ,mtx-type)))
          (method-spec 
            (name "solve")
            (arity 2)
            (args ("self" "other"))
            (return ,mtx-type)
            (contract (a-arrow ,mtx-type ,mtx-type ,mtx-type)))
          (method-spec 
            (name "least-squares-solve")
            (arity 2)
            (args ("self" "other"))
            (return ,mtx-type)
            (contract (a-arrow ,mtx-type ,mtx-type ,mtx-type)))
          (method-spec 
            (name "lp-norm")
            (arity 2)
            (args ("self" "power"))
            (return ,N)
            (contract (a-arrow ,mtx-type ,N ,mtx-type)))
          (method-spec 
            (name "l1-norm")
            (arity 1)
            (args ("self"))
            (return ,N)
            (contract (a-arrow ,mtx-type ,N)))
          (method-spec 
            (name "l2-norm")
            (arity 1)
            (args ("self"))
            (return ,N)
            (contract (a-arrow ,mtx-type ,N)))
          (method-spec 
            (name "l-inf-norm")
            (arity 1)
            (args ("self"))
            (return ,N)
            (contract (a-arrow ,mtx-type ,N)))
          (method-spec 
            (name "qr-decomposition")
            (arity 1)
            (args ("self"))
            (return ,(L-of mtx-type))
            (contract (a-arrow ,mtx-type ,(L-of mtx-type))))
          (method-spec 
            (name "gram-schmidt")
            (arity 1)
            (args ("self"))
            (return ,mtx-type)
            (contract (a-arrow ,mtx-type ,mtx-type)))
          (method-spec 
            (name "_torepr")
            (arity 1)
            (args ("self"))
            (return ,S)
            (contract (a-arrow ,mtx-type ,S)))
          (method-spec 
            (name "equalTo") ;TODO: Get rid of camelCase
            (arity 2)
            (args ("self" "other"))
            (return ,B)
            (contract (a-arrow ,mtx-type ,mtx-type ,B)))
          (method-spec 
            (name "_equals")
            (arity 2)
            (args ("self" "other"))
            (return ,B)
            (contract (a-arrow ,mtx-type ,mtx-type ,B)))
          ))))))

@docmodule["matrices"]{
@section{The Vector Datatype}

@type-spec["Vector" '()]

The @pyret{Vector} type represents mathematical vectors. The datatype is equivalent to that of a @pyret{list} of numbers.

@section{The @pyret{vector} Constructor}
@collection-doc["vector" #:contract `(a-arrow ("elt" ,N) ,vec-type)]

Vector constructor which works in an identical manner to the @pyret{list} constructor.
@para{@bold{WARNING:} Matrix rows and columns are @bold{1-indexed}.}

@section[#:style mathjax-style]{The Matrix Datatype}
@data-spec["Matrix" (list
  @constructor-spec["Matrix" "matrix" (list `("rows" ("type" "normal") ("contract" ,N))
                                          `("cols" ("type" "normal") ("contract" ,N))
                                          `("elts" ("type" "normal") ("contract" ,RA)))])]

  The internal representation of matrices is not exposed, but, for reference, it is provided here.

  @nested[#:style 'inset]{
  @constructor-doc["Matrix" "matrix" (list `("rows" ("type" "normal") ("contract" ,N))
                                           `("cols" ("type" "normal") ("contract" ,N))
                                           `("elts" ("type" "normal") ("contract" ,RA)))
                                     (a-id "Matrix" (xref "matrices" "Matrix"))]{
  }

  @function["is-matrix" #:alt-docstrings ""]

  }

@section{Matrix Constructors}

@collection-doc["matrix(rows,cols)" #:contract `(a-arrow ("elt" ,N) ,mtx-type)]

Publicly exposed constructor which constructs a matrix of size 
@pyret{rows} by @pyret{cols} with the given elements, entered row by row.

The following example represents the matrix @math-imtx{1 & 2 & 3 \\ 4 & 5 & 6}:

@examples{
[matrix(2,3): 1, 2, 3, 4, 5, 6]
}

@collection-doc["row-matrix" #:contract `(a-arrow ("elt" ,N) ,mtx-type)]

Constructor which returns a one-row matrix containing the given entries.

The following will construct the matrix @math-imtx{1 & 2 & 3}:

@examples{
check:
  [row-matrix: 1, 2, 3] is [matrix(1,3): 1, 2, 3]
end
}

@collection-doc["col-matrix" #:contract `(a-arrow ("elt" ,N) ,mtx-type)]

Constructor which returns a one-column matrix containing the given entries.

The following will construct the matrix @math-imtx{1 \\ 2 \\ 3}:

@examples{
check:
  [col-matrix: 1, 2, 3] is [matrix(3,1): 1, 2, 3]
end
}

@function["identity-matrix"]

Constructs an @pyret{n}@math-in{\times}@pyret{n} identity matrix.

@examples{
check:
  identity-matrix(2) is [matrix(2,2): 1, 0, 0, 1]
  identity-matrix(3) is [matrix(3,3): 1, 0, 0, 0, 1, 0, 0, 0, 1]
end
}

@function["make-matrix"]

Constructs a matrix of the given size using only the given element.

@examples{
check:
  make-matrix(2, 3, 1) is [matrix(2,3): 1, 1, 1, 1, 1, 1]
  make-matrix(3, 2, 5) is [matrix(3,2): 5, 5, 5, 5, 5, 5]
end
}

@function["build-matrix"]

Constructs a matrix of the given size, where entry @math{(i,j)} is the result of @pyret{proc(i,j)}.

@examples{
check:
  build-matrix(2, 3, lam(i,j): i + j end) is [matrix(3,2): 0, 1, 1, 2, 2, 3]
end
}

@section{Matrix Methods}

These methods are available on all matrices.

@matrix-method["get"]

Returns the matrix's entry in the @math{i^th} row and the @math{j^th} column.

@examples{
check:
  [matrix(3,2): 1, 2, 3, 4, 5, 6].get(2,2) is 4
  [matrix(3,2): 1, 2, 3, 4, 5, 6].get(3,1) is 5
end
}

@matrix-method["to-list"]

Returns the matrix as a list of numbers (this is just the internal
@pyret{RawArray} representation written as a list).

For example, given the matrix @math-imtx{2 & 4 & 6 \\ 8 & 10 & 12 \\ 14 & 16 & 18}:

@examples{
check:
  [matrix(3,3): 2, 4, 6, 8, 10, 12, 14, 16, 18].to-list() is
  [list: 2, 4, 6, 8, 10, 12, 14, 16, 18]
end
}

@matrix-method["to-vector"]

Returns a one-row/one-column matrix as a vector.

@examples{
check:
  [matrix(2,1): 4, 5].to-vector() is [vector: 4, 5]
  [matrix(1,2): 4, 5].to-vector() is [matrix(2,1): 4, 5].to-vector()
end
}

@matrix-method["to-lists"]

Returns the matrix as a list of lists of numbers, with each list
corresponding to one row.

@examples{
check:
  [matrix(2,3): 1, 2, 3, 4, 5, 6].to-lists() is
  [list: [list: 1, 2, 3],
         [list: 4, 5, 6]]
end
}

@matrix-method["to-vectors"]

Returns the matrix as a list of lists of numbers (i.e. a list of @pyret{Vector}s), 
with each list corresponding to one column.

For example, the matrix @math-imtx{1 & 2 & 3 \\ 4 & 5 & 6} corresponds to the
vectors @math-imtx{1 \\ 4}, @math-imtx{2 \\ 5}, and @math-imtx{3 \\ 6}: 

@examples{
check:
  [matrix(2,3): 1, 2, 3, 4, 5, 6].to-vectors() is
  [list: [list: 1, 4],
         [list: 2, 5],
         [list: 3, 6]]
end
}

@matrix-method["row"]

Returns a one-row matrix with the matrix's given row.

@examples{
check:
  [matrix(2,3): 1, 2, 3, 4, 5, 6].row(2) is
  [matrix(1,3): 4, 5, 6]

  [matrix(3,3): 1, 2, 3, 4, 5, 6, 7, 8, 9].row(3) is
  [matrix(1,3): 7, 8, 9]
end
}

@matrix-method["col"]

Returns a one-column matrix with the matrix's given column.

@examples{
check:
  [matrix(2,3): 1, 2, 3, 4, 5, 6].col(2) is
  [matrix(2,1): 2, 5]

  [matrix(3,3): 1, 2, 3, 4, 5, 6, 7, 8, 9].col(3) is
  [matrix(3,1): 3, 6, 9]
end
}

@matrix-method["submatrix"]

Returns the submatrix of the matrix comprised of the intersection
of the given list of rows and the given list of columns.

For example, if our list of rows is @math-in{\{1, 2\}} and our
list of columns is @math-in{\{2, 3\}}, then the positions in the
resulting submatrix will be the elements with @math-in{(row,col)} positions
@math-in{\{(1, 2), (1, 3), (2, 2), (2, 3)\}}.

@math-in{
\left[\begin{matrix} 
            a_{11} & a_{12} & a_{13} \\
            a_{21} & a_{22} & a_{23} \\
            a_{31} & a_{32} & a_{33}
            \end{matrix}\right]}@pyret{.submatrix([list: 1, 2], [list: 2, 3])}
                                     @math-in{=
\left[\begin{matrix}
a_{12} & a_{13} \\
a_{22} & a_{23}\end{matrix}\right]}

This is shown in the below example:

@examples{
check:
  [matrix(3,3): 1, 2, 3, 4, 5, 6, 7, 8, 9].submatrix([list: 1, 2], [list: 2, 3]) is
  [matrix(2,2): 2, 3, 4, 5]
end
}

@matrix-method["transpose"]

Returns the transposition of the matrix. For example,
@math-disp{\begin{bmatrix}1 & 2 & 3 \\ 4 & 5 & 6\end{bmatrix}
                 \overrightarrow{Transpose}
                 \begin{bmatrix}1 & 4 \\ 2 & 5 \\ 3 & 6\end{bmatrix}}

@examples{
check:
  [matrix(2,3): 1, 2, 3, 4, 5, 6].transpose() is
  [matrix(3,2): 1, 4, 2, 5, 3, 6]
end
}

@matrix-method["diagonal"]

Returns a one-row matrix containing the matrix's diagonal entries.

@examples{
check:
  [matrix(3,3): 1, 2, 3, 4, 5, 6, 7, 8, 9].diagonal() is
  [matrix(1,3): 1, 5, 9]

  [matrix(3,2): 1, 2, 3, 4, 5, 6].diagonal() is
  [matrix(1,2): 1, 5]
end
}

@matrix-method["upper-triangle"]

Returns the upper triangle of the matrix, if the matrix is square.
For example, the upper triangle of @math-imtx{1 & 2 & 3\\ 4 & 5 & 6\\ 7 & 8 & 9}
would be @math-imtx{1 & 2 & 3\\ 0 & 5 & 6 \\ 0 & 0 & 9}.

@examples{
check:
  [matrix(2,2): 1, 2, 3, 4].upper-triangle() is [matrix(2,2): 1, 2, 0 ,4]

  [matrix(3,3): 1, 2, 3, 4, 5, 6, 7, 8, 9].upper-triangle() is
  [matrix(3,3): 1, 2, 3, 0, 5, 6, 0, 0, 9]
end
}

@matrix-method["lower-triangle"]

Returns the lower triangle of the matrix, if the matrix is square.
For example, the upper triangle of @math-imtx{1 & 2 & 3\\ 4 & 5 & 6\\ 7 & 8 & 9}
would be @math-imtx{1 & 0 & 0\\ 4 & 5 & 0\\ 7 & 8 & 9}.

@examples{
check:
  [matrix(2,2): 1, 2, 3, 4].lower-triangle() is [matrix(2,2): 1, 0, 3, 4]

  [matrix(3,3): 1, 2, 3, 4, 5, 6, 7, 8, 9].lower-triangle() is
  [matrix(3,3): 1, 0, 0, 4, 5, 0, 7, 8, 9]
end
}

@matrix-method["row-list"]

Returns the matrix as a list of one-row matrices.
(Very similar to @pyret{to-lists()}, except this method
returns a list of matrices instead.

@examples{
check:
  [matrix(2,3): 1, 2, 3, 4, 5, 6].row-list() is
  [list: [matrix(1,3): 1, 2, 3],
         [matrix(1,3): 4, 5, 6]]
end
}

@matrix-method["col-list"]

Returns the matrix as a list of one-column matrices.
(Very similar to @pyret{to-vectors()}, except this method
returns a list of matrices instead.

@examples{
check:
  [matrix(2,3): 1, 2, 3, 4, 5, 6].col-list() is
  [list: [matrix(2,1): 1, 4],
         [matrix(2,1): 2, 5],
         [matrix(2,1): 3, 6]]
end
}

@matrix-method["map"]

Maps the given function entrywise over the matrix.

@examples{
check:
  multTwo = lam(x): x * 2 end
  [matrix(2,2): 1, 2, 3, 4].map(multTwo) is
  [matrix(2,2): 2, 4, 6, 8]
end
}

@matrix-method["row-map"]

Maps the given function over each row in the matrix.

@examples{
check:
  # sumRow :: 1*n matrix
  # Computes the total sum of all entries in the given row
  sumRow = lam(row): [matrix(1,1): row.to-vector().foldr(_ + _)] end
  [matrix(2,3): 1, 2, 3, 4, 5, 6].row-map(sumRow) is
  [matrix(2,1): 6, 15]
end
}

@matrix-method["col-map"]

Maps the given function over each column in the matrix.

@examples{
check:
  # sumCol :: m*1 matrix
  # Computes the total sum of all entries in the given column
  sumCol = lam(col): [matrix(1,1): col.to-vector().foldr(_ + _)] end
  [matrix(2,3): 1, 2, 3, 4, 5, 6].col-map(sumCol) is
  [matrix(1,3): 5, 7, 9]
end
}

@matrix-method["augment"]

Returns the matrix augmented with the given matrix. For
example, augmenting the matrix @math-imtx{1 & 2\\4 & 5} with
the matrix @math-imtx{3\\ 6} yields the matrix
@math-imtx{1 & 2 & 3\\ 4 & 5 & 6}.

@examples{
check:
  [matrix(2,2): 1, 2, 4, 5].augment([matrix(2,1): 3, 6]) is
  [matrix(2,3): 1, 2, 3, 4, 5, 6]
end
}

@matrix-method["stack"]

Returns the matrix stacked on top of the given matrix. For
example, stacking the matrix @math-imtx{1 & 2 & 3} on top of
the matrix @math-imtx{4 & 5 & 6} gives the matrix
@math-imtx{1 & 2 & 3\\ 4 & 5 & 6}.

@examples{
check:
  [matrix(1,3): 1, 2, 3].stack([matrix(1,3): 4, 5, 6]) is
  [matrix(2,3): 1, 2, 3, 4, 5, 6]
end
}

@matrix-method["trace"]

Returns the trace of the matrix (i.e. the sum of its diagonal values).

@examples{
check:
  [matrix(3,3): 1, 2, 3, 4, 5, 6, 7, 8, 9].trace() is 15
  [matrix(2,2): 2, 4, 6, 8].trace() is 10
end
}

@matrix-method["scale"]

Multiplies each entry in the matrix by the given value.

@examples{
check:
  [matrix(2,2): 1, 2, 3, 4].scale(2) is 
  [matrix(2,2): 2, 4, 6, 8]

  [matrix(2,2): 2, 4, 6, 8].scale(1/2) is
  [matrix(2,2): 1, 2, 3, 4]
end
}

@matrix-method["dot"]

Returns the Frobenius Product of the matrix with the given matrix (for
1-dimensional matrices, this is simply the dot product). This is done by
multiplying the matrix with the transposition of @pyret{other} and taking
the trace of the result. An example of this calculation (@math-in{\ast} 
denotes matrix multiplication):

@math-in{\left(\left[\begin{smallmatrix}1 & 2 & 3\end{smallmatrix}\right]
\ast\left[\begin{smallmatrix}4\\ 2\\ ^4/_3 \end{smallmatrix}\right]\right)}@pyret{.trace()}
@math-in{=
\underbrace{\left[\begin{smallmatrix}(1\cdot 4)+(2\cdot 2)+(3\cdot \frac{4}{3})\end{smallmatrix}\right]}_{
1\times 1 \text{ matrix}}}@pyret{.trace()}@math-in{=12}

@examples{
check:
  [matrix(1,3): 1, 2, 3].dot([matrix(1,3): 4, 2, 4/3]) is 12
  [matrix(1,3): 1, 2, 3].dot([matrix(1,3): 1, 1, 1]) is 6
end
}

@matrix-method["expt"]

Multiplies the matrix by itself the given number of times.

@examples{
check:
  a = [matrix(2,2): 1, 2, 3, 4]
  a.expt(1) is a
  a.expt(2) is a * a
  a.expt(3) is a * a * a
end
}

@matrix-method["determinant"]

Returns the determinant of the matrix, calculated via a recursive
implementation of laplace expansion.

@examples{
check:
  [matrix(5,5): 1, 2, 1, 2, 3,
                2, 3, 1, 0, 1,
                2, 2, 1, 0, 0,
                1, 1, 1, 1, 1,
                0,-2, 0,-2,-2].determinant() is -2
end
}

@matrix-method["is-invertible"]

Returns true if the matrix is invertible (i.e. it has a nonzero determinant).

@matrix-method["rref"]

Returns the Reduced Row Echelon Form of the matrix. For example:
@math-disp{\begin{bmatrix}1 & 2 & 3 \\ 4 & 5 & 6\end{bmatrix}
                 \overrightarrow{RREF}
                 \begin{bmatrix}1 & 0 & -1\\ 0 & 1 & 2\end{bmatrix}}

@examples{
check:
  [matrix(2,3): 1, 2, 3, 4, 5, 6].rref() is
  [matrix(2,3): 1, 0,-1, 0, 1, 2]
end
}

@matrix-method["inverse"]

Returns the inverse of the matrix, if it is invertible (found
by augmenting the matrix with itself and finding the reduced-row
echelon form). For example:
@math-disp{\begin{bmatrix}1 & 0 & 4\\ 1 & 1 & 6\\ -3 & 0 & -10\end{bmatrix}^{-1}
                 = \begin{bmatrix}-5 & 0 & -2\\ -4 & 1 & -1\\ ^3/_2 & 0 & ^1/_2\end{bmatrix}}

@examples{
check:
  [mk-mtx(3,3): 1, 0, 4, 1, 1, 6, -3, 0, -10].inverse() is 
  [mk-mtx(3,3): -5, 0, -2, -4, 1, -1, 3/2, 0, 1/2]
end
}

@matrix-method["solve"]

Returns the matrix which, when multiplied on the right of this matrix, results in the given matrix.
In other words, this returns the solution to the system of equations represented by this and the given matrix.
This method only works on invertible matrices (Calculated by inverting itself and multiplying the given
matrix on the right side of this inverse).

@matrix-method["least-squares-solve"]

Returns the least squares solution for this and the given matrix, calculated using QR decomposition.

@matrix-method["lp-norm"]

Computes the @math{L^p} norm of the matrix using the given number.

@matrix-method["l1-norm"]
@matrix-method["l2-norm"]
@matrix-method["l-inf-norm"]

Computes the @math{L^1}, @math{L^2}, and @math{L}@superscript{∞} norms of the matrix, respectively.

@examples{
check:
  a = [matrix(3,1): 1, 2, 3]
  b = [matrix(3,3): 1, 0, 0, 2, 0, 0, 3, 0, 0]

  a.lp-norm(3) is-within(0.00001) num-expt(35, 1/3)
  b.lp-norm(3) is-within(0.00001) (b * a).lp-norm(3)

  a.l1-norm()  is-within(0.00001) 6
  a.l2-norm()  is-within(0.00001) num-sqrt(14)
  a.l-inf-norm() is 3
end
}

@matrix-method["qr-decomposition"]

Returns the QR Decomposition of the matrix as a list with two matricies, the first being
the Q matrix and the second being the R matrix.

@matrix-method["gram-schmidt"]

Returns an orthogonal matrix whose image is the same as the span of the matrix's columns.
(The same as the first result of @pyret{qr-decomposition})

@section{@pyret{Matrix} Binary Operations}

The following binary operations on matrices are defined:

@matrix-method["_plus"]

Adds the matrix to the given matrix.

@examples{
check:
  [matrix(2,2): 1, 2, 3, 4] + [matrix(2,2): 1, 2, 3, 4] is
  [matrix(2,2): 2, 4, 6, 8]
end
}

@matrix-method["_minus"]

Subtracts the given matrix from the matrix.

@examples{
check:
  [matrix(2,2): 1, 2, 3, 4] - [matrix(2,2): 0, 2, 3, 3] is
  [matrix(2,2): 1, 0, 0, 1]
end
}

@matrix-method["_times"]

Multiplies the given matrix on the right of the matrix
(Reminder: Matrix Multiplication is not commutative)

@examples{
check:
  [matrix(2,2): 1, 2, 3, 4] * [matrix(2,2): 3, 0, 0, 3] is
  [matrix(2,2): 3, 6, 9, 12]
end
}

@section{Matrix and Vector Functions}

The following functions are defined for matrices and vectors:

@function[
  "dot"
  #:examples
  '@{
  check:
    dot([vector: 1, 2, 3], [vector: 3, 2, 1]) is 10
  end
  }
]{Returns the dot product of the two given vectors.}

@function[
  "magnitude"
  #:examples
  '@{
  check:
    magnitude([vector: 3, 4]) is 5
    magnitude([vector: 4, 0]) is 4
  end
  }
]{Returns the magnitude of the given vector.}

@function[
  "cross"
  #:examples
  '@{
  check:
    cross([vector: 2, -3, 1], [vector: -2, 1, 1]) is [vector: -4, -4, -4]
  end
  }
]{Returns the cross product of the two given 3D vectors.}

@function[
  "normalize"
  #:examples
  '@{
  check:
    normalize([vector: 1, 2, 3]) is 
    [vector: (1 / num-sqrt(14)), (2 / num-sqrt(14)), (3 / num-sqrt(14))]
  end
  }
]{Normalizes the given vector into a unit vector.}

@function[
  "scale"
  #:examples
  '@{
  check:
    scale([vector: 1, 2, 3], 2) is [vector: 2, 4, 6]
  end
  }
]{Scales the given vector by the given constant.}

@function[
  "vector-add"
  #:examples
  '@{
  check:
    vector-add([vector: 1, 2, 3], [vector: 4, 5, 6]) is [vector: 5, 7, 9]
  end
  }
]{Adds the two given vectors.}

@function[
  "vector-sub"
  #:examples
  '@{
  check:
    vector-sub([vector: 1, 2, 3], [vector: 4, 5, 6]) is [vector: -3, -3, -3]
  end
  }
]{Subtracts the two given vectors.}

@function["is-row-matrix"]{Returns true if the given matrix has exactly one row.}
@function["is-col-matrix"]{Returns true if the given matrix has exactly one column.}
@function["is-square-matrix"]{Returns true if the given matrix has the same number of rows and columns.}
@function[
  "vector-to-matrix"
  #:examples
  '@{
  check:
    vector-to-matrix([vector: 1, 2, 3]) is [matrix(1,3): 1, 2, 3]
  end
  }
]{Converts the given vector into a one-row matrix.}

@function[
  "list-to-matrix"
  #:examples
  '@{
  check:
    list-to-matrix(2, 2, [list: 1, 2, 3, 4]) is 
    [matrix(2,2): 1, 2, 3, 4]
    
    list-to-matrix(2, 3, [list: 1, 2, 3, 4, 5, 6]) is 
    [matrix(2,3): 1, 2, 3, 4, 5, 6]
  end
  }
]{Converts the given list of numbers into a matrix of the given size.}

@function[
  "list-to-row-matrix"
  #:examples
  '@{
  check:
    list-to-row-matrix([list: 1, 2, 3, 4]) is [matrix(1,4): 1, 2, 3, 4]
  end
  }
]{Converts the given list of numbers into a one-row matrix.}

@function[
  "list-to-col-matrix"
  #:examples
  '@{
  check:
    list-to-col-matrix([list: 1, 2, 3, 4]) is [matrix(4,1): 1, 2, 3, 4]
  end
  }
]{Converts the given list of numbers into a one-column matrix.}

@function[
  "lists-to-matrix"
  #:examples
  '@{
  check:
    lists-to-matrix([list: [list: 1, 2, 3, 4]]) is [matrix(1,4): 1, 2, 3, 4]
    lists-to-matrix([list: [list: 1, 2, 3],
                           [list: 4, 5, 6]]) is [matrix(2,3): 1, 2, 3, 4, 5, 6]
  end
  }
]{Converts the given list of lists into a matrix, with each list as a row.}

@function[
  "vectors-to-matrix"
  #:examples
  '@{
  check:
    vectors-to-matrix([list: [vector: 1, 2, 3]]) is [matrix(3,1): 1, 2, 3]
    vectors-to-matrix([list: [vector: 1, 3, 5], [vector: 2, 4, 6]]) is
    [matrix(3,2): 1, 2, 3, 4, 5, 6]
  end
  }
]{Converts the given list of vectors into a matrix, with each vector as a column.}

@function["matrix-within"]{Returns a comparison predicate which returns true if each entry in both matrices is within @pyret{delta} of each other.}
  
}
