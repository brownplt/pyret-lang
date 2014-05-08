#lang scribble/doc

@; Documentation for pyret.lang.org.
@; Generated against the forms in docs/scribble-api.rkt

@(require "../scribble-api.rkt"
          )

@ignoremodule["modname"]


@docmodule["compiler/desugar-check"
           #:friendly-title "the desugar checking module, v 1.5"]{    
  @ignore[(list "f1" "f2")]                                                               
  @function["desugar-check" 
            #:contract (list (list "A.Program" "takes in")
                             "A.Program")
            #:alt-docstrings @lod[(list "bs" "simple bootstrap")
                                  (list "adv" "Advanced")]                      
            ]{Lengthy prose @lod[(list "bs" "intro prose") (list "adv" "advanced prose")]}
  
  @function["desugar-check" 
            #:contract (list (list "A.Program" "takes in")
                             "A.Program")
            #:alt-docstrings @lod[(list "bs" "simple bootstrap")
                                  (list "adv" "Advanced")]                      
            ]{Lengthy prose @lod[(list "bs" "intro prose") (list "adv" "advanced prose")]}
}



@;@module[#:name "name"]{
@;  @function[#:name "foo"
@;            #:contract "n->n"
@;            #:alt-docstrings  @lod[(bs "simple Bootstrap") (beg "Novice") (adv "Advanced")]]{
@;     Lengthy prose description ... possibly also a @lod[level-of-detail list] ...
@;     @examples[...]
@;     @bs-links[...]
@;     @other-links[...]
@;  }
@;  ...
@;}