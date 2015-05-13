" Vim syntax file
" Language: Pyret
" Maintainer: Joe Gibbs Politz (joe@cs.brown.edu)

if exists("b:current_syntax")
  finish
endif

set iskeyword+=-
set iskeyword+=:

syn match delimeter '!'
syn match delimeter '\.'
syn match delimeter '|'
syn match delimeter '('
syn match delimeter ')'
syn match delimeter '->'
syn match delimeter '='
syn match delimeter '=>'
syn match delimeter ':='

syn match delimeter '\['
syn match delimeter '\]'
syn match delimeter '{'
syn match delimeter '}'
syn match delimeter ':'
syn match delimeter '::'

syn keyword basic var fun fun: end with: sharing: data import provide as try: except when
syn keyword basic for from check: where: doc: and or not else: if else cases
syn keyword basic is is== is=~ is<=> is-not is-not== is-not=~ is-not<=> raises
syn keyword basic deriving ref graph: m-graph: block: satisfies shadow lam type type-let provide-types newtype
syn keyword basic let rec letrec

syn match op ' + '
syn match op ' - '
syn match op ' / '
syn match op ' * '
syn match op ' > '
syn match op ' < '
syn match op ' >= '
syn match op ' <= '
syn match op ' <> '

syn match comment '\#.*$'
syntax region comment start=/#|/ skip=/\./ end=/|#/

syntax region string start=/\v"/ skip=/\v\\./ end=/\v"/
syntax region string start=/\v'/ skip=/\v\\./ end=/\v'/
syntax region string start=/\v```/ skip=/\v\\./ end=/\v```/

syn match number "[0-9]+"

hi def link comment Comment
hi def link basic Label
hi def link delimeter PreProc
hi def link op Label
hi def link string Constant
hi def link number Constant
hi def link literal Constant

