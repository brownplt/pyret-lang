" Vim syntax file
" Language: Pyret
" Maintainer: Joe Gibbs Politz (joe@cs.brown.edu)
" Latest Revision: 29 May 2013

if exists("b:current_syntax")
  finish
endif

set iskeyword+=:

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

syn keyword basic var fun end case: with: sharing: data import provide as try: except when for from check: doc: : and or not

syn match op '+'
syn match op '-'
syn match op '/'
syn match op '*'
syn match op '>'
syn match op '<'
syn match op '>='
syn match op '<='
syn match op '<>'

syn match comment '\#.*$'

syn match string '"[^"]*"'
syn match string "'[^']*'"

syn match number "[0-9]+"

hi def link comment Comment
hi def link basic Label
hi def link delimeter PreProc
hi def link op Label
hi def link string Constant
hi def link number Constant
hi def link literal Constant

