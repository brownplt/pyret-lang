" Vim syntax file
" Language: Pyret
" Maintainer: Joe Gibbs Politz (joe@cs.brown.edu)
" Latest Revision: 10 January 2012

if exists("b:current_syntax")
  finish
endif

syn keyword basic var fun end cond with sharing data do import provide as try except when

syn match op '|'
syn match op '('
syn match op ')'
syn match op ':'
syn match op '->'
syn match op '=>'

syn match comment '\#.*$'

syn match string '"[^"]*"'
syn match string "'[^']*'"

syn match number "[0-9]+"

hi def link comment Comment
hi def link basic Label
hi def link op PreProc
hi def link string Constant
hi def link number Constant

" This hilarious line is *adding* hyphens as a non-word separator
se iskeyword+=-

