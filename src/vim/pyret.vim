" Vim syntax file
" Language: Pyret
" Maintainer: Joe Gibbs Politz (joe@cs.brown.edu)
" Latest Revision: 24 May 2013

if exists("b:current_syntax")
  finish
endif

syn keyword basic var fun end cond with sharing data do import provide as try except when for

syn match op '|'
syn match op '('
syn match op ')'
syn match op ':'
syn match op '->'
syn match op '=>'

syn match literal '\['
syn match literal '\]'
syn match literal '{'
syn match literal '}'

syn match comment '\#.*$'

syn match string '"[^"]*"'
syn match string "'[^']*'"

syn match number "[0-9]+"

hi def link comment Comment
hi def link basic Label
hi def link op PreProc
hi def link string Constant
hi def link number Constant
hi def link literal Constant

" This hilarious line is *adding* hyphens as a non-word separator
se iskeyword+=-

