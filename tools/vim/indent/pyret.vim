" Vim indent file
" Language:    Pyret
" Maintainer:  Dorai Sitaram, ds26gte.github.io
" Last Change: 2015-06-12

if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setl indentexpr=GetPyretIndent(v:lnum)

setl indentkeys=0{,0},!,o,O,=\|,=case,=catch,=else,=elseif,=end

if exists("*GetPyretIndent")
  finish
endif

let s:pyretIndentOpeningWords = 'ask\|cases\|catch\|check\|data\|for\|fun\|if\|sharing\|switch\|try\|when\|while'

let s:pyretIndentMiddleWords = 'case\|catch\|else\|elseif\|sharing\|where'

let s:pyretIndentClosingWords = 'end'

"let s:pyretIndentColon = '\(#.*\)\@<!:\s*$'

let s:pyretIndentOpeningBrace = '\(#.*\)\@<!{\s*$'

"let s:pyretIndentClosingBrace = '\(#.*\)\@<!}\s*$'

let s:pyretIndentClosingBrace = '^\s*}\s*$'

let s:pyretIndentClosingStruct = '\(#.*\)\@<!\]\s*$'

let s:pyretIndentOpeningStruct = '\(#.*\)\@<!\[[-[:alpha:]]\+:\s*$'

let s:pyretIndentClosingStruct = '\(#.*\)\@<!\]\s*$'

let s:pyretIndentOpeningFunction = '\(#.*\)\@<!):\s*$'

let s:pyretIndentPipe = '^\s*|'

let s:pyretIndentOpeners =
      \  '^\s*\(' . s:pyretIndentOpeningWords . '\)\>\|'
      \. '^\s*\(' . s:pyretIndentMiddleWords . '\)\>\|'
      \. s:pyretIndentOpeningBrace . '\|'
      \. s:pyretIndentOpeningFunction . '\|'
      \. s:pyretIndentOpeningStruct

let s:pyretIndentClosers =
      \  '^\s*\(' . s:pyretIndentClosingWords . '\)\>\|'
      \. '^\s*\(' . s:pyretIndentMiddleWords . '\)\>\|'
      \. s:pyretIndentClosingBrace . '\|'
      \. s:pyretIndentClosingStruct

func! s:PyretIndentClosingPipe(cnum)
  let l:pnum = a:cnum - 1
  let l:unmatchedEnds = 0
  while l:pnum > 0
    let l:pLine = getline(l:pnum)
    if l:pLine =~ s:pyretIndentPipe
      if l:unmatchedEnds == 0
        return 1
      endif
    elseif l:pLine =~ s:pyretIndentClosers && l:pLine !~ s:pyretIndentOpeners
      let l:unmatchedEnds += 1
    elseif l:pLine =~ s:pyretIndentOpeners && l:pLine !~ s:pyretIndentClosers
      if l:unmatchedEnds <= 0
        return 0
      else
        let l:unmatchedEnds -= 1
      endif
    endif
    let l:pnum -= 1
  endwhile
endfunc

func! GetPyretIndent(cnum)
  let l:pnum = a:cnum - 1

  while l:pnum > 0 && getline(l:pnum) =~ '^\s*$'
    let l:pnum -= 1
  endwhile

  if l:pnum == 0
    return 0
  endif

  let l:suggIndent = indent(l:pnum)

  let l:cLine = getline(a:cnum)
  let l:pLine = getline(l:pnum)

  if l:cLine =~ s:pyretIndentClosers
    if l:pLine !~ s:pyretIndentOpeners
      let l:suggIndent -= &sw
      if l:pLine !~ s:pyretIndentPipe && s:PyretIndentClosingPipe(a:cnum)
        let l:suggIndent -= &sw
      endif
    endif

  elseif l:cLine =~ s:pyretIndentOpeners
    if l:pLine =~ s:pyretIndentOpeners || l:pLine =~ s:pyretIndentPipe
      let l:suggIndent += &sw
    endif

  elseif l:cLine =~ s:pyretIndentPipe
    if l:pLine =~ s:pyretIndentOpeners
      let l:suggIndent += &sw
    elseif l:pLine =~ s:pyretIndentPipe
      "stay put
    else
      let l:suggIndent -= &sw
    endif

  else
    if l:pLine =~ s:pyretIndentOpeners || l:pLine =~ s:pyretIndentPipe
      let l:suggIndent += &sw
    endif
  endif

  return l:suggIndent
endfunc
