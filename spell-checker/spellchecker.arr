import file as F
import lists as L
import string-dict as D
import string as S

fun words( path ):
  S.string-to-lower( F.file-to-string( path ) )
end

all-words = L.filter( L.filter( L.split-pattern( words( "big.txt" ), "\\b" ),
  lam(str): str <> " " end ),
  lam(str): str <> "\n" end )

N = L.length( all-words )
vocab = D.count( all-words )

fun P( word ):
  D.get( vocab, word ) / N
end

fun correction( word ):
  # apply function P to each element in candidates( word ) and return the word
  # with the largest P(word)
  word
end

fun candidates( word ):
  e0 = known( [L.list: word] )

  known( e0 )
end

fun known( shadow words ):
  L.filter( words, lam( word ): D.has-key( vocab, word ) end )
end

candidates( "the" )