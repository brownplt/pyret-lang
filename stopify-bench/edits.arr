# This file is a Pyret port of the edits functions from http://norvig.com/spell-correct.html

include string-dict
import file("benchmark-base.arr") as B

fun edits1(word):
  letters = 'abcdefghijklmnopqrstuvwxyz'
  splits = for map(i from range(0, string-length(word) + 1)):
    { string-substring(word, 0, i);
      string-substring(word, i, string-length(word)) }
  end.filter({({L;R}): string-length(R) > 0})
  deletes = for map({L; R} from splits):
    L + string-substring(R, 1, string-length(R))
  end
  transposes = for map({L; R} from splits.filter({({_; R}): string-length(R) > 1})):
    L + string-char-at(R, 1) + string-char-at(R, 0) + string-substring(R, 2, string-length(R))
  end
  replaces =
    for fold(lst from empty, c from string-explode(letters)):
      for map({L; R} from splits):
        L + c + string-substring(R, 1, string-length(R))
      end + lst
    end
  inserts =
    for fold(lst from empty, c from string-explode(letters)):
      for map({L; R} from splits):
        L + c + R
      end + lst
    end
  deletes + (transposes + (replaces + inserts))
end

fun edits2(word):
  for fold(lst from empty, one-away from edits1(word)) block:
    edits1(one-away) + lst
  end
end

B.benchmark(lam(): edits2("correktions") end, 1)
