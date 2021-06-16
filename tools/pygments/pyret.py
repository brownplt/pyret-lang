from pygments.lexer import RegexLexer, words
from pygments.token import *

class CustomLexer(RegexLexer):
    name = 'Pyret'
    aliases = ['pyret']
    filenames = ['*.arr']

    before_word = r"(?<![-a-zA-Z0-9_])"
    after_word = r"(?:(?![-a-zA-Z0-9_])|$)"
    unsigned_decimal_part = "[0-9]+(?:\\.[0-9]+)?(?:[eE][-+]?[0-9]+)?"
    unsigned_rational_part = "[0-9]+/[0-9]+"
    number = before_word + ("[-+]?" + unsigned_decimal_part) + after_word
    badNumber = before_word + ("~?[+-]?\\.[0-9]+(?:[eE][-+]?[0-9]+)?") + after_word
    roughnum = before_word + ("~[-+]?"  + "(?:" + unsigned_rational_part + "|" + unsigned_decimal_part + ")") + after_word
    rational = before_word + ("[-+]?" + unsigned_rational_part) + after_word

    dquot_str = ("\"(?:" +
                 "\\\\[01234567]{1,3}" +
                 "|\\\\x[0-9a-fA-F]{1,2}" +
                 "|\\\\u[0-9a-fA-f]{1,4}" +
                 "|\\\\[\\\\nrt\"\']" +
                 "|[^\\\\\"\n\r])*\"")
    squot_str = ("\'(?:" +
                 "\\\\[01234567]{1,3}" +
                 "|\\\\x[0-9a-fA-F]{1,2}" +
                 "|\\\\u[0-9a-fA-f]{1,4}" +
                 "|\\\\[\\\\nrt\"\']" +
                 "|[^\\\\\'\n\r])*\'")
    unterminated_string = ("^[\"\'].*")

    tokens = {
        'root': [
            (r'\s+', Text),
            # (roughnum, Number.Float),
            # (rational, Number.Rational),
            # (number, Number.Int),
            # (badNumber, Error),
            # (dquot_str, String),
            # (squot_str, String),
            # (r'```', String.Multiline, 'tqs'),
            # (unterminated_string, Error),

            (r'#\|', Comment.Multiline, 'comment'),
            (r'#.*$', Comment.Singleline),

            (words(("reactor", "try", "ref-graph", "block", "table", "load-table", "doc", "otherwise", "then", "with", "sharing", "where", "do", "row", "source"), prefix=before_word, suffix=r'(?=:)'), Keyword),

            (words(("when", "violates", "var", "using", "type-let", "type", "transform", "spy", "sieve", "shared", "shadow", "select", "satisfies", "sanitize", "ref", "rec", "raises-violates", "raises-satisfies", "raises-other-than", "raises", "provide-types", "provide", "order", "or", "of", "newtype", "method", "letrec", "let", "lazy", "lam", "is=~", "is==", "is<=>", "is-roughly", "is-not=~", "is-not==", "is-not<=>", "is-not", "is", "include", "import", "if", "fun", "from", "for", "extract", "extend", "except", "examples", "end", "else if", "else", "does-not-raise", "descending", "data", "check", "cases", "by", "because", "ask", "ascending", "as", "and"), prefix=before_word, suffix=after_word), Keyword),

            (words(('true', 'false'), prefix=before_word, suffix=after_word), Literal),


            (r'<=>', Operator),
            (r'::', Punctuation),
            (r'=~', Operator),
            (r'==', Operator),
            (r'>=', Operator),
            (r'<=', Operator),
            (r'=>', Punctuation),
            (r'->', Punctuation),
            (r':=', Operator),
            (r'<>', Operator),
            (r':', Punctuation),
            (r'.', Punctuation),
            (r'<', Operator),
            (r'>', Operator),
            (r',', Punctuation),
            (r'\^', Punctuation),
            (r'!', Operator),
            (r';', Punctuation),
            (r'\|', Punctuation),
            (r'=', Operator),
            (r'[+]', Operator),
            (r'[*]', Operator),
            (r'/', Operator),
            (r'\\', Operator),
            (r'[(){}\[\]]', Punctuation),

            (before_word + r'[a-zA-Z_][-a-zA-Z0-9_]*(?![-a-zA-Z0-0_])' + after_word, Name),
            
        ],
        'comment': [
            (r'[^#|]', Comment.Multiline),
            (r'#|', Comment.Multiline, '#push'),
            (r'|#', Comment.Multiline, '#pop'),
            (r'[#|]', Comment.Multiline)
        ],
        'tqs': [
            (r'```', String.Multiline, '#pop'),
            (r'.', String.Multiline),
        ]
    }
