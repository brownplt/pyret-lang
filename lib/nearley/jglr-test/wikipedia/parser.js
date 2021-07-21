define("wiki-base/parser", ["jglr/jglr"], function(E) {
  const Grammar = E.Grammar;
  const Nonterm = E.Nonterm;
  const Token = E.Token;
  const Rule = E.Rule;

  var g_json = {
    "version": 1,
    "start": "START",
    "name": "WikipediaGrammar",
    "acceptStates": [
      1
    ],
    "mode": "RNGLR",
    "rulesByOldId": {
      "0": {
        "name": "p",
        "symbols": [
          "@s"
        ]
      },
      "1": {
        "name": "s",
        "symbols": [
          "@s",
          "'PLUS",
          "@m"
        ]
      },
      "2": {
        "name": "s",
        "symbols": [
          "@m"
        ]
      },
      "3": {
        "name": "m",
        "symbols": [
          "@m",
          "'TIMES",
          "@t"
        ]
      },
      "4": {
        "name": "m",
        "symbols": [
          "@t"
        ]
      },
      "5": {
        "name": "t",
        "symbols": [
          "'NUMBER"
        ]
      },
      "6": {
        "name": "START",
        "symbols": [
          "@p"
        ]
      },
      "24": {
        "position": 1,
        "like": 0
      },
      "29": {
        "position": 1,
        "like": 2
      },
      "36": {
        "position": 1,
        "like": 4
      },
      "40": {
        "position": 1,
        "like": 5
      },
      "51": {
        "position": 3,
        "like": 1
      },
      "54": {
        "position": 3,
        "like": 3
      }
    },
    "actionsByOldId": {
      "0": "dA",
      "1": "dA",
      "2": "dA",
      "3": "dA",
      "4": "dA",
      "5": "dA",
      "6": "dA"
    },
    "flagsByOldId": {},
    "rules": [
      0,
      1,
      2,
      3,
      4,
      5,
      6
    ],
    "reduceActions": [
      24,
      0,
      29,
      0,
      36,
      0,
      40,
      0,
      51,
      0,
      54,
      0
    ],
    "rnTable": [
      {
        "p": [
          1
        ],
        "s": [
          2
        ],
        "m": [
          3
        ],
        "t": [
          4
        ],
        "'NUMBER": [
          5
        ]
      },
      {
        "$": [
          -1,
          -1
        ]
      },
      {
        "'PLUS": [
          6
        ],
        "$": [
          -1,
          0
        ]
      },
      {
        "'TIMES": [
          7
        ],
        "$": [
          -1,
          2
        ],
        "'PLUS": [
          -1,
          2
        ]
      },
      {
        "$": [
          -1,
          4
        ],
        "'PLUS": [
          -1,
          4
        ],
        "'TIMES": [
          -1,
          4
        ]
      },
      {
        "$": [
          -1,
          6
        ],
        "'PLUS": [
          -1,
          6
        ],
        "'TIMES": [
          -1,
          6
        ]
      },
      {
        "m": [
          8
        ],
        "t": [
          4
        ],
        "'NUMBER": [
          5
        ]
      },
      {
        "t": [
          9
        ],
        "'NUMBER": [
          5
        ]
      },
      {
        "'TIMES": [
          7
        ],
        "$": [
          -1,
          8
        ],
        "'PLUS": [
          -1,
          8
        ]
      },
      {
        "$": [
          -1,
          10
        ],
        "'PLUS": [
          -1,
          10
        ],
        "'TIMES": [
          -1,
          10
        ]
      }
    ],
    "I": {
      "ε": 0
    },
    "eSPPFs": [
      {
        "label": "EPSILON"
      }
    ]
  };
  return { WikipediaGrammar: Grammar.fromSerializable(g_json) };
})
