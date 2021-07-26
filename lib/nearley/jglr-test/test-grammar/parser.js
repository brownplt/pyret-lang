define("test-base/parser", ["jglr/jglr"], function(E) {
  const Grammar = E.Grammar;
  const Nonterm = E.Nonterm;
  const Token = E.Token;
  const Rule = E.Rule;

  var g_json = {
    "version": 1,
    "start": "START",
    "name": "TestGrammar",
    "acceptStates": [
      1
    ],
    "mode": "RNGLR",
    "rulesByOldId": {
      "0": {
        "name": "opt-add",
        "symbols": [
          "'NUMBER",
          "'PLUS",
          "@opt-add_I2?"
        ]
      },
      "1": {
        "name": "opt-add_I2?",
        "symbols": []
      },
      "2": {
        "name": "opt-add_I2?",
        "symbols": [
          "@opt-add_I2"
        ]
      },
      "3": {
        "name": "opt-add_I2",
        "symbols": [
          "'NUMBER"
        ]
      },
      "4": {
        "name": "START",
        "symbols": [
          "@opt-add"
        ]
      },
      "11": {
        "position": 2,
        "like": 0
      },
      "16": {
        "position": 1,
        "like": 3
      },
      "18": {
        "position": 3,
        "like": 0
      },
      "20": {
        "position": 1,
        "like": 2
      }
    },
    "actionsByOldId": {
      "0": "dA",
      "1": "Inline",
      "2": "Inline",
      "3": "Inline",
      "4": "dA"
    },
    "flagsByOldId": {},
    "rules": [
      0,
      1,
      2,
      3,
      4
    ],
    "reduceActions": [
      11,
      1,
      1,
      1,
      16,
      0,
      18,
      0,
      20,
      0
    ],
    "rnTable": [
      {
        "opt-add": [
          1
        ],
        "'NUMBER": [
          2
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
          3
        ]
      },
      {
        "'NUMBER": [
          4
        ],
        "opt-add_I2?": [
          5
        ],
        "opt-add_I2": [
          6
        ],
        "$": [
          -1,
          0,
          2
        ]
      },
      {
        "$": [
          -1,
          4
        ]
      },
      {
        "$": [
          -1,
          6
        ]
      },
      {
        "$": [
          -1,
          8
        ]
      }
    ],
    "I": {
      "ε": 0,
      "opt-add_I2?": 1
    },
    "eSPPFs": [
      {
        "label": "EPSILON"
      },
      {
        "label": "opt-add_I2?",
        "kids": [],
        "rule": 1
      }
    ]
  };
  return { TestGrammar: Grammar.fromSerializable(g_json) };
})
