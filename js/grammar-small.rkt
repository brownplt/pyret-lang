#lang ragg

program: prelude block

end: END | SEMI

prelude: [provide-stmt] import-stmt*

import-stmt: IMPORT (import-name | import-string) AS NAME
import-name: NAME
import-string: STRING
provide-stmt: PROVIDE stmt end | PROVIDE STAR

block: stmt*

stmt: NAME
