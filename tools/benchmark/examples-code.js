var sortListProgram =
'fun isort(l):\n \
  cases (List) l:\n \
    | empty => empty\n \
    | link(f, r) =>\n \
      insert(f, isort(r))\n \
  end\n \
end\n \
\n \
fun insert(e, l):\n \
  cases (List) l:\n \
    | empty => [list: e]\n \
    | link(f, r) =>\n \
      if e < f:\n \
        link(e, l)\n \
      else:\n \
        link(f, insert(e, r))\n \
      end\n \
  end\n \
end\n \
\n \
check:\n \
  isort([list: 1, 4, 2]) is [list: 1, 2, 4]\n \
  isort([list: 5, 4, 3, 3]) is [list: 3, 3, 4, 5]\n \
  isort(empty) is [list: ]\n \
  isort([list: ]) is empty\n \
  isort([list: 1]) is empty\n \
end';


var benchmarks = 
[
{program: '1 + true', name: '"1 + true" [this should error out]'},
{program: '', name: '"" (empty)'},
{program: '1', name: '"1"'},
{program: 'print("Ahoy, world!")', name:'"print("Ahoy, world!")"'},
{program: 'lam(x): x end', name: '"lam(x): x end"'},
{program: sortListProgram, name: 'insertion-sort.arr'},
{program: 'range(0,100).map(lam(x): x + 1 end)', name: '"range(0,100).map(lam(x): x + 1 end)"'}
];

var b = require('benchmark-pyret');
b.runBenchmarks(benchmarks, {}, true, function (){});
