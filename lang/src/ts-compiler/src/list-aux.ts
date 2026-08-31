/*
  Ported from: src/arr/compiler/list-aux.arr

  Pyret Option results become `T | undefined` (`none` -> undefined,
  `some(v)` -> v).
*/

export function identity<T>(t: T): T { return t; }

// all2 returns false if any application of f returns false, or if the
// lengths differ. This behavior is chosen to maintain the short-circuiting
// semantics.
export function all2Strict<A, B>(f: (a: A, b: B) => boolean, l1: A[], l2: B[]): boolean {
  let i = 0;
  for (;;) {
    const done1 = i >= l1.length;
    const done2 = i >= l2.length;
    if (done1 && done2) { return true; }
    if (done1 || done2) { return false; }
    if (!f(l1[i], l2[i])) { return false; }
    i = i + 1;
  }
}

export function map2Strict<A, B, R>(f: (a: A, b: B) => R, l1: A[], l2: B[]): R[] | undefined {
  // The Pyret original recurs before applying f, so f is applied to the
  // last pair first; preserve that order for side-effecting f.
  function helper(i: number): R[] | undefined {
    const done1 = i >= l1.length;
    const done2 = i >= l2.length;
    if (done1 && done2) { return []; }
    if (done1 || done2) { return undefined; }
    const rest = helper(i + 1);
    if (rest === undefined) { return undefined; }
    return [f(l1[i], l2[i]), ...rest];
  }
  return helper(0);
}

export function fold2Strict<A, B, R>(f: (acc: R, a: A, b: B) => R, base: R, l1: A[], l2: B[]): R | undefined {
  let acc = base;
  let i = 0;
  for (;;) {
    const done1 = i >= l1.length;
    const done2 = i >= l2.length;
    if (done1 && done2) { return acc; }
    if (done1 || done2) { return undefined; }
    acc = f(acc, l1[i], l2[i]);
    i = i + 1;
  }
}
