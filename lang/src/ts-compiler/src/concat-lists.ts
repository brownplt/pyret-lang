/*
  Ported from: src/arr/compiler/concat-lists.arr

  data ConcatList<a>: concat-empty | concat-singleton | concat-append
                    | concat-cons | concat-snoc
  Pyret `+` on ConcatLists (_plus) becomes the method `append(other)`.
  `to-list` returns a plain array.

  Every traversal here is ITERATIVE over an explicit stack. The compiler
  builds these lists one link per ANF statement / case / variant, so an
  ordinary large program makes spines thousands of links deep, and the
  traversals run on every compile (emit, DAG simplify) -- recursing one
  frame per link overflows browser-sized stacks (see
  tests/stack-safety-test.js). Callback ORDER is part of the contract --
  callers' functions can be effectful -- and each helper documents the
  order it preserves. The one deliberate change: mapToListAcc used to
  evaluate f in a shape-dependent order (snoc: last first; append: right
  before left) as an artifact of its accumulator recursion; it now
  evaluates f strictly left-to-right like everything else. Its only
  callers build pure pretty-printer docs, and the RESULT array order is
  unchanged.
*/

import { InternalCompilerError } from './shared';

export abstract class ConcatListBase<T> {
  abstract get $name(): string;
  toListAcc(rest: T[]): T[] {
    const out = elementsForward(this as unknown as ConcatList<T>);
    return rest.length === 0 ? out : out.concat(rest);
  }
  mapToListAcc<U>(f: (x: T) => U, rest: U[]): U[] {
    const out = mapForward(this as unknown as ConcatList<T>, f);
    return rest.length === 0 ? out : out.concat(rest);
  }
  map<U>(f: (x: T) => U): ConcatList<U> {
    return mapPreservingShape(this as unknown as ConcatList<T>, f);
  }
  // f left-to-right
  each(f: (x: T) => void): void {
    const walk = new ForwardWalk(this as unknown as ConcatList<T>);
    for (let x = walk.next(); x !== DONE; x = walk.next()) {
      f(x as T);
    }
  }
  // f(acc, x) left-to-right
  foldl<B>(f: (base: B, x: T) => B, base: B): B {
    const walk = new ForwardWalk(this as unknown as ConcatList<T>);
    let acc = base;
    for (let x = walk.next(); x !== DONE; x = walk.next()) {
      acc = f(acc, x as T);
    }
    return acc;
  }
  // f(acc, x) right-to-left, exactly as the recursive formulation applied it
  foldr<B>(f: (base: B, x: T) => B, base: B): B {
    const elts = elementsForward(this as unknown as ConcatList<T>);
    let acc = base;
    for (let i = elts.length - 1; i >= 0; i--) {
      acc = f(acc, elts[i]);
    }
    return acc;
  }
  getFirst(): T {
    let cur = this as unknown as ConcatList<T>;
    for (;;) {
      switch (cur.$name) {
        case 'concat-empty': throw new InternalCompilerError('getFirst on concat-empty');
        case 'concat-singleton': return cur.element;
        case 'concat-cons': return cur.first;
        case 'concat-snoc':
          if (cur.head.isEmpty()) { return cur.last; }
          cur = cur.head;
          break;
        case 'concat-append':
          cur = cur.left.isEmpty() ? cur.right : cur.left;
          break;
      }
    }
  }
  getLast(): T {
    let cur = this as unknown as ConcatList<T>;
    for (;;) {
      switch (cur.$name) {
        case 'concat-empty': throw new InternalCompilerError('getLast on concat-empty');
        case 'concat-singleton': return cur.element;
        case 'concat-snoc': return cur.last;
        case 'concat-cons':
          if (cur.rest.isEmpty()) { return cur.first; }
          cur = cur.rest;
          break;
        case 'concat-append':
          cur = cur.right.isEmpty() ? cur.left : cur.right;
          break;
      }
    }
  }
  isEmpty(): boolean {
    const stack: ConcatList<T>[] = [this as unknown as ConcatList<T>];
    while (stack.length > 0) {
      const cur = stack.pop() as ConcatList<T>;
      switch (cur.$name) {
        case 'concat-empty': break;
        case 'concat-append': stack.push(cur.left, cur.right); break;
        default: return false;
      }
    }
    return true;
  }
  length(): number {
    const walk = new ForwardWalk(this as unknown as ConcatList<T>);
    let n = 0;
    for (let x = walk.next(); x !== DONE; x = walk.next()) {
      n++;
    }
    return n;
  }
  // The ''-skipping is per NODE, not per element (an append whose left
  // half renders '' contributes no separator, but a cons whose first
  // renders '' does) -- so this is a faithful post-order evaluation of
  // the original recursion, String() calls in left-to-right order.
  joinStr(sep: string): string {
    type Frame = { node: ConcatList<T>; state: number; a: string };
    const frames: Frame[] = [{ node: this as unknown as ConcatList<T>, state: 0, a: '' }];
    const vals: string[] = [];
    while (frames.length > 0) {
      const fr = frames[frames.length - 1];
      const node = fr.node;
      switch (node.$name) {
        case 'concat-empty':
          frames.pop();
          vals.push('');
          break;
        case 'concat-singleton':
          frames.pop();
          vals.push(String(node.element));
          break;
        case 'concat-cons':
          if (fr.state === 0) {
            fr.a = String(node.first);
            fr.state = 1;
            frames.push({ node: node.rest, state: 0, a: '' });
          } else {
            const r = vals.pop() as string;
            frames.pop();
            vals.push(r === '' ? fr.a : fr.a + sep + r);
          }
          break;
        case 'concat-snoc':
          if (fr.state === 0) {
            fr.state = 1;
            frames.push({ node: node.head, state: 0, a: '' });
          } else {
            const h = vals.pop() as string;
            const l = String(node.last);
            frames.pop();
            vals.push(h === '' ? l : h + sep + l);
          }
          break;
        case 'concat-append':
          if (fr.state === 0) {
            fr.state = 1;
            frames.push({ node: node.left, state: 0, a: '' });
          } else if (fr.state === 1) {
            fr.a = vals.pop() as string;
            fr.state = 2;
            frames.push({ node: node.right, state: 0, a: '' });
          } else {
            const r = vals.pop() as string;
            frames.pop();
            if (fr.a === '') { vals.push(r); }
            else if (r === '') { vals.push(fr.a); }
            else { vals.push(fr.a + sep + r); }
          }
          break;
      }
    }
    return vals[0];
  }
  // Structure-mirroring, rebuilt bottom-up (pure -- no callback order).
  reverse(): ConcatList<T> {
    type Frame = { node: ConcatList<T>; state: number; a?: ConcatList<T> };
    const frames: Frame[] = [{ node: this as unknown as ConcatList<T>, state: 0 }];
    const vals: ConcatList<T>[] = [];
    while (frames.length > 0) {
      const fr = frames[frames.length - 1];
      const node = fr.node;
      switch (node.$name) {
        case 'concat-empty':
        case 'concat-singleton':
          frames.pop();
          vals.push(node);
          break;
        case 'concat-cons':
          if (fr.state === 0) {
            fr.state = 1;
            frames.push({ node: node.rest, state: 0 });
          } else {
            frames.pop();
            vals.push(new ConcatSnoc(vals.pop() as ConcatList<T>, node.first));
          }
          break;
        case 'concat-snoc':
          if (fr.state === 0) {
            fr.state = 1;
            frames.push({ node: node.head, state: 0 });
          } else {
            frames.pop();
            vals.push(new ConcatCons(node.last, vals.pop() as ConcatList<T>));
          }
          break;
        case 'concat-append':
          // original: new ConcatAppend(right.reverse(), left.reverse())
          if (fr.state === 0) {
            fr.state = 1;
            frames.push({ node: node.right, state: 0 });
          } else if (fr.state === 1) {
            fr.a = vals.pop() as ConcatList<T>;
            fr.state = 2;
            frames.push({ node: node.left, state: 0 });
          } else {
            frames.pop();
            vals.push(new ConcatAppend(fr.a as ConcatList<T>, vals.pop() as ConcatList<T>));
          }
          break;
      }
    }
    return vals[0];
  }
  // f left-to-right, short-circuiting on the first false
  all(f: (x: T) => boolean): boolean {
    const walk = new ForwardWalk(this as unknown as ConcatList<T>);
    for (let x = walk.next(); x !== DONE; x = walk.next()) {
      if (!f(x as T)) { return false; }
    }
    return true;
  }
  // sharing method _plus
  append(other: ConcatList<T>): ConcatList<T> {
    const self = this as unknown as ConcatList<T>;
    if (isConcatEmpty(self)) { return other; }
    else if (isConcatEmpty(other)) { return self; }
    else { return new ConcatAppend<T>(self, other); }
  }
  toList(): T[] {
    return this.toListAcc([]);
  }
  mapToListLeft<U>(f: (x: T) => U): U[] {
    // f left-to-right (this was mapToListLeft's whole reason to exist)
    return mapForward(this as unknown as ConcatList<T>, f);
  }
  mapToList<U>(f: (x: T) => U): U[] {
    return this.mapToListAcc(f, []);
  }
  find(f: (x: T) => boolean): T | undefined {
    return find(f, this as unknown as ConcatList<T>);
  }
}

export class ConcatEmpty<T> extends ConcatListBase<T> {
  get $name(): 'concat-empty' { return 'concat-empty'; }
}

export class ConcatSingleton<T> extends ConcatListBase<T> {
  get $name(): 'concat-singleton' { return 'concat-singleton'; }
  constructor(public element: T) { super(); }
}

export class ConcatAppend<T> extends ConcatListBase<T> {
  get $name(): 'concat-append' { return 'concat-append'; }
  constructor(public left: ConcatList<T>, public right: ConcatList<T>) { super(); }
}

export class ConcatCons<T> extends ConcatListBase<T> {
  get $name(): 'concat-cons' { return 'concat-cons'; }
  constructor(public first: T, public rest: ConcatList<T>) { super(); }
}

export class ConcatSnoc<T> extends ConcatListBase<T> {
  get $name(): 'concat-snoc' { return 'concat-snoc'; }
  constructor(public head: ConcatList<T>, public last: T) { super(); }
}

export type ConcatList<T> =
  | ConcatEmpty<T>
  | ConcatSingleton<T>
  | ConcatAppend<T>
  | ConcatCons<T>
  | ConcatSnoc<T>;

// A deferred element in a forward walk (a snoc's `last`, pending until its
// head is exhausted). A wrapper class so an element that is itself a
// ConcatList cannot be confused with a link.
class PendingElem<T> {
  constructor(public e: T) {}
}

const DONE = Symbol('concat-walk-done');

/*
  In-order (left-to-right) element walk, iteratively: cons yields `first`
  then descends `rest`; snoc descends `head` with `last` pending behind
  it; append descends left with right pending. next() returns DONE when
  exhausted. Elements are yielded in exactly the order the recursive
  each/foldl visited them.
*/
class ForwardWalk<T> {
  private stack: (ConcatList<T> | PendingElem<T>)[];
  constructor(root: ConcatList<T>) {
    this.stack = [root];
  }
  next(): T | typeof DONE {
    const stack = this.stack;
    while (stack.length > 0) {
      const cur = stack.pop() as ConcatList<T> | PendingElem<T>;
      if (cur instanceof PendingElem) { return cur.e; }
      switch (cur.$name) {
        case 'concat-empty': break;
        case 'concat-singleton': return cur.element;
        case 'concat-cons':
          stack.push(cur.rest);
          return cur.first;
        case 'concat-snoc':
          stack.push(new PendingElem(cur.last), cur.head);
          break;
        case 'concat-append':
          stack.push(cur.right, cur.left);
          break;
      }
    }
    return DONE;
  }
}

function elementsForward<T>(root: ConcatList<T>): T[] {
  const out: T[] = [];
  const walk = new ForwardWalk(root);
  for (let x = walk.next(); x !== DONE; x = walk.next()) {
    out.push(x as T);
  }
  return out;
}

function mapForward<T, U>(root: ConcatList<T>, f: (x: T) => U): U[] {
  const out: U[] = [];
  const walk = new ForwardWalk(root);
  for (let x = walk.next(); x !== DONE; x = walk.next()) {
    out.push(f(x as T));
  }
  return out;
}

/*
  Structure-preserving map, f applied left-to-right (the recursive map's
  order for every node type: cons evaluates f(first) before mapping rest,
  snoc maps head before f(last)). The result has the SAME node shape as
  the input, so no consumer can tell this apart from the recursion.
*/
function mapPreservingShape<T, U>(root: ConcatList<T>, f: (x: T) => U): ConcatList<U> {
  type Frame = { node: ConcatList<T>; state: number; a?: unknown };
  const frames: Frame[] = [{ node: root, state: 0 }];
  const vals: ConcatList<U>[] = [];
  while (frames.length > 0) {
    const fr = frames[frames.length - 1];
    const node = fr.node;
    switch (node.$name) {
      case 'concat-empty':
        frames.pop();
        vals.push(node as unknown as ConcatList<U>);
        break;
      case 'concat-singleton':
        frames.pop();
        vals.push(new ConcatSingleton(f(node.element)));
        break;
      case 'concat-cons':
        if (fr.state === 0) {
          fr.a = f(node.first);
          fr.state = 1;
          frames.push({ node: node.rest, state: 0 });
        } else {
          frames.pop();
          vals.push(new ConcatCons(fr.a as U, vals.pop() as ConcatList<U>));
        }
        break;
      case 'concat-snoc':
        if (fr.state === 0) {
          fr.state = 1;
          frames.push({ node: node.head, state: 0 });
        } else {
          const h = vals.pop() as ConcatList<U>;
          frames.pop();
          vals.push(new ConcatSnoc(h, f(node.last)));
        }
        break;
      case 'concat-append':
        if (fr.state === 0) {
          fr.state = 1;
          frames.push({ node: node.left, state: 0 });
        } else if (fr.state === 1) {
          fr.a = vals.pop();
          fr.state = 2;
          frames.push({ node: node.right, state: 0 });
        } else {
          frames.pop();
          vals.push(new ConcatAppend(fr.a as ConcatList<U>, vals.pop() as ConcatList<U>));
        }
        break;
    }
  }
  return vals[0];
}

export function isConcatEmpty(x: any): x is ConcatEmpty<any> { return x instanceof ConcatEmpty; }
export function isConcatSingleton(x: any): x is ConcatSingleton<any> { return x instanceof ConcatSingleton; }
export function isConcatAppend(x: any): x is ConcatAppend<any> { return x instanceof ConcatAppend; }
export function isConcatCons(x: any): x is ConcatCons<any> { return x instanceof ConcatCons; }
export function isConcatSnoc(x: any): x is ConcatSnoc<any> { return x instanceof ConcatSnoc; }

// Singleton value for concat-empty (Pyret's concat-empty is a singleton)
// plus constructor helpers, in both original-style and short (cl*) names.
export const concatEmpty: ConcatList<any> = new ConcatEmpty<any>();
export function concatSingleton<T>(element: T): ConcatList<T> { return new ConcatSingleton(element); }
export function concatAppend<T>(left: ConcatList<T>, right: ConcatList<T>): ConcatList<T> { return new ConcatAppend(left, right); }
export function concatCons<T>(first: T, rest: ConcatList<T>): ConcatList<T> { return new ConcatCons(first, rest); }
export function concatSnoc<T>(head: ConcatList<T>, last: T): ConcatList<T> { return new ConcatSnoc(head, last); }

export const clEmpty: ConcatList<any> = concatEmpty;
export const clSing = concatSingleton;
export const clAppend = concatAppend;
export const clCons = concatCons;
export const clSnoc = concatSnoc;

// Takes a predicate and returns either the first item in this list that
// passes the predicate, or undefined (Pyret returns an Option).
// f left-to-right, stopping at the first hit.
export function find<T>(f: (x: T) => boolean, l: ConcatList<T>): T | undefined {
  const walk = new ForwardWalk(l);
  for (let x = walk.next(); x !== DONE; x = walk.next()) {
    if (f(x as T)) { return x as T; }
  }
  return undefined;
}

export function foldl<T, B>(f: (base: B, x: T) => B, base: B, lst: ConcatList<T>): B {
  return lst.foldl(f, base);
}
export function foldr<T, B>(f: (base: B, x: T) => B, base: B, lst: ConcatList<T>): B {
  return lst.foldr(f, base);
}
export function map<T, U>(f: (x: T) => U, lst: ConcatList<T>): ConcatList<U> {
  return lst.map(f);
}
export function each<T>(f: (x: T) => void, lst: ConcatList<T>): void {
  return lst.each(f);
}

export function all<T>(f: (x: T) => boolean, lst: ConcatList<T>): boolean {
  return lst.all(f);
}

// Pyret's `[clist: ...]` construction object; mirrors the arity-specific
// structures (make1..make5 build cons/singleton chains; the general case
// folds with snoc).
export function clist<T>(...elts: T[]): ConcatList<T> {
  switch (elts.length) {
    case 0: return concatEmpty as ConcatList<T>;
    case 1: return new ConcatSingleton(elts[0]);
    case 2: return new ConcatCons(elts[0], new ConcatSingleton(elts[1]));
    case 3: return new ConcatCons(elts[0], new ConcatCons(elts[1], new ConcatSingleton(elts[2])));
    case 4: return new ConcatCons(elts[0], new ConcatCons(elts[1], new ConcatCons(elts[2], new ConcatSingleton(elts[3]))));
    case 5: return new ConcatCons(elts[0], new ConcatCons(elts[1], new ConcatCons(elts[2], new ConcatCons(elts[3], new ConcatSingleton(elts[4])))));
    default: {
      let clst: ConcatList<T> = concatEmpty as ConcatList<T>;
      for (const elt of elts) {
        clst = new ConcatSnoc(clst, elt);
      }
      return clst;
    }
  }
}

// Returns a catenable list made up of f(n, e1), f(n+1, e2) .. for e1, e2
// ... in lst
export function map_list_n<T, U>(f: (n: number, x: T) => U, n: number, lst: T[]): ConcatList<U> {
  // f is applied front-to-back, as in the Pyret original (order matters
  // for side-effecting f, e.g. gensym)
  const mapped = lst.map((x, idx) => f(n + idx, x));
  let result: ConcatList<U> = concatEmpty as ConcatList<U>;
  for (let idx = mapped.length - 1; idx >= 0; idx--) {
    result = new ConcatCons(mapped[idx], result);
  }
  return result;
}

export function each_n<T>(f: (n: number, x: T) => void, n: number, lst: ConcatList<T>): void {
  let counter = n;
  lst.each((item: T) => {
    f(counter, item);
    counter = counter + 1;
  });
}

export function map_list<T, U>(f: (x: T) => U, lst: T[]): ConcatList<U> {
  // f is applied front-to-back, as in the Pyret original
  const mapped = lst.map(f);
  let result: ConcatList<U> = concatEmpty as ConcatList<U>;
  for (let idx = mapped.length - 1; idx >= 0; idx--) {
    result = new ConcatCons(mapped[idx], result);
  }
  return result;
}

// Returns a catenable list made up of f(elem1, elem2) for each elem1 in
// l1, elem2 in l2
export function map_list2<A, B, C>(f: (a: A, b: B) => C, l1: A[], l2: B[]): ConcatList<C> {
  // f is applied front-to-back, as in the Pyret original; stops at the
  // shorter list
  const len = Math.min(l1.length, l2.length);
  const mapped: C[] = [];
  for (let idx = 0; idx < len; idx++) {
    mapped.push(f(l1[idx], l2[idx]));
  }
  let result: ConcatList<C> = concatEmpty as ConcatList<C>;
  for (let idx = mapped.length - 1; idx >= 0; idx--) {
    result = new ConcatCons(mapped[idx], result);
  }
  return result;
}

// Returns a ConcatList version of this list
export function from_list<T>(l: T[]): ConcatList<T> {
  let result: ConcatList<T> = concatEmpty as ConcatList<T>;
  for (let idx = l.length - 1; idx >= 0; idx--) {
    result = new ConcatCons(l[idx], result);
  }
  return result;
}

// camelCase aliases for the underscore-named helpers
export const mapListN = map_list_n;
export const eachN = each_n;
export const mapList = map_list;
export const mapList2 = map_list2;
export const fromList = from_list;
