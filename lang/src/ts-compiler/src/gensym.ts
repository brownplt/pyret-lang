/*
  Ported from: src/arr/compiler/gensym.arr
*/

let gensymCounter = 0;

export function reset(): void {
  gensymCounter = 0;
}

export function makeName(base: string): string {
  gensymCounter = 1 + gensymCounter;
  return base + String(gensymCounter);
}
