
interface SrcLoc {
  startCol: number;
  startRow: number;
  startChar: number;
  endRow: number;
  endCol: number;
  endChar: number;
}
interface Token {
  name: string;
  value: string;
  key: string;
  asString: string;
  pos: SrcLoc;
}
interface Nonterm {
  kids: ParserCST[];
  name: string;
  pos: SrcLoc;
  toString: () => string;
}

type ParserCST = Token | Nonterm;

export { SrcLoc, Token, Nonterm, ParserCST };

export class Pause<A> {
  stack: any[]
  pause: PausePackage<A>
  resumer: (pause: PausePackage<A>) => void
}

export type PauseHandlers<A> = {
  break: () => void,
  error: (err: any) => void,
  resume: (val: A) => void, 
}

export class PausePackage<A> {
  setHandlers: (handlers: PauseHandlers<A>) => void
  break: () => never
  error: (err: any) => never
  resume: (val: A) => never
}