/*
  Ported from: src/arr/trove/srcloc.arr

  data Srcloc: builtin(module-name) | srcloc(source, start-line, ...).
  Class names are `Builtin` and `Srcloc`; the union type is named `Loc`
  (the rest of the compiler refers to it as `type Loc = S.Srcloc`).
*/

export abstract class SrclocBase {
  abstract get $name(): string;
  abstract format(showFile: boolean): string;
  abstract key(): string;
  abstract sameFile(other: Loc): boolean;
  abstract before(other: Loc): boolean;
  abstract contains(other: Loc): boolean;
  abstract isBuiltin(): boolean;
  abstract equals(other: Loc): boolean;
  abstract toString(): string;
  // sharing
  after(other: Loc): boolean {
    return other.before(this as unknown as Loc);
  }
}

export class Builtin extends SrclocBase {
  get $name(): 'builtin' { return 'builtin'; }
  constructor(public moduleName: string) { super(); }
  format(_showFile: boolean): string {
    return '<builtin ' + this.moduleName + '>';
  }
  key(): string { return this.moduleName; }
  sameFile(other: Loc): boolean {
    return isBuiltin(other) && (other.moduleName === this.moduleName);
  }
  before(other: Loc): boolean {
    switch (other.$name) {
      case 'builtin': return this.moduleName < other.moduleName;
      case 'srcloc': return false;
    }
  }
  contains(_other: Loc): boolean {
    return false;
  }
  isBuiltin(): boolean { return true; }
  equals(other: Loc): boolean {
    return isBuiltin(other) && (other.moduleName === this.moduleName);
  }
  toString(): string {
    return 'builtin(' + this.moduleName + ')';
  }
}

export class Srcloc extends SrclocBase {
  get $name(): 'srcloc' { return 'srcloc'; }
  constructor(
    public source: string,
    public startLine: number,
    public startColumn: number,
    public startChar: number,
    public endLine: number,
    public endColumn: number,
    public endChar: number
  ) { super(); }
  // Returns either 'file: line, col' or just 'line, col', depending on the
  // show-file flag
  format(showFile: boolean): string {
    if (showFile) {
      return this.source + ':' + String(this.startLine) + ':' + String(this.startColumn)
        + '-' + String(this.endLine) + ':' + String(this.endColumn);
    } else {
      return 'line ' + String(this.startLine) + ', column ' + String(this.startColumn);
    }
  }
  key(): string {
    return this.source + ':' + String(this.startChar) + '-' + String(this.endChar);
  }
  sameFile(other: Loc): boolean {
    return isSrcloc(other) && (this.source === other.source);
  }
  // Returns true if this location comes before the other one, assuming they
  // come from the same file
  before(other: Loc): boolean {
    switch (other.$name) {
      case 'builtin': return true;
      default: return this.startChar < other.startChar;
    }
  }
  atStart(): Srcloc {
    return new Srcloc(this.source,
      this.startLine, this.startColumn, this.startChar,
      this.startLine, this.startColumn, this.startChar);
  }
  atEnd(): Srcloc {
    return new Srcloc(this.source,
      this.endLine, this.endColumn, this.endChar,
      this.endLine, this.endColumn, this.endChar);
  }
  // Pyret's _plus. Note: assumes that both locations are from same file
  plus(other: Srcloc): Srcloc {
    if (this.startChar <= other.startChar) {
      if (this.endChar >= other.endChar) {
        return this;
      } else {
        return new Srcloc(this.source,
          this.startLine, this.startColumn, this.startChar,
          other.endLine, other.endColumn, other.endChar);
      }
    } else {
      if (this.endChar > other.endChar) {
        return new Srcloc(this.source,
          other.startLine, other.startColumn, other.startChar,
          this.endLine, this.endColumn, this.endChar);
      } else {
        return other;
      }
    }
  }
  // Note: assumes that both locations are from same file
  upto(other: Srcloc): Srcloc {
    if (this.startChar <= other.endChar) {
      return new Srcloc(this.source,
        this.startLine, this.startColumn, this.startChar,
        other.startLine, other.startColumn, other.startChar);
    } else {
      return this;
    }
  }
  // Note: assumes that both locations are from same file
  uptoEnd(other: Srcloc): Srcloc {
    if (this.startChar <= other.endChar) {
      return new Srcloc(this.source,
        this.startLine, this.startColumn, this.startChar,
        other.endLine, other.endColumn, other.endChar);
    } else {
      return this;
    }
  }
  contains(other: Loc): boolean {
    return isSrcloc(other)
      && (this.startLine <= other.startLine)
      && (this.startChar <= other.startChar)
      && (this.endLine >= other.endLine)
      && (this.endChar >= other.endChar);
  }
  isBuiltin(): boolean { return false; }
  equals(other: Loc): boolean {
    return isSrcloc(other)
      && (this.source === other.source)
      && (this.startLine === other.startLine)
      && (this.startColumn === other.startColumn)
      && (this.startChar === other.startChar)
      && (this.endLine === other.endLine)
      && (this.endColumn === other.endColumn)
      && (this.endChar === other.endChar);
  }
  toString(): string {
    // Pyret `tostring` of a srcloc: the source is printed bare (unquoted),
    // like the .arr default tostring. render-error-display's cmcode case and
    // other String(loc) sites are tostring contexts, not torepr, so do not
    // quote the source (an earlier port used JSON.stringify -- torepr shape --
    // which diverged from the .arr, e.g. the reactor well-formedness message).
    return 'srcloc(' + this.source
      + ', ' + String(this.startLine)
      + ', ' + String(this.startColumn)
      + ', ' + String(this.startChar)
      + ', ' + String(this.endLine)
      + ', ' + String(this.endColumn)
      + ', ' + String(this.endChar) + ')';
  }
}

export type Loc = Builtin | Srcloc;

export function isBuiltin(x: any): x is Builtin { return x instanceof Builtin; }
export function isSrcloc(x: any): x is Srcloc { return x instanceof Srcloc; }

export const dummyLoc: Loc = new Builtin('dummy location');
