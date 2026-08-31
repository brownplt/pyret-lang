/*
  Port of `data CompileError` from src/arr/compiler/compile-structs.arr
  (lines 646-2916). See CONVENTIONS.md.

  tostring decision: literal `tostring(...)` sites fall into two cases.
  (1) T.Type defines `_output` (backed by `to-string`), so Pyret's
  `tostring` renders a Type as its `to-string()`; the type-structs.ts port
  exposes that as `toString()`, which we call directly (type-mismatch).
  (2) A.Name and SL.Srcloc define no `_output`/`_tostring`, so Pyret
  renders them with the default structural repr, e.g.
  `s-name(srcloc("f", 1, 0, 0, 1, 3, 3), "x")`. The helper `stringify`
  below mirrors that: strings pass through unchanged, numbers via String,
  and data values (anything with a `$name`) render as
  `$name(field-reprs...)` over the constructor fields in source order
  (nested strings are quoted, like torepr). Note Name.toString() in ast.ts
  returns key() and so is NOT used here.
*/

import * as ED from './error-display';
import { Loc, Srcloc, Builtin } from './srcloc';
import * as A from './ast';
import type * as T from './type-structs';
import { drawAndHighlight, type BindOrigin } from './compile-structs';
import { toRepr as torepr } from './shared';

// ---------- local construction helpers mirroring error-display.arr's
// [ED.error: ...], [ED.para: ...], etc. construct syntax ----------

function error(contents: ED.ErrorDisplay[]): ED.ErrorDisplay { return new ED.HSequence(contents, " "); }
function sequence(contents: ED.ErrorDisplay[]): ED.ErrorDisplay { return new ED.HSequence(contents, " "); }
function para(contents: ED.ErrorDisplay[]): ED.ErrorDisplay { return new ED.Paragraph(contents); }
function paraNospace(contents: ED.ErrorDisplay[]): ED.ErrorDisplay { return new ED.HSequence(contents, ""); }
function text(s: string): ED.ErrorDisplay { return new ED.Text(s); }
function code(contents: ED.ErrorDisplay): ED.ErrorDisplay { return new ED.Code(contents); }
function highlight(contents: ED.ErrorDisplay, locs: Loc[], color: number): ED.ErrorDisplay { return new ED.Highlight(contents, locs, color); }
function edLoc(l: Loc): ED.ErrorDisplay { return new ED.Loc(l); }
function cmcode(l: Loc): ED.ErrorDisplay { return new ED.Cmcode(l); }
function embed(val: any): ED.ErrorDisplay { return new ED.Embed(val); }
function vSequence(contents: ED.ErrorDisplay[]): ED.ErrorDisplay { return new ED.VSequence(contents); }
function bulletedSequence(contents: ED.ErrorDisplay[]): ED.ErrorDisplay { return new ED.BulletedSequence(contents); }

// Pyret `l1 + l2` on srclocs (Srcloc._plus).
function locPlus(a: Loc, b: Loc): Loc { return (a as any).plus(b); }

// Pyret map_n
function mapN<T, U>(f: (n: number, x: T) => U, start: number, xs: T[]): U[] {
  return xs.map((x, i) => f(start + i, x));
}

export function stringify(v: any): string {
  if (typeof v === 'string') return v;
  if (typeof v === 'number') return String(v);
  return torepr(v);
}

export abstract class CompileErrorBase {
  abstract get $name(): string;
  abstract renderFancyReason(): ED.ErrorDisplay;
  abstract renderReason(): ED.ErrorDisplay;
}

export class WfErr extends CompileErrorBase {
  get $name(): 'wf-err' { return 'wf-err'; }
  constructor(public msg: ED.ErrorDisplay[], public loc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return this.renderReason();
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      new ED.Paragraph([highlight(text("Well-formedness:"), [this.loc], 0), text(" ")]
          .concat(this.msg)),
      cmcode(this.loc)]);
  }
}

export class WfEmptyBlock extends CompileErrorBase {
  get $name(): 'wf-empty-block' { return 'wf-empty-block'; }
  constructor(public loc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("This "),
        highlight(text("block"), [this.loc], 0),
        text(" is empty:")]),
      cmcode(this.loc)]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("Pyret rejected your program because there is an empty block at")]),
      para([drawAndHighlight(this.loc)])]);
  }
}

export class WfErrSplit extends CompileErrorBase {
  get $name(): 'wf-err-split' { return 'wf-err-split'; }
  constructor(public msg: string, public loc: Loc[]) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return this.renderReason();
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("Well-formedness: "),
        text(this.msg),
        text(" at")]),
      vSequence(this.loc.map((l) => para([drawAndHighlight(l)])))]);
  }
}

export class ReservedName extends CompileErrorBase {
  get $name(): 'reserved-name' { return 'reserved-name'; }
  constructor(public loc: Loc, public id: string) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("Reading a "),
        highlight(text("name"), [this.loc], 0),
        text(" errored:")]),
      cmcode(this.loc),
      para([
        text("This name is reserved by Pyret, and cannot be used in a definition.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The name "),
        code(text(this.id)),
        text(" at "),
        edLoc(this.loc),
        text(" is reserved by Pyret, and cannot be used in a definition.")])]);
  }
}

export class ContractOnImport extends CompileErrorBase {
  get $name(): 'contract-on-import' { return 'contract-on-import'; }
  constructor(public loc: Loc, public name: string, public importLoc: Loc, public importUri: string) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("Contracts for functions can only be defined once, and the contract for "),
        highlight(code(text(this.name)), [this.loc], 0),
        text(" is already defined in the "),
        highlight(code(text(this.importUri)),
          [this.importLoc], 1),
        text(" library.")]),
      cmcode(this.loc)]);
  }
  renderReason(): ED.ErrorDisplay {
    // NOTE: `self.import-type` does not exist on this variant in the Pyret
    // source either; this mirrors that bug faithfully (errors if run).
    return error([
      para([
        text("Contracts for functions can only be defined once, and the contract for "),
        code(text(this.name)), text(" at "), edLoc(this.loc),
        text(" is already defined in the "),
        code(text((this as any).importType.tosource().pretty(1000).join(""))),
        text(" library.")])]);
  }
}

export class ContractRedefined extends CompileErrorBase {
  get $name(): 'contract-redefined' { return 'contract-redefined'; }
  constructor(public loc: Loc, public name: string, public defnLoc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("Contracts for functions can only be defined once, and the contract for "),
        highlight(code(text(this.name)), [this.loc], 0),
        text(" is "),
        highlight(text("already defined"), [this.defnLoc], -1),
        text(": ")]),
      cmcode(this.defnLoc)]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("Contracts for functions can only be defined once, and the contract for "),
        code(text(this.name)), text(" at "), edLoc(this.loc),
        text(" is already defined at "), edLoc(this.defnLoc)])]);
  }
}

export class ContractNonFunction extends CompileErrorBase {
  get $name(): 'contract-non-function' { return 'contract-non-function'; }
  constructor(public loc: Loc, public name: string, public defnLoc: Loc, public defnIsFunction: boolean) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    if (this.defnIsFunction) {
      return error([
        para([
          text("The contract for "),
          highlight(code(text(this.name)), [this.loc], 0),
          text(" is not a valid function contract, but "),
          highlight(code(text(this.name)), [this.defnLoc], -1),
          text(" is defined as a function.")]),
        cmcode(this.loc),
        para([
          text("The contract and the "),
          highlight(text("definition"), [this.defnLoc], -1),
          text(" must be consistent.")]),
        cmcode(this.defnLoc)]);
    } else {
      return error([
        para([
          text("The contract for "),
          highlight(code(text(this.name)), [this.loc], 0),
          text(" is a function contract, but "),
          highlight(code(text(this.name)), [this.defnLoc], -1),
          text(" is not defined as a function.")]),
        cmcode(this.loc),
        para([
          text("The contract and the "),
          highlight(text("definition"), [this.defnLoc], -1),
          text(" must be consistent.")]),
        cmcode(this.defnLoc)]);
    }
  }
  renderReason(): ED.ErrorDisplay {
    if (this.defnIsFunction) {
      return error([
        para([
          text("The contract for "),
          code(text(this.name)), text(" at "), edLoc(this.loc),
          text(" is not a valid function contract, but "),
          code(text(this.name)), text(" at "), edLoc(this.defnLoc),
          text(" is defined as a function.")]),
        para([text("The contract and the definition must be consistent.")])]);
    } else {
      return error([
        para([
          text("The contract for "),
          code(text(this.name)), text(" at "), edLoc(this.loc),
          text(" is a function contract, but "),
          code(text(this.name)), text(" at "), edLoc(this.defnLoc),
          text(" is not defined as a function.")]),
        para([text("The contract and the definition must be consistent.")])]);
    }
  }
}

export class ContractInconsistentNames extends CompileErrorBase {
  get $name(): 'contract-inconsistent-names' { return 'contract-inconsistent-names'; }
  constructor(public loc: Loc, public name: string, public defnLoc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The contract for "),
        highlight(code(text(this.name)), [this.loc], 0)]),
      cmcode(this.loc),
      para([
        text("specifies arguments that are inconsistent with the "),
        highlight(text("associated definition"), [this.defnLoc], -1), text(":")]),
      cmcode(this.defnLoc)]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The contract for "),
        code(text(this.name)), text(" at "), edLoc(this.loc),
        text(" specifies arguments that are inconsistent with the definition at "), edLoc(this.defnLoc)])]);
  }
}

export class ContractInconsistentParams extends CompileErrorBase {
  get $name(): 'contract-inconsistent-params' { return 'contract-inconsistent-params'; }
  constructor(public loc: Loc, public name: string, public defnLoc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The contract for "),
        highlight(code(text(this.name)), [this.loc], 0)]),
      cmcode(this.loc),
      para([
        text("specifies type parameters that are inconsistent with the "),
        highlight(text("associated definition"), [this.defnLoc], -1), text(":")]),
      cmcode(this.defnLoc)]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The contract for "),
        code(text(this.name)), text(" at "), edLoc(this.loc),
        text(" specifies type parameters that are inconsistent with the definition at "), edLoc(this.defnLoc)])]);
  }
}

export class ContractUnused extends CompileErrorBase {
  get $name(): 'contract-unused' { return 'contract-unused'; }
  constructor(public loc: Loc, public name: string) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The contract for "),
        highlight(code(text(this.name)), [this.loc], 0)]),
      cmcode(this.loc),
      para([
        text(" does not match the name of any function definition.")]),
      para([
        text("Contracts must appear just before their function's definition (or just before the function's examples block).  Check the spelling of this contract's name, or move it closer to its function if necessary.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The contract for "), code(text(this.name)), text(" at "), edLoc(this.loc),
        text(" does not match the name of any function definition.")]),
      para([
        text("Contracts must appear just before their function's definition (or just before the function's examples block).  Check the spelling of this contract's name, or move it closer to its function if necessary.")])]);
  }
}

export class ContractBadLoc extends CompileErrorBase {
  get $name(): 'contract-bad-loc' { return 'contract-bad-loc'; }
  constructor(public loc: Loc, public name: string, public defnLoc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("Contracts must appear just before their associated definition (or just before the function's examples block).  The contract for "),
        highlight(code(text(this.name)), [this.loc], 0)]),
      cmcode(this.loc),
      para([text(" comes after its "),
        highlight(text("associated definition"), [this.defnLoc], -1), text(".")]),
      cmcode(this.defnLoc),
      para([text("Move the contract just before its function.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("Contracts must appear just before their associated definition (or just before the function's examples block).  The contract for "), code(text(this.name)), text(" at "), edLoc(this.loc),
        text(" comes after its associated definition at "), edLoc(this.defnLoc), text(". Move the contract just before its function.")])]);
  }
}

export class ZeroFraction extends CompileErrorBase {
  get $name(): 'zero-fraction' { return 'zero-fraction'; }
  constructor(public loc: any, public numerator: any) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("Reading a "),
        highlight(text("fraction literal expression"), [this.loc], 0),
        text(" errored:")]),
      cmcode(this.loc),
      para([
        text("Its denominator is zero.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("Pyret disallows the fraction literal expression")]),
      para([
        code(sequence([
          embed(this.numerator),
          text(" / 0")]))]),
      para([
        text("at "),
        edLoc(this.loc),
        text(" because its denominator is zero.")])]);
  }
}

export class MixedBinops extends CompileErrorBase {
  get $name(): 'mixed-binops' { return 'mixed-binops'; }
  constructor(public expLoc: any, public opAName: any, public opALoc: any, public opBName: any, public opBLoc: any) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("Reading this "),
        highlight(text("expression"), [this.expLoc], -1),
        text(" errored:")]),
      cmcode(this.expLoc),
      para([
        text("The "),
        code(highlight(text(this.opAName), [this.opALoc], 0)),
        text(" and "),
        code(highlight(text(this.opBName), [this.opBLoc], 1)),
        text(" operations are at the same grouping level. "),
        text("Add parentheses to group the operations, and make the order of operations clear.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("Operators of different kinds cannot be mixed at the same level, but "),
        code(text(this.opAName)),
        text(" is at "),
        edLoc(this.opALoc),
        text(" at the same level as "),
        code(text(this.opBName)),
        text(" at "),
        edLoc(this.opBLoc),
        text(". Use parentheses to group the operations and to make the order of operations clear.")])]);
  }
}

export class BlockEnding extends CompileErrorBase {
  get $name(): 'block-ending' { return 'block-ending'; }
  constructor(public l: Loc, public blockLoc: Loc, public kind: any) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("This "),
        highlight(text("block"), [this.blockLoc], -1),
        text(" ends with a "),
        highlight(text(this.kind), [this.l], 0),
        text(":")]),
      cmcode(this.l),
      para([
        text("Blocks should end with an expression")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The block at "),
        edLoc(this.blockLoc),
        text(" ends with a " + this.kind + " at "),
        edLoc(this.l),
        text(". Blocks should end with an expression.")])]);
  }
}

export class SingleBranchIf extends CompileErrorBase {
  get $name(): 'single-branch-if' { return 'single-branch-if'; }
  constructor(public expr: A.Expr) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("An "),
        highlight(text("if-expression"), [(this.expr as any).l], -1),
        text(" has only one "),
        highlight(text("branch"), [(this.expr as any).branches[0].l], 0),
        text(":")]),
      cmcode((this.expr as any).l)]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("If-expressions may not only have one branch, but the if-expression at "),
        edLoc((this.expr as any).l),
        text(" does not have any other branches.")])]);
  }
}

export class UnwelcomeWhere extends CompileErrorBase {
  get $name(): 'unwelcome-where' { return 'unwelcome-where'; }
  constructor(public kind: any, public loc: any, public blockLoc: any) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("A "),
        highlight(code(text("where")), [this.blockLoc], 0),
        text(" can't be added to a "),
        highlight(text(this.kind), [this.loc], -1),
        text(":")]),
      cmcode(this.blockLoc),
      para([
        text("A "),
        code(text("where")),
        text(" block may only be added to named function declarations"),
        text(".")])]);
  }
  renderReason(): ED.ErrorDisplay {
    // The original had ED.loc(self.kind) -- a string where a srcloc is
    // expected -- which crashed BOTH renderers (caught by wf-parity on the
    // unwelcome-where programs). Fixed to ED.text in compile-structs.arr and
    // here together.
    return error([
      para([
        code(text("`where`")),
        text(" blocks are only allowed on named function and declarations; a where block may not be added to a "),
        text(this.kind),
        text(" at "),
        edLoc(this.loc),
        text(".")])]);
  }
}

export class NonExample extends CompileErrorBase {
  get $name(): 'non-example' { return 'non-example'; }
  constructor(public expr: A.Expr) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        highlight(text("This"), [(this.expr as any).l], 0),
        text(" is not a testing statement:")]),
      cmcode((this.expr as any).l),
      para([
        code(text("example")),
        text(" blocks must only contain testing statements.  "),
        text("A test consists of an expression followed by an answer connected by a testing keyword, usually "),
        code(text("is")), text(".")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        code(text("example")),
        text(" blocks must only contain testing statements, but the statement at "),
        edLoc((this.expr as any).l),
        text(" isn't a testing statement.  "),
        text("A test consists of an expression followed by an answer connected by a testing keyword, usually ")])]);
  }
}

export class TupleGetBadIndex extends CompileErrorBase {
  get $name(): 'tuple-get-bad-index' { return 'tuple-get-bad-index'; }
  constructor(public l: any, public tup: any, public index: any, public indexLoc: any) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    if (!Number.isInteger(this.index)) {
      return error([
        para([
          text("This "),
          highlight(text("tuple indexing"), [this.l], -1),
          text(" expression cannot extract a "),
          highlight(text("non-integer position"), [this.indexLoc], 0),
          text(".")]),
        cmcode(this.l)]);
    } else if (this.index < 0) {
      return error([
        para([
          text("This "),
          highlight(text("tuple indexing"), [this.l], -1),
          text(" expression cannot extract a "),
          highlight(text("negative position"), [this.indexLoc], 0),
          text(".")]),
        cmcode(this.l)]);
    } else {
      return error([
        para([
          text("This "),
          highlight(text("tuple indexing"), [this.l], -1),
          text(" expression cannot extract an "),
          highlight(text("index"), [this.indexLoc], 0),
          text(" that large. There are no tuples that big.")]),
        cmcode(this.l)]);
    }
  }
  renderReason(): ED.ErrorDisplay {
    if (!Number.isInteger(this.index)) {
      return error([
        para([
          text("The tuple indexing expression at "),
          edLoc(this.l),
          text(" was given an invalid, non-integer index.")])]);
    } else if (this.index < 0) {
      return error([
        para([
          text("The tuple indexing expression at "),
          edLoc(this.l),
          text(" was given an invalid, negative index.")])]);
    } else {
      return error([
        para([
          text("The tuple indexing expression at "),
          edLoc(this.l),
          text(" was given an index bigger than any tuple.")])]);
    }
  }
}

export class ImportArityMismatch extends CompileErrorBase {
  get $name(): 'import-arity-mismatch' { return 'import-arity-mismatch'; }
  constructor(public l: any, public kind: any, public args: any, public expectedArity: any, public expectedArgs: any) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("This "),
        highlight(sequence([code(text(this.kind)), text(" import statement")]),
                  [this.l], -1),
        text(":")]),
      cmcode(this.l),
      para([
        text("expects "),
        ED.edArgs(this.expectedArity),
        text(":")]),
      bulletedSequence(this.expectedArgs.map((s: string) => text(s)))]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("This "),
        code(text(this.kind)),
        text(" import statement at "),
        edLoc(this.l),
        text(" expects "),
        ED.edArgs(this.expectedArity),
        text(":")]),
      bulletedSequence(this.expectedArgs.map((s: string) => text(s)))]);
  }
}

export class NoArguments extends CompileErrorBase {
  get $name(): 'no-arguments' { return 'no-arguments'; }
  constructor(public expr: any) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("This "),
        highlight(text("method declaration"), [this.expr.l], 0),
        text(" should accept at least one argument:")]),
      cmcode(this.expr.l),
      para([
        text("When a method is applied, the first argument is a reference to the object it belongs to.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("Method declarations should accept at least one argument, but the method declaration at "),
        edLoc(this.expr.l),
        text(" has no arguments. When a method is applied, the first argument is a reference to the object it belongs to.")])]);
  }
}

export class NonToplevel extends CompileErrorBase {
  get $name(): 'non-toplevel' { return 'non-toplevel'; }
  constructor(public kind: any, public l: Loc, public parentLoc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("This "),
        code(highlight(text(this.kind), [this.l], 0)),
        text(" is inside "),
        highlight(text("another block"), [this.parentLoc], -1),
        text(":")]),
      cmcode(this.l),
      para([
        text(this.kind),
        text(" may only occur at the top-level of the program.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("You may only define the "),
        code(text(this.kind)),
        text(" at "),
        edLoc(this.l),
        text(" at the top-level.")])]);
  }
}

export class UnwelcomeTest extends CompileErrorBase {
  get $name(): 'unwelcome-test' { return 'unwelcome-test'; }
  constructor(public loc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The "),
        highlight(text("testing statement"), [this.loc], 0)]),
      cmcode(this.loc),
      para([
        text("is not inside a "),
        code(text("check")),
        text(", "), code(text("where")),
        text(" or "),
        code(text("examples")),
        text(" block.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The testing statement at "),
        edLoc(this.loc),
        text(" is not inside a "),
        code(text("check")),
        text(", "), code(text("where")),
        text(" or "),
        code(text("examples")),
        text(" block.")])]);
  }
}

export class UnwelcomeTestRefinement extends CompileErrorBase {
  get $name(): 'unwelcome-test-refinement' { return 'unwelcome-test-refinement'; }
  constructor(public refinement: any, public op: any) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("This "),
        highlight(text("testing operator"), [this.op.l], 0),
        text(" may not be used with a "),
        highlight(text("refinement"), [this.refinement.l], 1),
        text(":")]),
      cmcode(locPlus(this.op.l, this.refinement.l))]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The testing operator at "),
        edLoc(this.op.l),
        text(" may not be used with the refinement syntax, "),
        code(text("%(...)"))])]);
  }
}

export class UnderscoreAs extends CompileErrorBase {
  get $name(): 'underscore-as' { return 'underscore-as'; }
  constructor(public l: Loc, public kind: any) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The underscore "),
        code(highlight(text("_"), [this.l], 0)),
        text(" cannot be used as "),
        text(this.kind),
        text(".")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The underscore "),
        code(text("_")),
        text(" at "),
        edLoc(this.l),
        text(" cannot be used as "),
        text(this.kind),
        text(".")])]);
  }
}

export class UnderscoreAsPattern extends CompileErrorBase {
  get $name(): 'underscore-as-pattern' { return 'underscore-as-pattern'; }
  constructor(public l: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("An underscore cannot be used for this "),
        highlight(text("pattern"), [this.l], 0),
        text(" in a cases expression:")]),
      cmcode(this.l),
      para([
        text("To match all cases not matched by the other branches, use the pattern "),
        code(text("else")),
        text(" instead.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The underscore "),
        code(text("_")),
        text(" at "),
        edLoc(this.l),
        text(" cannot be used as a pattern in a cases expression. To match all cases not matched by the previous branches, use the pattern "),
        code(text("else")),
        text(" instead.")])]);
  }
}

export class UnderscoreAsExpr extends CompileErrorBase {
  get $name(): 'underscore-as-expr' { return 'underscore-as-expr'; }
  constructor(public l: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The underscore "),
        code(highlight(text("_"), [this.l], 0)),
        text(" cannot be used where an expression is expected.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The underscore "),
        code(text("_")),
        text(" at "),
        edLoc(this.l),
        text(" cannot be used where an expression is expected.")])]);
  }
}

export class UnderscoreAsAnn extends CompileErrorBase {
  get $name(): 'underscore-as-ann' { return 'underscore-as-ann'; }
  constructor(public l: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The underscore "),
        code(highlight(text("_"), [this.l], 0)),
        text(" cannot be used where a type annotation is expected.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The underscore "),
        code(text("_")),
        text(" at "),
        edLoc(this.l),
        text(" cannot be used where a type annotation is expected.")])]);
  }
}

export class BlockNeeded extends CompileErrorBase {
  get $name(): 'block-needed' { return 'block-needed'; }
  constructor(public exprLoc: Loc, public blocks: A.Expr[]) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    if (this.blocks.length > 1) {
      return error([
        para([
          text("This expression contains one or more "),
          highlight(text("blocks"), this.blocks.map((b: any) => b.l), -1),
          text(" that contain "),
          highlight(text("multiple expressions"), A.flatten(this.blocks.map((b: any) => b.stmts)).filter((e: any) => !A.isBinder(e)).map((e: any) => e.l), 0),
          text(":")]),
        cmcode(this.exprLoc),
        para([
          text("Either simplify each of these blocks to a single expression, or mark the outer expression with"),
          code(text("block:")), text("to indicate this is deliberate.")])]);
    } else {
      return error([
        para([
          text("This expression contains a "),
          highlight(text("block"), [(this.blocks[0] as any).l], -1),
          text(" that contains "),
          highlight(text("multiple expressions"), A.flatten(this.blocks.map((b: any) => b.stmts)).filter((e: any) => !A.isBinder(e)).map((e: any) => e.l), 0),
          text(".")]),
        cmcode(this.exprLoc),
        para([
          text("Either simplify this block to a single expression, or mark the outer expression with "),
          code(text("block:")), text(" to indicate this is deliberate.")])]);
    }
  }
  renderReason(): ED.ErrorDisplay {
    if (this.blocks.length > 1) {
      return error([
        para([text("The expression at "), drawAndHighlight(this.exprLoc),
          text(" contains several blocks that each contain multiple expressions:")]),
        vSequence(this.blocks.map((b: any) => b.l).map(drawAndHighlight)),
        para([
          text("Either simplify each of these blocks to a single expression, or mark the outer expression with "),
          code(text("block:")), text(" to indicate this is deliberate.")])]);
    } else {
      return error([
        para([text("The expression at "), drawAndHighlight(this.exprLoc),
          text(" contains a block that contains multiple expressions:")]),
        vSequence(this.blocks.map((b: any) => b.l).map(drawAndHighlight)),
        para([
          text("Either simplify this block to a single expression, or mark the outer expression with "),
          code(text("block:")), text(" to indicate this is deliberate.")])]);
    }
  }
}

export class NameNotProvided extends CompileErrorBase {
  get $name(): 'name-not-provided' { return 'name-not-provided'; }
  constructor(public nameLoc: any, public impLoc: any, public name: A.Name, public typ: string) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    if (this.nameLoc instanceof Builtin) {
      return para([
        text("ERROR: should not be allowed to have a builtin import that's not defined"),
        text(this.name.toname()), text("at"),
        drawAndHighlight(this.nameLoc)]);
    } else {
      return error([
        para([
          text("The name "),
          code(highlight(text(this.name.toname()), [this.nameLoc], 0)),
          text(" is not provided as a " + this.typ + " in the import at ")]),
        cmcode(this.impLoc)]);
    }
  }
  renderReason(): ED.ErrorDisplay {
    if (this.nameLoc instanceof Builtin) {
      return para([
        text("ERROR: should not be allowed to have a builtin import that's not defined"),
        text(this.name.toname()), text("at"),
        drawAndHighlight(this.nameLoc)]);
    } else {
      return error([
        para([
          text("The name "),
          code(text(this.name.toname())),
          text(" at "),
          edLoc(this.nameLoc),
          text(" is not provided as a " + this.typ + " in the import at "),
          edLoc(this.impLoc)])]);
    }
  }
}

export class UnboundId extends CompileErrorBase {
  get $name(): 'unbound-id' { return 'unbound-id'; }
  constructor(public id: A.Expr) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    if ((this.id as any).l instanceof Builtin) {
      return para([
        text("ERROR: should not be allowed to have a builtin that's unbound:"),
        text((this.id as any).id.toname()), text("at"),
        drawAndHighlight((this.id as any).l)]);
    } else {
      return error([
        para([
          text("The name "),
          code(highlight(text((this.id as any).id.toname()), [(this.id as any).l], 0)),
          text(" is unbound:")]),
        cmcode((this.id as any).l),
        para([
          text("It is "),
          highlight(text("used"), [(this.id as any).l], 0),
          text(" but not previously defined.  You may need to run the program, or check dashes and capitalization in the name.")])]);
    }
  }
  renderReason(): ED.ErrorDisplay {
    if ((this.id as any).l instanceof Builtin) {
      return para([
        text("ERROR: should not be allowed to have a builtin that's unbound:"),
        text((this.id as any).id.toname()),
        drawAndHighlight((this.id as any).l)]);
    } else {
      return error([
        para([
          text("The name "),
          code(text((this.id as any).id.toname())),
          text(" at "),
          edLoc((this.id as any).l),
          text(" is unbound. It is "),
          text("used but not previously defined.  You may need to run the program, or check dashes and capitalization in the name.")])]);
    }
  }
}

export class UnboundVar extends CompileErrorBase {
  get $name(): 'unbound-var' { return 'unbound-var'; }
  constructor(public id: string, public loc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    if (this.loc instanceof Builtin) {
      return para([
        text("ERROR: should not be allowed to have a builtin that's unbound:"),
        text(this.id),
        drawAndHighlight(this.loc)]);
    } else {
      return error([
        para([
          text("The variable "),
          code(highlight(text(this.id), [this.loc], 0)),
          text(" is unbound. It is "),
          highlight(text("assigned to"), [this.loc], 0),
          text(" but not previously defined.  You may need to run the program, or check dashes and capitalization in the name.")])]);
    }
  }
  renderReason(): ED.ErrorDisplay {
    if (this.loc instanceof Builtin) {
      return para([
        text("ERROR: should not be allowed to have a builtin that's unbound:"),
        text(this.id),
        drawAndHighlight(this.loc)]);
    } else {
      return error([
        para([
          text("The variable "),
          code(text(this.id)),
          text(" at "),
          edLoc(this.loc),
          text(" is unbound. It is "),
          text("used but not previously defined.  You may need to run the program, or check dashes and capitalization in the name.")])]);
    }
  }
}

export class UnboundTypeId extends CompileErrorBase {
  get $name(): 'unbound-type-id' { return 'unbound-type-id'; }
  constructor(public ann: A.Ann) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    // NOTE: the builtin branch references `self.id.l`, which does not exist
    // on this variant in the Pyret source either; ported faithfully.
    if ((this.ann as any).l instanceof Builtin) {
      return para([
        text("ERROR: should not be allowed to have a builtin that's unbound:"),
        text((this.ann as any).tosource().pretty(1000)[0]),
        drawAndHighlight((this as any).id.l)]);
    } else {
      return error([
        para([
          text("The name "),
          code(highlight(text((this.ann as any).tosource().pretty(1000).join("")), [(this.ann as any).l], 0)),
          text(" is used to indicate a type, but a definition of a type named "),
          code(highlight(text((this.ann as any).tosource().pretty(1000).join("")), [(this.ann as any).l], 0)),
          text(" could not be found.")])]);
    }
  }
  renderReason(): ED.ErrorDisplay {
    if ((this.ann as any).l instanceof Builtin) {
      return para([
        text("ERROR: should not be allowed to have a builtin that's unbound:"),
        text((this.ann as any).tosource().pretty(1000)[0]), text("at"),
        drawAndHighlight((this as any).id.l)]);
    } else {
      const annName = (this.ann as any).tosource().pretty(1000).join("");
      return error([
        para([
          text("The name "),
          code(text(annName)),
          text(" at "),
          edLoc((this.ann as any).l),
          text(" is used to indicate a type, but a definition of a type named "),
          code(text(annName)),
          text(" could not be found.")])]);
    }
  }
}

export class TypeIdUsedInDotLookup extends CompileErrorBase {
  get $name(): 'type-id-used-in-dot-lookup' { return 'type-id-used-in-dot-lookup'; }
  constructor(public loc: Loc, public name: A.Name) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The "),
        highlight(text("name"), [this.loc], 0),
        text(" is being used with a dot accessor as if to access a type within another module.")]),
      cmcode(this.loc),
      para([
        text("but it does not refer to a module.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      paraNospace([
        text("The name "),
        text(stringify(this.name)),
        text(" is being used with a dot accessor as if to access a type within another module at "),
        drawAndHighlight(this.loc),
        text(", but it does not refer to a module.")])]);
  }
}

export class TypeIdUsedAsValue extends CompileErrorBase {
  get $name(): 'type-id-used-as-value' { return 'type-id-used-as-value'; }
  constructor(public id: A.Name, public origin: BindOrigin) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    const intro =
      para([
        text("This "),
        highlight(text("name"), [(this.id as any).l], 0),
        text(" is being used as a value:")]);
    const usage = cmcode((this.id as any).l);
    const { definitionBindSite: ldef, newDefinition: newdef, uriOfDefinition: uri } = this.origin as any;
    if (newdef) {
      return error([intro, usage,
        para([
          text("But it is "),
          highlight(text("defined as a type"), [ldef], 1),
          text(":")]),
        cmcode(ldef)]);
    } else {
      // TODO(joe/ben): This may be able to use lbind and ldef when they
      // are more refined; come back to this
      return error([intro, usage,
        para([
          text("But it is defined as a type in "),
          embed(uri),
          text(".")])]);
    }
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      paraNospace([
        text("The name "),
        text((this.id as any).s),
        text(" is used as a value at "),
        drawAndHighlight((this.id as any).l),
        text(", but it is defined as a type.")])]);
  }
}

export class UnexpectedTypeVar extends CompileErrorBase {
  get $name(): 'unexpected-type-var' { return 'unexpected-type-var'; }
  constructor(public loc: Loc, public name: A.Name) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    // NOTE: the color argument is `self.loc` in the Pyret source (a srcloc,
    // not a number); ported faithfully.
    return error([
      para([
        text("The "),
        highlight(text("name"), [this.loc], this.loc as any),
        text(" is used in a dot-annotation")]),
      cmcode(this.loc),
      para([
        text("but is bound as a type variable.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    //### TODO ###
    return error([
      paraNospace([
        text("The name "),
        text(stringify(this.name)),
        text(" is used in a dot-annotation at "),
        drawAndHighlight(this.loc),
        text(", but is bound as a type variable")])]);
  }
}

export class PointlessVar extends CompileErrorBase {
  get $name(): 'pointless-var' { return 'pointless-var'; }
  constructor(public loc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    if (this.loc instanceof Builtin) {
      return error([
        para([
          text("ERROR: should not be allowed to have a builtin that's anonymous:"),
          drawAndHighlight(this.loc)])]);
    } else {
      return error([
        para([
          text("This "),
          highlight(text("variable binding"), [this.loc], 0),
          text(" is pointless:")]),
        cmcode(this.loc),
        para([
          text("There is no name that can be used to mutate it later on.")])]);
    }
  }
  renderReason(): ED.ErrorDisplay {
    if (this.loc instanceof Builtin) {
      return para([
        text("ERROR: should not be allowed to have a builtin that's anonymous:"),
        drawAndHighlight(this.loc)]);
    } else {
      return error([
        para([
          text("Defining the anonymous variable "),
          code(text("var _")),
          text(" at "),
          edLoc(this.loc),
          text(" is pointless since there is no name that can be used to mutate it later on.")])]);
    }
  }
}

export class PointlessRec extends CompileErrorBase {
  get $name(): 'pointless-rec' { return 'pointless-rec'; }
  constructor(public loc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    if (this.loc instanceof Builtin) {
      return error([
        para([
          text("ERROR: should not be allowed to have a builtin that's anonymous:"),
          drawAndHighlight(this.loc)])]);
    } else {
      return error([
        para([
          text("This "),
          highlight(text("recursive binding"), [this.loc], 0),
          text(" is pointless:")]),
        cmcode(this.loc),
        para([
          text("There isn't a name that can be used to make a recursive call.")])]);
    }
  }
  renderReason(): ED.ErrorDisplay {
    if (this.loc instanceof Builtin) {
      return error([
        para([
          text("ERROR: should not be allowed to have a builtin that's anonymous:"),
          drawAndHighlight(this.loc)])]);
    } else {
      return error([
        para([
          text("Defining the anonymous recursive binding "),
          code(text("rec _")),
          text(" at "),
          edLoc(this.loc),
          text(" is pointless since there is no name to call recursively.")])]);
    }
  }
}

export class PointlessShadow extends CompileErrorBase {
  get $name(): 'pointless-shadow' { return 'pointless-shadow'; }
  constructor(public loc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    if (this.loc instanceof Builtin) {
      return para([
        text("ERROR: should not be allowed to have a builtin that's anonymous:"),
        drawAndHighlight(this.loc)]);
    } else {
      return error([
        para([
          text("This "),
          highlight(text("shadowing binding"), [this.loc], 0),
          text(" is pointless:")]),
        cmcode(this.loc),
        para([
          text("There is no name to shadow.")])]);
    }
  }
  renderReason(): ED.ErrorDisplay {
    if (this.loc instanceof Builtin) {
      return para([
        text("ERROR: should not be allowed to have a builtin that's anonymous:"),
        drawAndHighlight(this.loc)]);
    } else {
      return error([
        para([
          text("The anonymous binding "),
          code(text("shadow _")),
          text(" at "),
          edLoc(this.loc),
          text(" cannot shadow anything: there is no name to shadow.")])]);
    }
  }
}

export class BadAssignment extends CompileErrorBase {
  get $name(): 'bad-assignment' { return 'bad-assignment'; }
  constructor(public iuse: A.Expr, public idef: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    const useLocColor = 0;
    const defLocColor = 1;
    return error([
      para([
        text("The "),
        highlight(text("variable assignment statement"), [(this.iuse as any).l], useLocColor)]),
      cmcode((this.iuse as any).l),
      para([
        text(" expects the name "),
        code(highlight(text((this.iuse as any).id.toname()), [(this.iuse as any).l], useLocColor)),
        text(" to refer to a variable definition statement, but "),
        code(text((this.iuse as any).id.toname())),
        text(" is declared by an "),
        highlight(text("identifier definition statement."), [this.idef], defLocColor)]),
      cmcode(this.idef)]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The variable assignment expression "),
        code(text((this.iuse as any).tosource().pretty(1000)[0])),
        text(" at "),
        edLoc((this.iuse as any).l),
        text(" expects the name "),
        code(text((this.iuse as any).id.toname())),
        text(" to refer to a variable definition expression, but "),
        code(text((this.iuse as any).id.toname())),
        text(" is declared by an identifier definition expression at "),
        edLoc(this.idef)])]);
  }
}

export class MixedIdVar extends CompileErrorBase {
  get $name(): 'mixed-id-var' { return 'mixed-id-var'; }
  constructor(public id: string, public varLoc: Loc, public idLoc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The name "),
        code(text(this.id)),
        text(" is both "),
        highlight(text("declared as a variable"), [this.varLoc], 0)]),
      cmcode(this.varLoc),
      para([
        text("and "),
        highlight(text("declared as an identifier"), [this.idLoc], 1)]),
      cmcode(this.idLoc)]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text(this.id + " is declared as both a variable (at " + stringify(this.varLoc) + ")"
            + " and an identifier (at " + (this.idLoc as any).format(!(this.varLoc as any).sameFile(this.idLoc)) + ")")])]);
  }
}

export class ShadowId extends CompileErrorBase {
  get $name(): 'shadow-id' { return 'shadow-id'; }
  // TODO: disambiguate what is doing the shadowing and what is being shadowed.
  // it's not necessarily a binding; could be a function definition.
  constructor(public id: string, public newLoc: Loc, public oldLoc: Loc, public importLoc: Loc | undefined) { super(); }
  renderFancyReason(): ED.ErrorDisplay {

    // included in definitions, shadowed in definitions
    // included in definitions, shadowed in interactions
    // global in definitions, shadowed in definitions
    // global in definitions, shadowed in interactions
    // everything else mentions the name somewhere as a local-bind-site

    const oldLocColor = 0;
    const newLocColor = 1;
    const impLocColor = 2;
    if (this.oldLoc instanceof Builtin) {
      if (this.importLoc === undefined) {
        return error([
          para([
            text("The declaration of "),
            highlight(code(text(this.id)), [this.newLoc], newLocColor),
            text(" shadows the declaration of a built-in of the same name.")])]);
      } else {
        const impLoc = this.importLoc;
        return error([
          para([
            text("The declaration of "),
            highlight(code(text(this.id)), [this.newLoc], newLocColor),
            text(" shadows the declaration of a built-in of the same name, which was imported "),
            highlight(code(text("here")), [impLoc], impLocColor)])]);
      }
    } else {
      const filename = (this.oldLoc as Srcloc).source;
      const isBuiltinLoc = filename.indexOf("builtin://") === 0;
      if (this.importLoc === undefined) {
        return error([
          isBuiltinLoc ?
            para([
              text("The declaration of "),
              highlight(code(text(this.id)), [this.newLoc], newLocColor),
              text(" shadows a built-in declaration of the same name.")])
          :
            para([
              text("The declaration of "),
              highlight(code(text(this.id)), [this.newLoc], newLocColor),
              text(" shadows a previous declaration of "),
              highlight(code(text(this.id)), [this.oldLoc], oldLocColor)])
          ]);
      } else {
        const impLoc = this.importLoc;
        if ((impLoc as any).key() === (this.oldLoc as any).key()) {
          return para([
            text("The declaration of "),
            highlight(code(text(this.id)), [this.newLoc], newLocColor),
            text(" shadows a previous declaration of "),
            highlight(code(text(this.id)), [this.oldLoc], oldLocColor)]);
        } else {
          return error([
            isBuiltinLoc ?
              para([
                text("The declaration of "),
                highlight(code(text(this.id)), [this.newLoc], newLocColor),
                text(" shadows a built-in declaration of the same name, which was imported "),
                highlight(code(text("here")), [impLoc], impLocColor)])
            :
              para([
                text("The declaration of "),
                highlight(code(text(this.id)), [this.newLoc], newLocColor),
                text(" shadows a previous declaration of "),
                highlight(code(text(this.id)), [this.oldLoc], oldLocColor),
                text(", which was imported "),
                highlight(code(text("here")), [impLoc], impLocColor)])
            ]);
        }
      }
    }
  }
  renderReason(): ED.ErrorDisplay {
    if (this.oldLoc instanceof Builtin) {
      if (this.importLoc === undefined) {
        return error([
          para([
            text("7The declaration of "),
            code(text(this.id)),
            text(" at "),
            edLoc(this.newLoc),
            text(" shadows the declaration of a built-in of the same name, defined at "),
            edLoc(this.oldLoc)])]);
      } else {
        return error([
          para([
            text("8The declaration of "),
            code(text(this.id)),
            text(" at "),
            edLoc(this.newLoc),
            text(" shadows the declaration of a built-in of the same name, defined at "),
            edLoc(this.oldLoc),
            text(" and imported from "),
            edLoc(this.importLoc)])]);
      }
    } else {
      if (this.importLoc === undefined) {
        return error([
          para([
            text("9The declaration of "),
            code(text(this.id)),
            text(" at "),
            edLoc(this.newLoc),
            text(" shadows a previous declaration of "),
            code(text(this.id)),
            text(" defined at "),
            edLoc(this.oldLoc)])]);
      } else {
        return error([
          para([
            text("0The declaration of "),
            code(text(this.id)),
            text(" at "),
            edLoc(this.newLoc),
            text(" shadows a previous declaration of "),
            code(text(this.id)),
            text(" defined at "),
            edLoc(this.oldLoc),
            text(" and imported from "),
            edLoc(this.importLoc)])]);
      }
    }
  }
}

export class DuplicateId extends CompileErrorBase {
  get $name(): 'duplicate-id' { return 'duplicate-id'; }
  constructor(public id: string, public newLoc: Loc, public oldLoc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    const oldLocColor = 0;
    const newLocColor = 1;
    if (this.oldLoc instanceof Builtin) {
      return error([
        para([
          text("The declaration named "),
          highlight(code(text(this.id)), [this.newLoc], newLocColor),
          text(" is preceeded in the same scope by another declaration also named "),
          highlight(code(text(this.id)), [this.oldLoc], oldLocColor),
          text(".")])]);
    } else {
      return error([
        para([
          text("This declaration of a "),
          highlight(text("name"), [this.newLoc], 0),
          text(" conflicts with an earlier declaration of the "),
          highlight(text("same name"), [this.oldLoc], 1),
          text(":")]),
        cmcode(this.oldLoc),
        cmcode(this.newLoc)]);
    }
  }
  renderReason(): ED.ErrorDisplay {
    if (this.oldLoc instanceof Builtin) {
      return error([
        para([
          text("The declaration named "),
          code(text(this.id)),
          text(" at "),
          edLoc(this.newLoc),
          text(" is preceeded in the same scope by another declaration also named "),
          code(text(this.id)),
          text(" at "),
          edLoc(this.oldLoc)])]);
    } else {
      return error([
        para([
          text("The declaration named "),
          code(text(this.id)),
          text(" at "),
          edLoc(this.newLoc),
          text(" is preceeded in the same scope by another declaration also named "),
          code(text(this.id)),
          text(" at "),
          edLoc(this.oldLoc)])]);
    }
  }
}

export class DuplicateField extends CompileErrorBase {
  get $name(): 'duplicate-field' { return 'duplicate-field'; }
  constructor(public id: string, public newLoc: Loc, public oldLoc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    const adjust = (l: any): Loc => {
      const n = this.id.length;
      return new Srcloc(l.source,
        l.startLine, l.startColumn, l.startChar,
        l.startLine, l.startColumn + n, l.startChar + n);
    };
    const oldLocColor = 0;
    const newLocColor = 1;
    return error([
      para([
        text("The declaration of the field named "),
        highlight(code(text(this.id)), [adjust(this.newLoc)], newLocColor),
        text(" is preceeded by declaration of an field also named "),
        highlight(code(text(this.id)), [adjust(this.oldLoc)], oldLocColor),
        text(":")]),
      cmcode(locPlus(this.oldLoc, this.newLoc)),
      para([text("Pick a different name for one of them.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The declaration of the field named "),
        code(text(this.id)),
        text(" at "),
        edLoc(this.newLoc),
        text(" is preceeded in the same object by a field that is also named "),
        code(text(this.id)),
        text(" at "),
        edLoc(this.oldLoc),
        text(".")]),
      para([text("Pick a different name for one of them.")])]);
  }
}

export class SameLine extends CompileErrorBase {
  get $name(): 'same-line' { return 'same-line'; }
  constructor(public a: Loc, public b: Loc, public bIsParen: boolean) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        highlight(text("This expression"), [this.a], 0),
        text(" is on the same line as "),
        highlight(text("another expression"), [this.b], 1),
        text(":")]),
      cmcode(locPlus(this.a, this.b)),
      this.bIsParen ?
        para([
          text("Each expression within a block should be on its own line.  "),
          text("If you meant to write a function call, there should be no space between the "),
          highlight(text("function expression"), [this.a], 0),
          text(" and the "), highlight(text("arguments"), [this.b], 1), text(".")])
      :
        para([
          text("Each expression within a block should be on its own line.")])
      ]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("Pyret expects each expression within a block to have its own line, but the expression at "),
        edLoc(this.a),
        text(" is on the same line as the expression at "),
        edLoc(this.b),
        text(".")])]);
  }
}

export class TemplateSameLine extends CompileErrorBase {
  get $name(): 'template-same-line' { return 'template-same-line'; }
  constructor(public a: Loc, public b: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("There are two "),
        highlight(text("unfinished template expressions"), [this.a, this.b], 0),
        text(" on the same line.")]),
      cmcode(locPlus(this.a, this.b)),
      para([
        text("Either remove one, or separate them.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("There are two unfinished template expressions on the same line at "),
        edLoc(locPlus(this.a, this.b)),
        text(". Either remove one, or separate them.")])]);
  }
}

export class TypeMismatch extends CompileErrorBase {
  get $name(): 'type-mismatch' { return 'type-mismatch'; }
  constructor(public type1: T.Type, public type2: T.Type) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    const ordered = this.type1.l.before(this.type2.l) ? [this.type1, this.type2] : [this.type2, this.type1];
    const type1 = ordered[0];
    const type2 = ordered[1];
    return error([
      para([
        text("Type checking failed because of a type inconsistency.")]),
      para([
        text("The type constraint "),
        highlight(text(type1.toString()), [type1.l], 0),
        text(" was incompatible with the type constraint "),
        highlight(text(type2.toString()), [type2.l], 1)])]);
  }
  renderReason(): ED.ErrorDisplay {
    const ordered = this.type1.l.before(this.type2.l) ? [this.type1, this.type2] : [this.type2, this.type1];
    const type1 = ordered[0];
    const type2 = ordered[1];
    return error([
      para([
        text("Type checking failed because of a type inconsistency.")]),
      para([
        text("The type constraint "),
        code(text(type1.toString())),
        text(" at "), drawAndHighlight(type1.l),
        text(" was incompatible with the type constraint "),
        code(text(type2.toString())),
        text(" at "), drawAndHighlight(type2.l)])]);
  }
}

export class IncorrectType extends CompileErrorBase {
  get $name(): 'incorrect-type' { return 'incorrect-type'; }
  constructor(public badName: string, public badLoc: Loc, public expectedName: string, public expectedLoc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The type checker rejected your program because it found a "),
        highlight(text(this.badName), [this.badLoc], 0),
        text(" but it expected a "),
        highlight(text(this.expectedName), [this.expectedLoc], 1)])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      paraNospace([
        text("Expected to find "), code(text(this.expectedName)),
        text(" at "), drawAndHighlight(this.badLoc),
        text(", required by "), drawAndHighlight(this.expectedLoc),
        text(", but instead found "), code(text(this.badName)), text(".")])]);
  }
}

export class IncorrectTypeExpression extends CompileErrorBase {
  get $name(): 'incorrect-type-expression' { return 'incorrect-type-expression'; }
  constructor(public badName: string, public badLoc: Loc, public expectedName: string, public expectedLoc: Loc, public e: A.Expr) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The type checker rejected the expression")]),
      para([
        cmcode((this.e as any).l)]),
      para([
        text("because it found a "),
        highlight(text(this.badName), [this.badLoc], 0),
        text(" but it expected a "),
        highlight(text(this.expectedName), [this.expectedLoc], 1)])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The type checker rejected the expression")]),
      para([
        code(vSequence((this.e as any).tosource().pretty(80).map((s: string) => text(s))))]),
      para([
        text("because the expression at "), drawAndHighlight(this.badLoc),
        text(" was of type "), code(text(this.badName)),
        text(" but it was expected to be of type "), code(text(this.expectedName)),
        text(" because of "), drawAndHighlight(this.expectedLoc)])]);
  }
}

export class BadTypeInstantiation extends CompileErrorBase {
  get $name(): 'bad-type-instantiation' { return 'bad-type-instantiation'; }
  constructor(public appType: T.Type /* %(is-t-app) */, public expectedLength: any) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The type checker rejected your program because the type application "),
        highlight(embed(this.appType), [this.appType.l], 0),
        text(" expected " + stringify(this.expectedLength) + " type arguments, "),
        text("but it received " + stringify((this.appType as any).args.length))])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The type checker rejected your program because the type application "),
        highlight(embed(this.appType), [this.appType.l], 0),
        text(" expected " + stringify(this.expectedLength) + " type arguments, "),
        text("but it received " + stringify((this.appType as any).args.length))])]);
  }
}

export class IncorrectNumberOfArgs extends CompileErrorBase {
  get $name(): 'incorrect-number-of-args' { return 'incorrect-number-of-args'; }
  constructor(public appExpr: any, public funTyp: any) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    const edApplicant = highlight(text("applicant"), [this.appExpr._fun.l], 0);
    return error([
      para([
        text("The "),
        highlight(text("function application"), [this.appExpr.l], -1)]),
      cmcode(this.appExpr.l),
      para([
        text("expects the "), edApplicant,
        text(" to evaluate to a function that accepts exactly the same number of arguments as are given to it.")]),
      para([
        highlight(ED.edArgs(this.appExpr.args.length), this.appExpr.args.map((a: any) => a.l), 1),
        text(" " + (this.appExpr.args.length === 1 ? "is " : "are ")
              + "given, but the type signature of the "),
        edApplicant]),
      para([
        embed(this.funTyp)]),
      para([
        text("indicates that it evaluates to a function accepting exactly "),
        ED.edArgs(this.funTyp.args.length),
        text(".")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The type checker rejected your program because the function application expression")]),
      para([
        code(vSequence(this.appExpr.tosource().pretty(80).map((s: string) => text(s))))]),
      para([
        text("expects the applicant at "),
        edLoc(this.appExpr._fun.l),
        text(" to evaluate to a function accepting exactly the same number of arguments as given to it in application.")]),
      para([
        text("However, the applicant is given "),
        ED.edArgs(this.appExpr.args.length),
        text(" and the type signature of the applicant")]),
      para([
        embed(this.funTyp)]),
      para([
        text("indicates that it evaluates to a function accepting exactly "),
        ED.edArgs(this.funTyp.args.length),
        text(".")])]);
  }
}

export class MethodMissingSelf extends CompileErrorBase {
  get $name(): 'method-missing-self' { return 'method-missing-self'; }
  // TODO: is this a duplicate of `no-arguments`???
  constructor(public expr: A.Expr) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The "),
        highlight(text("method declaration"), [(this.expr as any).l], 0)]),
      cmcode((this.expr as any).l),
      para([
        text(" does not accept at least one argument. When a method is applied, the first argument is a reference to the object it belongs to.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("Method declarations are expected to accept at least one argument, but the method declaration at "),
        edLoc((this.expr as any).l),
        text(" has no arguments. When a method is applied, the first argument is a reference to the object it belongs to.")])]);
  }
}

export class ApplyNonFunction extends CompileErrorBase {
  get $name(): 'apply-non-function' { return 'apply-non-function'; }
  constructor(public appExpr: A.Expr, public typ: any) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    const edApplicant = highlight(text("applicant"), [(this.appExpr as any)._fun.l], 0);
    return error([
      para([
        text("The "),
        highlight(text("function application"), [(this.appExpr as any).l], -1)]),
      cmcode((this.appExpr as any).l),
      para([
        text("expects the "), edApplicant,
        text(" to evaluate to a function value.")]),
      para([
        text("The "),
        edApplicant,
        text(" is a ")]),
      embed(this.typ)]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The type checker rejected your program because the function application expression")]),
      para([
        code(vSequence((this.appExpr as any).tosource().pretty(80).map((s: string) => text(s))))]),
      para([
        text("at "),
        edLoc((this.appExpr as any)._fun.l),
        text(" expects the applicant to evaluate to a function value. However, the type of the applicant is "),
        embed(this.typ)])]);
  }
}

export class TupleTooSmall extends CompileErrorBase {
  get $name(): 'tuple-too-small' { return 'tuple-too-small'; }
  constructor(public index: number, public tupLength: number, public tup: string, public tupLoc: Loc, public accessLoc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The type checker rejected your program because the tuple type")]),
      highlight(embed(this.tup), [this.tupLoc], 0),
      para([
        text(" has only " + stringify(this.tupLength) + " elements, so the index"),
        code(highlight(text(stringify(this.index)), [this.accessLoc], 1)),
        text(" is too large")])]);
  }
  renderReason(): ED.ErrorDisplay {
    // NOTE: ED.text(self.index) in the source passes a number to text;
    // ported faithfully.
    return error([
      para([
        text("The type checker rejected your program because the tuple type ")]),
      embed(this.tup),
      text(" at "),
      edLoc(this.tupLoc),
      text(" does not have a value at index "),
      code(text(this.index as any)),
      text(" as indicated by the access of at "),
      edLoc(this.accessLoc)]);
  }
}

export class ObjectMissingField extends CompileErrorBase {
  get $name(): 'object-missing-field' { return 'object-missing-field'; }
  constructor(public fieldName: string, public obj: string, public objLoc: Loc, public accessLoc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The type checker rejected your program because the object type")]),
      highlight(text(this.obj), [this.objLoc], 0),
      para([
        text("does not have a field named "),
        code(highlight(text(this.fieldName), [this.accessLoc], 1))])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The type checker rejected your program because the object type ")]),
      embed(this.obj),
      text(" at "),
      edLoc(this.objLoc),
      text(" does not have a field named "),
      code(text(this.fieldName)),
      text(" as indicated by the access of that field at "),
      edLoc(this.accessLoc)]);
  }
}

export class DuplicateVariant extends CompileErrorBase {
  get $name(): 'duplicate-variant' { return 'duplicate-variant'; }
  constructor(public id: string, public found: Loc, public previous: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("This "),
        highlight(text("variant"), [this.found], 0),
        text(" is preceeded by "),
        highlight(text("another variant"), [this.previous], 1),
        text(" of the same name:")]),
      cmcode(this.previous),
      cmcode(this.found),
      para([
        text("A data declaration may not have two variants with the same names.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("A variant may not have the same name as any other variant in the type, but the declaration of a variant "),
        code(text(this.id)),
        text(" at "),
        edLoc(this.found),
        text(" is preceeded by a declaration of a variant also named "),
        code(text(this.id)),
        text(" at "),
        edLoc(this.previous),
        text(".")])]);
  }
}

export class DataVariantDuplicateName extends CompileErrorBase {
  get $name(): 'data-variant-duplicate-name' { return 'data-variant-duplicate-name'; }
  constructor(public id: string, public found: Loc, public dataLoc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("This "),
        highlight(text("variant"), [this.found], 0),
        text(" has the same name as its "),
        highlight(text("containing datatype"), [this.dataLoc], 1), text(".")]),
      cmcode(this.found),
      cmcode(this.dataLoc),
      para([
        text("The "),
        code(text("is-" + this.id)),
        text(" predicates will shadow each other.  Please rename either the variant or the datatype to avoid this.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The variant "),
        code(text(this.id)),
        text(" at "),
        edLoc(this.found),
        text(" has the same name as its containing datatype.  The "),
        code(text("is-" + this.id)),
        text(" predicates will shadow each other.  Please rename either the variant or the datatype to avoid this.")])]);
  }
}

export class DuplicateIsVariant extends CompileErrorBase {
  get $name(): 'duplicate-is-variant' { return 'duplicate-is-variant'; }
  constructor(public id: string, public isFound: Loc, public baseFound: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("This "),
        highlight(text("variant"), [this.baseFound], 0),
        text(" will create a predicate named "), code(text("is-" + this.id)),
        text(", but "),
        highlight(text("another variant"), [this.isFound], 1),
        text(" is defined with that name:")]),
      cmcode(this.baseFound),
      cmcode(this.isFound),
      para([
        text("Please rename one of the variants so their names do not collide.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The variant "),
        code(text(this.id)),
        text(" at "),
        edLoc(this.baseFound),
        text(" will create a predicate named "),
        code(text("is-" + this.id)),
        text(", but another variant is defined with that name.  Please rename one of the variants so their names do not collide.")])]);
  }
}

export class DuplicateIsData extends CompileErrorBase {
  get $name(): 'duplicate-is-data' { return 'duplicate-is-data'; }
  constructor(public id: string, public isFound: Loc, public baseFound: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("This "),
        highlight(text("data definition"), [this.baseFound], 0),
        text(" will create a predicate named "), code(text("is-" + this.id)),
        text(", but "),
        highlight(text("one of its variants"), [this.isFound], 1),
        text(" is defined with that name:")]),
      cmcode(this.baseFound),
      cmcode(this.isFound),
      para([
        text("Please rename either the variant or the data definition so their names do not collide.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The data definition "),
        code(text(this.id)),
        text(" at "),
        edLoc(this.baseFound),
        text(" will create a predicate named "),
        code(text("is-" + this.id)),
        text(", but one of its variants is defined with that name.  Please rename either the variant or the data definition so their names do not collide.")])]);
  }
}

export class DuplicateIsDataVariant extends CompileErrorBase {
  get $name(): 'duplicate-is-data-variant' { return 'duplicate-is-data-variant'; }
  constructor(public id: string, public isFound: Loc, public baseFound: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("This "),
        highlight(text("variant"), [this.baseFound], 0),
        text(" will create a predicate named "), code(text("is-" + this.id)),
        text(", but "),
        highlight(text("the data definition"), [this.isFound], 1),
        text(" already uses that name:")]),
      cmcode(this.baseFound),
      cmcode(this.isFound),
      para([
        text("Please rename either the variant or the data definition so their names do not collide.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The variant "),
        code(text(this.id)),
        text(" at "),
        edLoc(this.baseFound),
        text(" will create a predicate named "),
        code(text("is-" + this.id)),
        text(", but its surrounding data definition already uses that name.  Please rename either the variant or the data definition so their names do not collide.")])]);
  }
}

export class DuplicateBranch extends CompileErrorBase {
  get $name(): 'duplicate-branch' { return 'duplicate-branch'; }
  constructor(public id: string, public found: Loc, public previous: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("This "),
        highlight(text("branch"), [this.found], 0),
        text(" is preceeded by "),
        highlight(text("another branch"), [this.previous], 1),
        text(" that matches the same name: ")]),
      cmcode(this.previous),
      cmcode(this.found),
      para([
        text("A variant may not be matched more than once in a cases expression.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("A variant may not be matched more than once in a cases expression, but the branch matching the variant "),
        code(text(this.id)),
        text(" at "),
        edLoc(this.found),
        text(" is preceeded by a branch also matching "),
        code(text(this.id)),
        text(" at "),
        edLoc(this.previous),
        text(".")])]);
  }
}

export class UnnecessaryBranch extends CompileErrorBase {
  get $name(): 'unnecessary-branch' { return 'unnecessary-branch'; }
  constructor(public branch: A.CasesBranch, public dataType: T.DataType, public casesLoc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The type checker rejected your program because the "),
        highlight(text("cases expression"), [this.casesLoc], 0),
        text(" expects that all of its branches have a variant of the same name in the data-type "),
        code(text(this.dataType.name)),
        text(". However, no variant named "),
        code(highlight(text((this.branch as any).name), [(this.branch as any).patLoc], 1)),
        text(" exists in "),
        code(text(this.dataType.name)),
        text("'s "),
        highlight(text("variants"), this.dataType.variants.map((v: any) => v.l), 2),
        text(":")]),
      bulletedSequence(this.dataType.variants.map((variant: any) =>
          code(highlight(text(variant.name), [variant.l], 2))))]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The type checker rejected your program because the cases expression at "),
        edLoc(this.casesLoc),
        text(" expects that all of its branches have a variant of the same name in the data-type "),
        code(text(this.dataType.name)),
        text(". However, no variant named "),
        code(text((this.branch as any).name)),
        text(" (mentioned in the branch at "),
        edLoc((this.branch as any).patLoc),
        text(")"),
        text(" exists in the type "),
        code(text(this.dataType.name)),
        text("'s variants:")]),
      bulletedSequence(this.dataType.variants.map((v: any) => v.name).map((s: string) => text(s)))]);
  }
}

export class UnnecessaryElseBranch extends CompileErrorBase {
  get $name(): 'unnecessary-else-branch' { return 'unnecessary-else-branch'; }
  constructor(public typeName: string, public loc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The type checker rejected your program because the "),
        highlight(text("cases expression"), [this.loc], 0),
        text(" has a branch for every variant of "),
        code(text(this.typeName)),
        text(". Therefore, the "),
        code(text("else")),
        text(" branch is unreachable.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      paraNospace([
        text("The else branch for the cases expression at "),
        drawAndHighlight(this.loc),
        text(" is not needed since all variants of " + this.typeName + " have been exhausted.")])]);
  }
}

export class NonExhaustivePattern extends CompileErrorBase {
  get $name(): 'non-exhaustive-pattern' { return 'non-exhaustive-pattern'; }
  constructor(public missing: T.TypeVariant[], public typeName: string, public loc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The "),
        highlight(text("cases expression"), [this.loc], 0),
        text(" should be able to handle all possible values of "),
        code(text(this.typeName)),
        text(", but its branches cannot handle "),
        highlight(text(
            this.missing.length > 1 ? "several variants"
            : "a variant"
            ), this.missing.map((m: any) => m.l), 1),
        text(".")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The cases expression at"),
        drawAndHighlight(this.loc),
        text("does not exhaust all variants of " + this.typeName
          + ". It is missing: " + this.missing.map((m: any) => m.name).join(", ") + ".")])]);
  }
}

export class CantMatchOn extends CompileErrorBase {
  get $name(): 'cant-match-on' { return 'cant-match-on'; }
  constructor(public ann: any, public typeName: string, public loc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("A "),
        code(highlight(text("cases expressions"), [this.loc], 0)),
        text(" can only branch on variants of "),
        code(text("data")),
        text(" types. The type "),
        code(highlight(text(this.typeName), [this.ann.l], 1)),
        text(" cannot be used in cases expressions.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The type specified " + this.typeName),
        text("at"),
        drawAndHighlight(this.loc),
        text("cannot be used in a cases expression.")])]);
  }
}

export class DifferentBranchTypes extends CompileErrorBase {
  get $name(): 'different-branch-types' { return 'different-branch-types'; }
  constructor(public l: any, public branchTypes: any) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The branches of this expression evaluate to different types and no common type encompasses all of them:")]),
      bulletedSequence(mapN((n: number, branch: any) =>
          highlight(embed(branch), [branch.l], n),
          0, this.branchTypes))]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The branches of this expression evaluate to different types and no common type encompasses all of them:")]),
      bulletedSequence(mapN((n: number, branch: any) =>
          sequence([
            edLoc(branch.l), text(" has type "), embed(branch)]),
          0, this.branchTypes))]);
  }
}

export class IncorrectNumberOfBindings extends CompileErrorBase {
  get $name(): 'incorrect-number-of-bindings' { return 'incorrect-number-of-bindings'; }
  constructor(public branch: A.CasesBranch, public variant: T.TypeVariant) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    const edFields = (n: number): ED.ErrorDisplay =>
      sequence([
        embed(n),
        text(n === 1 ? " field" : " fields")]);
    return error([
      para([
        text("The type checker expects that the "),
        highlight(text("pattern"), [(this.branch as any).patLoc], 0),
        text(" in the cases branch has the same number of "),
        highlight(text("field bindings"), (this.branch as any).args.map((a: any) => a.l), 1),
        text(" as the data variant "),
        code(highlight(text(this.variant.name), [this.variant.l], 2)),
        text(" has "),
        highlight(text("fields"), [A.dummyLoc], 3),
        text(". However, the branch pattern binds "),
        highlight(edFields((this.branch as any).args.length), (this.branch as any).args.map((a: any) => a.l), 1),
        text(" and the variant is declared as having "),
        highlight(edFields(this.variant.fields.length), [A.dummyLoc], 3)])]);
  }
  renderReason(): ED.ErrorDisplay {
    const edFields = (n: number): ED.ErrorDisplay =>
      sequence([
        embed(n),
        text(n === 1 ? " field" : " fields")]);
    return error([
      para([
        text("The type checker expects that the pattern at "),
        edLoc((this.branch as any).patLoc),
        text(" in the cases branch has the same number of field bindings as the data variant "),
        code(text(this.variant.name)),
        text(" at "),
        edLoc(this.variant.l),
        text(" has fields. However, the branch pattern binds "),
        edFields((this.branch as any).args.length),
        text(" and the variant is declared as having "),
        edFields(this.variant.fields.length)])]);
  }
}

export class CasesSingletonMismatch extends CompileErrorBase {
  get $name(): 'cases-singleton-mismatch' { return 'cases-singleton-mismatch'; }
  constructor(public name: string, public branchLoc: Loc, public shouldBeSingleton: boolean) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    if (this.shouldBeSingleton) {
      return error([
        para([
          text("The type checker rejected your program because the cases branch named "),
          code(highlight(text(this.name), [this.branchLoc], 0)),
          text(" has an argument list, but the variant is a singleton.")])]);
    } else {
      return error([
        para([
          text("The type checker rejected your program because the cases branch named "),
          code(highlight(text(this.name), [this.branchLoc], 0)),
          text(" has an argument list, but the variant is not a singleton.")])]);
    }
  }
  renderReason(): ED.ErrorDisplay {
    if (this.shouldBeSingleton) {
      return error([
        para([
          text("The type checker rejected your program because the cases branch named "),
          code(text(this.name)),
          text(" at "),
          edLoc(this.branchLoc),
          text(" has an argument list, but the variant is a singleton.")])]);
    } else {
      return error([
        para([
          text("The type checker rejected your program because the cases branch named "),
          code(text(this.name)),
          text(" at "),
          edLoc(this.branchLoc),
          text(" has an argument list, but the variant is not a singleton.")])]);
    }
  }
}

export class GivenParameters extends CompileErrorBase {
  get $name(): 'given-parameters' { return 'given-parameters'; }
  // duplicate of `bad-type-instantiation` ?
  constructor(public dataType: string, public loc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return this.renderReason();
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The data type"), code(text(this.dataType)),
        text("does not take any parameters, but is given some at"),
        drawAndHighlight(this.loc)])]);
  }
}

export class UnableToInstantiate extends CompileErrorBase {
  get $name(): 'unable-to-instantiate' { return 'unable-to-instantiate'; }
  constructor(public loc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return this.renderReason();
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("In the type at"), drawAndHighlight(this.loc),
        text("there was not enough information to instantiate the type, "
          + "or the given arguments are incompatible.")])]);
  }
}

export class UnableToInfer extends CompileErrorBase {
  get $name(): 'unable-to-infer' { return 'unable-to-infer'; }
  constructor(public loc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      paraNospace([
        text("Unable to infer the type of "),
        highlight(text("the expression"), [this.loc], 0),
        text(" at "),
        cmcode(this.loc),
        text("Please add an annotation.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      paraNospace([
        text("Unable to infer the type of "), drawAndHighlight(this.loc),
        text(". Please add an annotation.")])]);
  }
}

export class UnannFailedTestInference extends CompileErrorBase {
  get $name(): 'unann-failed-test-inference' { return 'unann-failed-test-inference'; }
  constructor(public functionLoc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The type checker could not infer the type of the "),
        highlight(text("function"), [this.functionLoc], 0),
        text(". Please add type annotations to the arguments.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The type checker could not infer the type of the function at"),
        drawAndHighlight(this.functionLoc),
        text(". Please add type annotations to the arguments.")])]);
  }
}

export class ToplevelUnann extends CompileErrorBase {
  get $name(): 'toplevel-unann' { return 'toplevel-unann'; }
  constructor(public arg: A.Bind) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The "),
        highlight(text("argument"), [(this.arg as any).l], 0),
        text(" at "),
        cmcode((this.arg as any).l),
        text(" needs a type annotation. Alternatively, provide a where: block with examples of the function's use.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The "),
        text("argument at"), drawAndHighlight((this.arg as any).l),
        text(" needs a type annotation. Alternatively, provide a where: block with examples of the function's use.")])]);
  }
}

export class PolymorphicReturnTypeUnann extends CompileErrorBase {
  get $name(): 'polymorphic-return-type-unann' { return 'polymorphic-return-type-unann'; }
  constructor(public functionLoc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The "),
        highlight(text("function"), [this.functionLoc], 0),
        text(" is polymorphic. Please annotate its return type.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The function at "),
        drawAndHighlight(this.functionLoc),
        text(" is polymorphic. Please annotate its return type.")])]);
  }
}

export class BinopTypeError extends CompileErrorBase {
  get $name(): 'binop-type-error' { return 'binop-type-error'; }
  constructor(public binop: A.Expr, public tl: T.Type, public tr: T.Type, public etl: T.Type, public etr: T.Type) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The typechecker thinks there's a problem with the "),
        code(highlight(text((this.binop as any).op), [(this.binop as any).opL], 0)),
        text(" binary operator expression:")]),
      cmcode((this.binop as any).l),
      para([
        text("where the it thinks the "),
        highlight(text("left hand side"), [(this.binop as any).left.l], 1),
        text(" is a "), embed(this.tl),
        text(" and the "),
        highlight(text("right hand side"), [(this.binop as any).right.l], 2),
        text(" is a "), embed(this.tr), text(".")]),
      para([
        text("When the type checker sees a "),
        highlight(embed(this.etl), [(this.binop as any).left.l], 1),
        text("to the left of a "),
        code(highlight(text((this.binop as any).op), [(this.binop as any).opL], 0)),
        text(" it thinks that the "),
        highlight(text("right hand side"), [(this.binop as any).right.l], 2),
        text(" should be a "),
        embed(this.etr)])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The typechecker thinks there's a problem with the "),
        code(highlight(text((this.binop as any).op), [(this.binop as any).opL], 0)),
        text(" binary operator expression at "), edLoc((this.binop as any).opL)]),
      para([
        text("where the it thinks the "),
        highlight(text("left hand side"), [(this.binop as any).left.l], 1),
        text(" is a "), embed(this.tl),
        text(" and the "),
        highlight(text("right hand side"), [(this.binop as any).right.l], 2),
        text(" is a "), embed(this.tr), text(".")]),
      para([
        text("When the type checker sees a "),
        highlight(embed(this.tl), [(this.binop as any).left.l], 1),
        text("to the left of a "),
        code(highlight(text((this.binop as any).op), [(this.binop as any).opL], 0)),
        text(" it thinks that the "),
        highlight(text("right hand side"), [(this.binop as any).right.l], 2),
        text(" should be a "),
        embed(this.etr)])]);
  }
}

export class CantTypecheck extends CompileErrorBase {
  get $name(): 'cant-typecheck' { return 'cant-typecheck'; }
  constructor(public reason: string, public loc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return this.renderReason();
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("This program cannot be type-checked. " + "The reason that it cannot be type-checked is: " + this.reason +
      " at "), cmcode(this.loc)])]);
  }
}

export class Unsupported extends CompileErrorBase {
  get $name(): 'unsupported' { return 'unsupported'; }
  constructor(public message: string, public blameLoc: Loc) { super(); }
  //### TODO ###
  renderFancyReason(): ED.ErrorDisplay {
    return this.renderReason();
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      paraNospace([
        text(this.message + " (found at "),
        drawAndHighlight(this.blameLoc),
        text(")")])]);
  }
}

export class NonObjectProvide extends CompileErrorBase {
  get $name(): 'non-object-provide' { return 'non-object-provide'; }
  constructor(public loc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      paraNospace([
        text("Couldn't read the program because the provide statement must contain an object literal"),
        cmcode(this.loc)])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      paraNospace([
        text("Couldn't read the program because the provide statement must contain an object literal at "),
        drawAndHighlight(this.loc)])]);
  }
}

export class NoModule extends CompileErrorBase {
  get $name(): 'no-module' { return 'no-module'; }
  constructor(public loc: Loc, public modName: string) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      paraNospace([
        text("There is no module imported with the name "),
        highlight(text(this.modName), [this.loc], 0)])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      paraNospace([
        text("There is no module imported with the name " + this.modName),
        text(" (used at "),
        drawAndHighlight(this.loc),
        text(")")])]);
  }
}

export class TableEmptyHeader extends CompileErrorBase {
  get $name(): 'table-empty-header' { return 'table-empty-header'; }
  constructor(public loc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        highlight(text("This table"), [this.loc], 0),
        text(" has no column names, but tables must have at least one column.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The table at "),
        edLoc(this.loc),
        text(" has no column names, but tables must have at least one column.")])]);
  }
}

export class TableEmptyRow extends CompileErrorBase {
  get $name(): 'table-empty-row' { return 'table-empty-row'; }
  constructor(public loc: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        highlight(text("This table row"), [this.loc], 0),
        text(" is empty, but table rows cannot be empty.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The table row at "),
        edLoc(this.loc),
        text(" is empty, but table rows cannot be empty.")])]);
  }
}

export class TableRowWrongSize extends CompileErrorBase {
  get $name(): 'table-row-wrong-size' { return 'table-row-wrong-size'; }
  constructor(public headerLoc: Loc, public header: A.FieldName[], public row: A.TableRow) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    const edCols = (n: number, ls: Loc[], c: number): ED.ErrorDisplay =>
      highlight(sequence([
          embed(n),
          n !== 1 ?
            text("columns")
          :
            text("column")]), ls, c);
    return error([
      para([
        text("The table row")]),
      cmcode((this.row as any).l),
      para([
        text("has "),
        edCols((this.row as any).elems.length, (this.row as any).elems.map((e: any) => e.l), 0),
        text(", but the table header")]),
      cmcode(this.headerLoc),
      para([
        text(" declares "),
        edCols(this.header.length, this.header.map((h: any) => h.l), 1),
        text(".")])]);
  }
  renderReason(): ED.ErrorDisplay {
    const edCols = (n: number): ED.ErrorDisplay =>
      sequence([
        embed(n),
        n !== 1 ?
          text("columns")
        :
          text("column")]);
    return error([
      para([
        text("The table row at "),
        edLoc((this.row as any).l),
        text(" has "),
        edCols((this.row as any).elems.length),
        text(", but the table header "),
        edLoc(this.headerLoc),
        text(" declares "),
        edCols(this.header.length),
        text(".")])]);
  }
}

export class TableDuplicateColumnName extends CompileErrorBase {
  get $name(): 'table-duplicate-column-name' { return 'table-duplicate-column-name'; }
  constructor(public column1: A.FieldName, public column2: A.FieldName) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    return error([
      para([
        text("Column "),
        highlight(text((this.column1 as any).name), [(this.column1 as any).l], 0),
        text(" and column "),
        highlight(text((this.column2 as any).name), [(this.column2 as any).l], 0),
        text(" have the same name, but table columns must have different names.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The table columns at "),
        edLoc((this.column1 as any).l),
        text(" and at "),
        edLoc((this.column2 as any).l),
        text(" have the same name, but columns in a table must have different names.")])]);
  }
}

export class TableReducerBadColumn extends CompileErrorBase {
  get $name(): 'table-reducer-bad-column' { return 'table-reducer-bad-column'; }
  constructor(public extension: A.TableExtendField, public colDefs: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    const badColumn = (this.extension as any).col;
    const badColumnName = badColumn.tosource().pretty(80).join("\n");
    const reducer = (this.extension as any).reducer;
    const reducerName = reducer.tosource().pretty(80).join("\n");
    return error([
      para([
        text("The column "),
        highlight(text(badColumnName), [badColumn.l], 0),
        text(" is used with the reducer "),
        highlight(text(reducerName), [reducer.l], 1),
        text(", but it is not one of the "),
        highlight(text("used columns"), [this.colDefs], 2),
        text(".")])]);
  }
  renderReason(): ED.ErrorDisplay {
    const badColumn = (this.extension as any).col;
    const reducer = (this.extension as any).reducer;
    return error([
      para([
        text("The column at "),
        edLoc(badColumn.l),
        text(" is used with the reducer at "),
        edLoc(reducer.l),
        text(", but it is not one of the used columns listed at "),
        edLoc(this.colDefs),
        text(".")])]);
  }
}

export class TableSanitizerBadColumn extends CompileErrorBase {
  get $name(): 'table-sanitizer-bad-column' { return 'table-sanitizer-bad-column'; }
  constructor(public sanitizeExpr: A.LoadTableSpec, public colDefs: Loc) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    const badColumn = (this.sanitizeExpr as any).name;
    const badColumnName = badColumn.toname();
    const sanitizer = (this.sanitizeExpr as any).sanitizer;
    const sanitizerName = sanitizer.tosource().pretty(80).join(" ");
    return error([
      para([
        text("The column "),
        highlight(text(badColumnName), [badColumn.l], 0),
        text(" is used with the sanitizer "),
        highlight(text(sanitizerName), [sanitizer.l], 1),
        text(", but it is not one of the "),
        highlight(text("used columns"), [this.colDefs], 2),
        text(".")])]);
  }
  renderReason(): ED.ErrorDisplay {
    const badColumn = (this.sanitizeExpr as any).name;
    const sanitizer = (this.sanitizeExpr as any).sanitizer;
    return error([
      para([
        text("The column at "),
        edLoc(badColumn.l),
        text(" is used with the sanitizer at "),
        edLoc(sanitizer.l),
        text(", but it is not one of the used columns listed at "),
        edLoc(this.colDefs),
        text(".")])]);
  }
}

export class LoadTableBadNumberSrcs extends CompileErrorBase {
  get $name(): 'load-table-bad-number-srcs' { return 'load-table-bad-number-srcs'; }
  constructor(public lte: A.Expr, public numFound: number) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    // pretty(80) is a list of lines; join to a single string for `text`.
    const loadTableExpr = (this.lte as any).tosource().pretty(80).join("");
    return error([
      para([
        text("The table loader "),
        highlight(text(loadTableExpr), [(this.lte as any).l], 0),
        text(" specifies "
            + String(this.numFound)
            + " sources, but it should only specify one.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The table loader at "),
        edLoc((this.lte as any).l),
        text(" specifies "
            + String(this.numFound)
            + " sources, but it should only specify one.")])]);
  }
}

export class LoadTableDuplicateSanitizer extends CompileErrorBase {
  get $name(): 'load-table-duplicate-sanitizer' { return 'load-table-duplicate-sanitizer'; }
  constructor(public original: A.LoadTableSpec, public colName: string, public duplicateExp: A.LoadTableSpec) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    // pretty(80) is a list of lines; join to a single string for `text`.
    const origPretty = (this.original as any).tosource().pretty(80).join("");
    return error([
      para([
        text("The column "),
        highlight(text(this.colName), [(this.duplicateExp as any).l], 0),
        text(" is already sanitized by the sanitizer "),
        highlight(text(origPretty), [(this.original as any).l], 1),
        text(".")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The column at "),
        edLoc((this.duplicateExp as any).l),
        text(" is already sanitized by the sanitizer at "),
        edLoc((this.original as any).l),
        text(".")])]);
  }
}

export class LoadTableNoBody extends CompileErrorBase {
  get $name(): 'load-table-no-body' { return 'load-table-no-body'; }
  constructor(public loadTableExp: A.Expr) { super(); }
  renderFancyReason(): ED.ErrorDisplay {
    // pretty(80) is a list of lines; join to a single string for `text`.
    const pretty = (this.loadTableExp as any).tosource().pretty(80).join("");
    return error([
      para([
        text("The table loader "),
        highlight(text(pretty), [(this.loadTableExp as any).l], 0),
        text(" has no information about how to load the table. "
            + "It should at least contain a source.")])]);
  }
  renderReason(): ED.ErrorDisplay {
    return error([
      para([
        text("The table loader at "),
        edLoc((this.loadTableExp as any).l),
        text(" has no information about how to load the table. "
            + "It should at least contain a source.")])]);
  }
}

export type CompileError =
  | WfErr
  | WfEmptyBlock
  | WfErrSplit
  | ReservedName
  | ContractOnImport
  | ContractRedefined
  | ContractNonFunction
  | ContractInconsistentNames
  | ContractInconsistentParams
  | ContractUnused
  | ContractBadLoc
  | ZeroFraction
  | MixedBinops
  | BlockEnding
  | SingleBranchIf
  | UnwelcomeWhere
  | NonExample
  | TupleGetBadIndex
  | ImportArityMismatch
  | NoArguments
  | NonToplevel
  | UnwelcomeTest
  | UnwelcomeTestRefinement
  | UnderscoreAs
  | UnderscoreAsPattern
  | UnderscoreAsExpr
  | UnderscoreAsAnn
  | BlockNeeded
  | NameNotProvided
  | UnboundId
  | UnboundVar
  | UnboundTypeId
  | TypeIdUsedInDotLookup
  | TypeIdUsedAsValue
  | UnexpectedTypeVar
  | PointlessVar
  | PointlessRec
  | PointlessShadow
  | BadAssignment
  | MixedIdVar
  | ShadowId
  | DuplicateId
  | DuplicateField
  | SameLine
  | TemplateSameLine
  | TypeMismatch
  | IncorrectType
  | IncorrectTypeExpression
  | BadTypeInstantiation
  | IncorrectNumberOfArgs
  | MethodMissingSelf
  | ApplyNonFunction
  | TupleTooSmall
  | ObjectMissingField
  | DuplicateVariant
  | DataVariantDuplicateName
  | DuplicateIsVariant
  | DuplicateIsData
  | DuplicateIsDataVariant
  | DuplicateBranch
  | UnnecessaryBranch
  | UnnecessaryElseBranch
  | NonExhaustivePattern
  | CantMatchOn
  | DifferentBranchTypes
  | IncorrectNumberOfBindings
  | CasesSingletonMismatch
  | GivenParameters
  | UnableToInstantiate
  | UnableToInfer
  | UnannFailedTestInference
  | ToplevelUnann
  | PolymorphicReturnTypeUnann
  | BinopTypeError
  | CantTypecheck
  | Unsupported
  | NonObjectProvide
  | NoModule
  | TableEmptyHeader
  | TableEmptyRow
  | TableRowWrongSize
  | TableDuplicateColumnName
  | TableReducerBadColumn
  | TableSanitizerBadColumn
  | LoadTableBadNumberSrcs
  | LoadTableDuplicateSanitizer
  | LoadTableNoBody;

export function isWfErr(x: any): x is WfErr { return x instanceof WfErr; }
export function isWfEmptyBlock(x: any): x is WfEmptyBlock { return x instanceof WfEmptyBlock; }
export function isWfErrSplit(x: any): x is WfErrSplit { return x instanceof WfErrSplit; }
export function isReservedName(x: any): x is ReservedName { return x instanceof ReservedName; }
export function isContractOnImport(x: any): x is ContractOnImport { return x instanceof ContractOnImport; }
export function isContractRedefined(x: any): x is ContractRedefined { return x instanceof ContractRedefined; }
export function isContractNonFunction(x: any): x is ContractNonFunction { return x instanceof ContractNonFunction; }
export function isContractInconsistentNames(x: any): x is ContractInconsistentNames { return x instanceof ContractInconsistentNames; }
export function isContractInconsistentParams(x: any): x is ContractInconsistentParams { return x instanceof ContractInconsistentParams; }
export function isContractUnused(x: any): x is ContractUnused { return x instanceof ContractUnused; }
export function isContractBadLoc(x: any): x is ContractBadLoc { return x instanceof ContractBadLoc; }
export function isZeroFraction(x: any): x is ZeroFraction { return x instanceof ZeroFraction; }
export function isMixedBinops(x: any): x is MixedBinops { return x instanceof MixedBinops; }
export function isBlockEnding(x: any): x is BlockEnding { return x instanceof BlockEnding; }
export function isSingleBranchIf(x: any): x is SingleBranchIf { return x instanceof SingleBranchIf; }
export function isUnwelcomeWhere(x: any): x is UnwelcomeWhere { return x instanceof UnwelcomeWhere; }
export function isNonExample(x: any): x is NonExample { return x instanceof NonExample; }
export function isTupleGetBadIndex(x: any): x is TupleGetBadIndex { return x instanceof TupleGetBadIndex; }
export function isImportArityMismatch(x: any): x is ImportArityMismatch { return x instanceof ImportArityMismatch; }
export function isNoArguments(x: any): x is NoArguments { return x instanceof NoArguments; }
export function isNonToplevel(x: any): x is NonToplevel { return x instanceof NonToplevel; }
export function isUnwelcomeTest(x: any): x is UnwelcomeTest { return x instanceof UnwelcomeTest; }
export function isUnwelcomeTestRefinement(x: any): x is UnwelcomeTestRefinement { return x instanceof UnwelcomeTestRefinement; }
export function isUnderscoreAs(x: any): x is UnderscoreAs { return x instanceof UnderscoreAs; }
export function isUnderscoreAsPattern(x: any): x is UnderscoreAsPattern { return x instanceof UnderscoreAsPattern; }
export function isUnderscoreAsExpr(x: any): x is UnderscoreAsExpr { return x instanceof UnderscoreAsExpr; }
export function isUnderscoreAsAnn(x: any): x is UnderscoreAsAnn { return x instanceof UnderscoreAsAnn; }
export function isBlockNeeded(x: any): x is BlockNeeded { return x instanceof BlockNeeded; }
export function isNameNotProvided(x: any): x is NameNotProvided { return x instanceof NameNotProvided; }
export function isUnboundId(x: any): x is UnboundId { return x instanceof UnboundId; }
export function isUnboundVar(x: any): x is UnboundVar { return x instanceof UnboundVar; }
export function isUnboundTypeId(x: any): x is UnboundTypeId { return x instanceof UnboundTypeId; }
export function isTypeIdUsedInDotLookup(x: any): x is TypeIdUsedInDotLookup { return x instanceof TypeIdUsedInDotLookup; }
export function isTypeIdUsedAsValue(x: any): x is TypeIdUsedAsValue { return x instanceof TypeIdUsedAsValue; }
export function isUnexpectedTypeVar(x: any): x is UnexpectedTypeVar { return x instanceof UnexpectedTypeVar; }
export function isPointlessVar(x: any): x is PointlessVar { return x instanceof PointlessVar; }
export function isPointlessRec(x: any): x is PointlessRec { return x instanceof PointlessRec; }
export function isPointlessShadow(x: any): x is PointlessShadow { return x instanceof PointlessShadow; }
export function isBadAssignment(x: any): x is BadAssignment { return x instanceof BadAssignment; }
export function isMixedIdVar(x: any): x is MixedIdVar { return x instanceof MixedIdVar; }
export function isShadowId(x: any): x is ShadowId { return x instanceof ShadowId; }
export function isDuplicateId(x: any): x is DuplicateId { return x instanceof DuplicateId; }
export function isDuplicateField(x: any): x is DuplicateField { return x instanceof DuplicateField; }
export function isSameLine(x: any): x is SameLine { return x instanceof SameLine; }
export function isTemplateSameLine(x: any): x is TemplateSameLine { return x instanceof TemplateSameLine; }
export function isTypeMismatch(x: any): x is TypeMismatch { return x instanceof TypeMismatch; }
export function isIncorrectType(x: any): x is IncorrectType { return x instanceof IncorrectType; }
export function isIncorrectTypeExpression(x: any): x is IncorrectTypeExpression { return x instanceof IncorrectTypeExpression; }
export function isBadTypeInstantiation(x: any): x is BadTypeInstantiation { return x instanceof BadTypeInstantiation; }
export function isIncorrectNumberOfArgs(x: any): x is IncorrectNumberOfArgs { return x instanceof IncorrectNumberOfArgs; }
export function isMethodMissingSelf(x: any): x is MethodMissingSelf { return x instanceof MethodMissingSelf; }
export function isApplyNonFunction(x: any): x is ApplyNonFunction { return x instanceof ApplyNonFunction; }
export function isTupleTooSmall(x: any): x is TupleTooSmall { return x instanceof TupleTooSmall; }
export function isObjectMissingField(x: any): x is ObjectMissingField { return x instanceof ObjectMissingField; }
export function isDuplicateVariant(x: any): x is DuplicateVariant { return x instanceof DuplicateVariant; }
export function isDataVariantDuplicateName(x: any): x is DataVariantDuplicateName { return x instanceof DataVariantDuplicateName; }
export function isDuplicateIsVariant(x: any): x is DuplicateIsVariant { return x instanceof DuplicateIsVariant; }
export function isDuplicateIsData(x: any): x is DuplicateIsData { return x instanceof DuplicateIsData; }
export function isDuplicateIsDataVariant(x: any): x is DuplicateIsDataVariant { return x instanceof DuplicateIsDataVariant; }
export function isDuplicateBranch(x: any): x is DuplicateBranch { return x instanceof DuplicateBranch; }
export function isUnnecessaryBranch(x: any): x is UnnecessaryBranch { return x instanceof UnnecessaryBranch; }
export function isUnnecessaryElseBranch(x: any): x is UnnecessaryElseBranch { return x instanceof UnnecessaryElseBranch; }
export function isNonExhaustivePattern(x: any): x is NonExhaustivePattern { return x instanceof NonExhaustivePattern; }
export function isCantMatchOn(x: any): x is CantMatchOn { return x instanceof CantMatchOn; }
export function isDifferentBranchTypes(x: any): x is DifferentBranchTypes { return x instanceof DifferentBranchTypes; }
export function isIncorrectNumberOfBindings(x: any): x is IncorrectNumberOfBindings { return x instanceof IncorrectNumberOfBindings; }
export function isCasesSingletonMismatch(x: any): x is CasesSingletonMismatch { return x instanceof CasesSingletonMismatch; }
export function isGivenParameters(x: any): x is GivenParameters { return x instanceof GivenParameters; }
export function isUnableToInstantiate(x: any): x is UnableToInstantiate { return x instanceof UnableToInstantiate; }
export function isUnableToInfer(x: any): x is UnableToInfer { return x instanceof UnableToInfer; }
export function isUnannFailedTestInference(x: any): x is UnannFailedTestInference { return x instanceof UnannFailedTestInference; }
export function isToplevelUnann(x: any): x is ToplevelUnann { return x instanceof ToplevelUnann; }
export function isPolymorphicReturnTypeUnann(x: any): x is PolymorphicReturnTypeUnann { return x instanceof PolymorphicReturnTypeUnann; }
export function isBinopTypeError(x: any): x is BinopTypeError { return x instanceof BinopTypeError; }
export function isCantTypecheck(x: any): x is CantTypecheck { return x instanceof CantTypecheck; }
export function isUnsupported(x: any): x is Unsupported { return x instanceof Unsupported; }
export function isNonObjectProvide(x: any): x is NonObjectProvide { return x instanceof NonObjectProvide; }
export function isNoModule(x: any): x is NoModule { return x instanceof NoModule; }
export function isTableEmptyHeader(x: any): x is TableEmptyHeader { return x instanceof TableEmptyHeader; }
export function isTableEmptyRow(x: any): x is TableEmptyRow { return x instanceof TableEmptyRow; }
export function isTableRowWrongSize(x: any): x is TableRowWrongSize { return x instanceof TableRowWrongSize; }
export function isTableDuplicateColumnName(x: any): x is TableDuplicateColumnName { return x instanceof TableDuplicateColumnName; }
export function isTableReducerBadColumn(x: any): x is TableReducerBadColumn { return x instanceof TableReducerBadColumn; }
export function isTableSanitizerBadColumn(x: any): x is TableSanitizerBadColumn { return x instanceof TableSanitizerBadColumn; }
export function isLoadTableBadNumberSrcs(x: any): x is LoadTableBadNumberSrcs { return x instanceof LoadTableBadNumberSrcs; }
export function isLoadTableDuplicateSanitizer(x: any): x is LoadTableDuplicateSanitizer { return x instanceof LoadTableDuplicateSanitizer; }
export function isLoadTableNoBody(x: any): x is LoadTableNoBody { return x instanceof LoadTableNoBody; }
