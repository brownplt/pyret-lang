import { TextDocument } from 'vscode-languageserver-textdocument';
import { parse, State } from './pyret-mode';
import { unpackModule } from './util';
import * as lsp from '../../external/lsp';
import { SrcLoc, Token } from './external-types';

export function splitMultilineToken(tok: Token): Token[] {
  if (tok.pos.startRow !== tok.pos.endRow) {
    return tok.value.split("\n").map((val, i, arr) => {
      let name = tok.name + (i == 0 ? "-START" : i == arr.length - 1 ? "-END" : "");
      let asString = "'" + name;
      let startChar = tok.value.indexOf(val) + tok.pos.startChar;
      let startRow = tok.pos.startRow + i;
      let pos: SrcLoc = {
        startChar: startChar,
        startCol: i == 0 ? tok.pos.startCol : 0,
        startRow: startRow,
        endChar: startChar + val.length,
        endCol: i == 0 ? tok.pos.startCol + val.length : val.length,
        endRow: startRow
      };
      return {
        name: name,
        value: val,
        key: asString + ":" + val,
        asString: asString,
        pos: pos
      };
    });
  } else {
    return [tok];
  }
}

export function* lineTokenizerFrom(tokenizer: any): Generator<[Token[], number], any, unknown> {
  let multilineTok: Token;
  let line: Token[] = [];
  let lineNum = 0;
  while (tokenizer.hasNext()) {
    multilineTok = tokenizer.next();
    for (let singleTok of splitMultilineToken(multilineTok)) {
      while (lineNum < singleTok.pos.startRow - 1) {
        yield [line, lineNum];
        line = [];
        lineNum++;
      }
      if (multilineTok.name !== "EOF") {
        line.push(singleTok);
      }
    }
  }
  yield [line, lineNum];
}


export class FormatCache {
  static readonly CACHE_INTERVAL = 20;
  states: State[] = [State.startState()];
  document: TextDocument;

  constructor(doc: TextDocument) {
    this.document = doc;
  }

  invalidateLinesAfter(line: number) {
    this.states.splice(Math.floor(line / FormatCache.CACHE_INTERVAL) + 1);
  }

  async forEachLine(start: number, end: number, proc: (state: State, lineNum: number) => void) {
    const runtime = await (lsp.result) as any;
    const tokLib: any = unpackModule(runtime.modules["jsfile://pyret-lang/src/arr/compiler/pyret-tokenizer.js"]);
    const tokenizer = tokLib.CommentTokenizer;

    const startIdx = Math.min(this.states.length-1, Math.floor(start / FormatCache.CACHE_INTERVAL));
    const state = this.states[startIdx].copy();
    const startLine = startIdx * FormatCache.CACHE_INTERVAL;

    const doc = this.document.getText({ start: { line: startLine, character: 0 }, end: { line: end + 1, character: 0 } }) ?? "";
    tokenizer.tokenizeFrom(doc);
    let lineTokenizer = lineTokenizerFrom(tokenizer);

    for (let [tokLine, lineNum] of lineTokenizer) {
      const curLine = lineNum + startLine;
      // cache state if needed
      if (curLine % FormatCache.CACHE_INTERVAL === 0 && curLine / FormatCache.CACHE_INTERVAL >= this.states.length) {
        this.states.push(state.copy());
      }

      // process the line of tokens
      if (tokLine.length === 0) {
        state.lineState.nestingsAtLineStart = state.lineState.nestingsAtLineEnd.copy();
      } else {
        tokLine.forEach((tok, i, arr) => {
          parse(state, arr[i - 1], tok, arr[i + 1]);
        });
      }
      
      // if in range, call procedure
      if (curLine >= start && curLine <= end) {
        proc(state, curLine);
      }
    }
  }
}