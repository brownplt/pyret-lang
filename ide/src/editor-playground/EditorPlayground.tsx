import React from 'react';
import { UnControlled as CodeMirror } from 'react-codemirror2';
import CM, { TextMarkerOptions } from 'codemirror';
import { connect } from 'react-redux';
import Tooltip from './Tooltip';
import { BackendCmd, State } from '../state';
import { Action } from '../action';
import { getRow, RHSObjects } from '../rhsObject';
import RVPortal from './RVPortal';
import { CHUNKSEP } from '../chunk';

require('pyret-codemirror-mode/mode/pyret');

const chunkSepNoNewline = CHUNKSEP.replace(/\n/, '');

interface Pos {
  top: number,
  left: number,
}

// "It'll have a line property pointing at the line handle that it is associated
// with." (Once again, CodeMirror types are bad)
interface LineWidget extends CM.LineWidget {
  line: CM.LineHandle
}

interface DispatchProps {
  run: () => void,
  save: (x: string) => void,
}

interface StateProps {
  rhs: RHSObjects,
  text: string,
}

type Props = StateProps & DispatchProps;

function mapStateToProps(state: State): StateProps {
  const {
    rhs,
    currentFileContents,
  } = state;

  return {
    rhs,
    text: currentFileContents ?? '',
  };
}

function mapDispatchToProps(dispatch: (action: Action) => any): DispatchProps {
  return {
    run() {
      return dispatch({ type: 'enqueueEffect', effect: { effectKey: 'initCmd', cmd: BackendCmd.Run } });
    },
    save(contents: string) {
      dispatch({
        type: 'update',
        key: 'currentFileContents',
        value: contents,
      });
    },
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

function addChunksMagicToLines(program: string, lineNumbers: number[]): string {
  const lineTexts = program.split(/\n/);
  lineNumbers.forEach((line) => {
    // Newline included in join
    // Sometimes the program might not have this entry yet, i'm not entirely
    // sure why
    lineTexts[line] = (lineTexts[line] ?? '') + chunkSepNoNewline;
  });
  return lineTexts.join('\n');
}

// marks should be sorted by mark.find().to.line!
function serializeToChunks(program: string, marks: CM.TextMarker[]): string {
  const lines = marks.map((mark) => mark.find().to.line);
  return addChunksMagicToLines(program, lines);
}

function Embeditor(props: Props) {
  const [_tooltipPos, setTooltipPos] = React.useState<Pos | null>(null);
  const tooltipPosRef = React.useRef(_tooltipPos);
  const [stateEditor, setEditor] = React.useState<(CM.Editor & CM.Doc) | null>(null);
  const [_needsFirstMark, setNeedsFirstMark] = React.useState<boolean>(true);
  // https://reactjs.org/docs/hooks-faq.html#is-there-something-like-forceupdate
  const [, forceUpdate] = React.useReducer((x) => x + 1, 0);
  const needsFirstMarkRef = React.useRef(_needsFirstMark);
  function empty(s: string): boolean {
    return s.replace(/ /g, '') === '';
  }
  function clearTooltip() {
    tooltipPosRef.current = null;
    setTooltipPos(null);
  }
  // NOTE(luna): Let's discuss briefly why this accepts an explicit editor, and
  // also why we need refs (eg for needsFirstMarkRef). If you search about this
  // issue, you will read about stale state and how it has to do with closures.
  // This is not what's happening. JS should give us a fresh closure every time
  // we render, which happens enough for state to be relevant in almost all
  // cases, because state changes trigger re-renders. Instead, the problem has
  // to do with react-codemirror2 (as always). react-codemirror2 associates the
  // callbacks (onKeyUp, critically) once during mount, and never again. So
  // recomputed closures are thus lost. This causes what looks like a stale
  // closure, but is not due to capturing variables, but rather a poor
  // react-codemirror2 design decision. The workarounds, however, are
  // essentially the same (except that restructuring the code isn't possible).
  function makeMark(editor: CM.Editor & CM.Doc, left: CM.Position, right: CM.Position) {
    // Mostly for debugging, it's nice (and fun) to have all our marks
    // have different colors!
    const rainbow = ['#fcc', '#fca', '#cff', '#cfc', '#ccf', '#faf', '#fdf'];
    const numMarks = editor.getAllMarks().length ?? 0;
    editor.markText(left, right, {
      css: `background-color: ${rainbow[numMarks % rainbow.length]};`,
      // NOTE(luna): The first segment should be the only inclusiveLeft mark. Why:
      // All segments (and thus marks) own their single leading newline. Not
      // trailing, because a chunk needs to exist after one single newline, so
      // needs a character. This complication is also why we treat the
      // beginning chunk specially, with it being truncated after new marks
      // are added. Of course, the first line doesn't have a leading newline,
      // so it needs to be inclusiveLeft because there's no "above chunk" to
      // defer to.
      inclusiveLeft: numMarks === 0,
      // We own the newline itself and the right side
      inclusiveRight: true,
    });
  }
  function deserializeMarks() {
    if (stateEditor === null) {
      throw new Error('deserializeMarks called before mount!');
    }
    const { text } = props;
    // Chunks serializes chunks as separated by a comment AND a newline,
    // however, we want to render that newline, so we consider the chunk
    // separator to simply by the comment
    const noSeps = text.replaceAll(chunkSepNoNewline, '');
    // If these seps are new to the actual editor instance, deserialize them
    const existMarks = () => (stateEditor.findMarksAt({ line: 0, ch: 0 }).length ?? 0) !== 0;
    if (noSeps !== stateEditor.getValue() && !existMarks()) {
      stateEditor.setValue(noSeps);
      const lines = text.split(/\n/);
      const sepLineOrNegative = lines.map((line, i) => (
        line.match(chunkSepNoNewline) === null ? -1 : i));
      const sepLines = [0, ...sepLineOrNegative.filter((i) => i >= 0), lines.length - 1];
      type Pairs = [number, number][];
      const pairsReducer: (acc: [Pairs, number], next: number) => [Pairs, number] = (
        ([pairs, last], next) => [[...pairs, [last, next]], next]
      );
      const pairs = (arr: number[]) => arr.slice(1).reduce(pairsReducer, [[], arr[0]]);
      pairs(sepLines)[0].forEach(([previousEnd, end]) => {
        const begin = previousEnd === 0 ? { line: 0, ch: 0 } : { line: previousEnd, ch: 99999 };
        makeMark(stateEditor, begin, { line: end, ch: 99999 });
      });
      setNeedsFirstMark(false as boolean);
      needsFirstMarkRef.current = false;
    }
  }
  function editorDidMount(editor: CM.Editor & CM.Doc) {
    setEditor(editor);
    forceUpdate();
  }
  // "Methods prefixed with doc. can, unless otherwise specified, be called both
  // on CodeMirror (editor) instances and CodeMirror.Doc instances."
  // This means CM.Editor & CM.Doc should be equal to CM.Editor, but the types
  // given don't fit right
  function onKeyDown(editor: CM.Editor & CM.Doc, event: KeyboardEvent) {
    if (needsFirstMarkRef.current) {
      makeMark(editor, { line: 0, ch: 0 }, { line: 9999, ch: 9999 });
      setNeedsFirstMark(false as boolean);
      needsFirstMarkRef.current = false;
    }
    clearTooltip();
    if (event.key === 'Enter') {
      const pos = (editor as any).getCursor();
      const token = editor.getTokenAt(pos);
      const lastLine = editor.getLine(pos.line - 1);
      const currentLine = editor.getLine(pos.line);
      if ((pos.line <= 1 && empty(lastLine)) || !empty(currentLine)) {
        // Do nothing. This was not an end-of-line enter, so we don't wanna get in the way!
      // from DefChunk.tsx: handleEnter
      } else if (token.state.lineState.tokens.length !== 0) {
        // My design instinct is to show nothing here: in the happy case, the
        // person is just writing a multiline expression and having a ball
        // However, we might have rules and such around, that should update
        forceUpdate();
      // Due to the above check, lastLine === '' as well in almost all cases
      } else if (empty(lastLine)) {
        // Double enter
        // Lots of edge cases to handle here
        // add chunk marker
        console.assert(pos.ch === 0);
        // We get rid of the extraneous newline, because visually it is occupied
        // by the chunk boundary. This is debatable. Lerner would say not only
        // keep it but rely on it for identifying chunk boundaries
        // --
        // We do this in a scope because previousLine and newLine are about to
        // become critically huge misnomers
        {
          const previousLine = { line: pos.line - 1, ch: 0 };
          const newLine = { line: pos.line, ch: pos.ch };
          editor.replaceRange('', previousLine, newLine);
        }
        // The statuses of the lines have changed from the replaceRange
        const newLine = { line: pos.line - 1, ch: 0 };
        // Update the previous mark that will keep expanding forever if we don't change it
        const oldMarks = editor.findMarksAt(newLine);
        const previousMarks = oldMarks.filter((mark) => mark.find().to.line === newLine.line);
        const endOfPrevious = { line: newLine.line - 1, ch: 9999 };
        const adjustMark = (oldMark: CM.TextMarker, newEnd: CM.Position) => {
          const oldMarkRange = oldMark.find();
          // Make replacement mark. As a workaround, to get all the options
          // back, just pass the old mark in as the options. i assume extraneous
          // fields are ignored, since i haven't seen any errors or bugs from
          // this. (TS claims there's a getOptions method but there isn't one
          // documented in any version or in reality in this version)
          editor.markText(oldMarkRange.from, newEnd, oldMark as TextMarkerOptions);
          // Don't clear before making the replacement mark or bad things happen!
          oldMark.clear();
        };
        if (previousMarks.length === 1) {
          const oldMark = previousMarks[0];
          adjustMark(oldMark, endOfPrevious);
          makeMark(editor, endOfPrevious, newLine);
        } else {
          // The inside of a chunk. What do we actually want to do here? And how
          // do we achieve it with the marks around?
          // For now: we'll split into two chunks, "before-cursor" and
          // "after-cursor" with the cursor at the beginning of "after-cursor"
          // (and "after-cursor" should own the newline like the others
          // presumably?")
          console.assert(previousMarks.length === 0);
          console.assert(oldMarks.length === 1);
          const oldMark = oldMarks[0];
          const oldEnd = oldMark.find().to;
          adjustMark(oldMark, endOfPrevious);
          // But now our new mark will be all the way to oldMarkRange.to this time
          makeMark(editor, endOfPrevious, oldEnd);
        }
        // Run chunks!!
        props.run();
        // While this runs, we should update the <hr /> placeholder, for visual
        // reasons. There's not really a good state for this. Technically, the
        // state that changed is the number of marks. We could put that state,
        // but it'd claim we're tracking more than we really are
        forceUpdate();
      } else {
        // NOTE(luna): "What? This looks weird! Why are you doing pixel stuff!"
        // Well here are some examples of things that DON'T work:
        // - CM.markText: Seems perfect! But doesn't work at all on blank lines fsr!
        // - Modifying the text: Not a good abstraction, hard to keep the cursor
        //   in the right place, have to modify text back
        // - Grabbing the cursor itself and adding css after to it or something:
        //   fsr (React? CM?) it gets overwritten. It's also nigh impossible
        //   to tell cursors apart
        const lastLineBegin = { line: pos.line - 1, ch: 0 };
        const { bottom, left } = editor.cursorCoords(lastLineBegin, 'local');
        // We use the bottom of the previous line because the editor gets
        // confused with lines that don't exist. In the current formulation,
        // this line does exist, but when things get rearranged, sometimes this
        // gets called before the line actually exists
        // ---
        // TODO(luna): Because some parent of this component is relatively
        // positioned, we cannot use 'window' positioning for convenience.
        // Instead, we can position relative to the codemirror! Except
        // codemirror reports relative positions differently than CSS seems to
        // want to with a 'position: relative' container div. So for now i'm
        // just winging an adjustment
        tooltipPosRef.current = { left: left + 30, top: bottom };
        setTooltipPos(tooltipPosRef.current);
      }
    }
  }
  const { rhs } = props;
  const rvs = stateEditor?.operation(() => {
    const marks = stateEditor?.getAllMarks() ?? [];
    return marks.map((marker, i) => {
      const { from, to } = marker.find();
      // is rhs.objects sorted?
      const relevant = rhs.objects.filter((rhsObject) => {
        const line = getRow(rhsObject) - 1;
        return (
          (from.line === 0 && line === 0)
          || (from.line < line && to.line >= line)
        );
      });
      const lineHandle = stateEditor.getLineHandle(Math.max(to.line, 0));
      /* eslint-disable-next-line */
      return <RVPortal rhs={relevant} editor={stateEditor} line={lineHandle} key={i * 13 + to.line} />;
    });
  });
  if (stateEditor !== null) {
    deserializeMarks();
  }
  return (
    <>
      <CodeMirror
        onChange={((editor: CM.Editor & CM.Doc, _data, value) => {
          if (stateEditor === null) {
            return;
          }
          const marks = editor.getAllMarks();
          // Is this necessary? It's not documented what order i get them in, so probably
          marks.sort((a, b) => a.find()?.from.line - b.find()?.from.line);
          const serialized = serializeToChunks(value, marks);
          // Well-behaved
          console.assert(serialized.replaceAll(chunkSepNoNewline, '') === value);
          props.save(serialized);
        }) as (editor: CM.Editor, _data: CM.EditorChange, value: string) => string}
        options={{
          mode: 'pyret',
          lineNumbers: true,
        }}
        // Bad types given by react-codemirror, too bad
        onKeyUp={onKeyDown as (x: CM.Editor, y: Event) => void}
        // onChange={onChange as (x: CM.Editor) => void}
        onMouseDown={clearTooltip}
        editorDidMount={editorDidMount as (x: CM.Editor) => void}
      />
      {tooltipPosRef.current === null
        ? null
        : <Tooltip left={tooltipPosRef.current.left} top={tooltipPosRef.current.top} />}
      {rvs}
    </>
  );
}

export default connector(Embeditor);
