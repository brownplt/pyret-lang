import React from 'react';
import { UnControlled as CodeMirror } from 'react-codemirror2';
import CM from 'codemirror';
import { connect } from 'react-redux';
import Tooltip from './Tooltip';
import { BackendCmd, State } from '../state';
import { Action } from '../action';
import { getRow, RHSObjects } from '../rhsObject';
import RVPortal from './RVPortal';

require('pyret-codemirror-mode/mode/pyret');

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

function Embeditor(props: Props) {
  const [_tooltipPos, setTooltipPos] = React.useState<Pos | null>(null);
  const tooltipPosRef = React.useRef(_tooltipPos);
  const [stateEditor, setEditor] = React.useState<(CM.Editor & CM.Doc) | null>(null);
  // https://reactjs.org/docs/hooks-faq.html#is-there-something-like-forceupdate
  const [, forceUpdate] = React.useReducer((x) => x + 1, 0);
  function empty(s: string): boolean {
    return s.replace(/ /g, '') === '';
  }
  function clearTooltip() {
    tooltipPosRef.current = null;
    setTooltipPos(null);
  }
  // NOTE(luna): Let's discuss briefly why we need refs. If you search about
  // this issue, you will read about stale state and how it has to do with
  // closures.  This is not what's happening. JS should give us a fresh closure
  // every time we render, which happens enough for state to be relevant in
  // almost all cases, because state changes trigger re-renders. Instead, the
  // problem has to do with react-codemirror2 (as always). react-codemirror2
  // associates the callbacks (onKeyUp, critically) once during mount, and never
  // again. So recomputed closures are thus lost. This causes what looks like a
  // stale closure, but is not due to capturing variables, but rather a poor
  // react-codemirror2 design decision. The workarounds, however, are
  // essentially the same (except that restructuring the code isn't possible).
  function editorDidMount(editor: CM.Editor & CM.Doc) {
    setEditor(editor);
    forceUpdate();
  }
  // "Methods prefixed with doc. can, unless otherwise specified, be called both
  // on CodeMirror (editor) instances and CodeMirror.Doc instances."
  // This means CM.Editor & CM.Doc should be equal to CM.Editor, but the types
  // given don't fit right
  function onKeyDown(editor: CM.Editor & CM.Doc, event: KeyboardEvent) {
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
    const text = stateEditor.getValue();
    const barriers = Array.from(text.matchAll(/\n\n/g));
    const indices = [0, ...barriers.map(({ index }) => index), 999999];
    const poses = indices.map((index) => (
      stateEditor.posFromIndex(index as number)
    ));
    type Pairs = [CM.Position, CM.Position][];
    const pairsReducer: (acc: [Pairs, CM.Position], next: CM.Position) => [Pairs, CM.Position] = (
      ([pairs, last], next) => [[...pairs, [last, next]], next]
    );
    const pairs = (arr: CM.Position[]) => arr.slice(1).reduce(pairsReducer, [[], arr[0]]);
    const ranges = pairs(poses)[0].map(([from, to]) => ({ from, to }));
    return ranges.map((pos, i) => {
      const { from, to } = pos;
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
  return (
    <>
      <CodeMirror
        onChange={((editor: CM.Editor & CM.Doc, _data, value) => {
          props.save(value);
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
