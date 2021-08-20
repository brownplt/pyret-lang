/* The container that chunks live in. Handles setting up and reordering chunks.
   Most of the interesting UI considerations about the chunk editor happens in
   DefChunks.tsx, not here. */

import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import CodeMirror from 'codemirror';
import { UnControlled } from 'react-codemirror2';
import { Action, ChunksUpdate } from './action';
import {
  EditorLayout,
  RunningState,
  State,
} from './state';
import {
  Chunk, emptyChunk, isInitializedEditor,
} from './chunk';
import Chat from './Chat';
import { enterShouldSend, isWrapFirst } from './utils';

type StateProps = {
  chunks: Chunk[],
  enterNewline: boolean,
  editorLayout: EditorLayout
  running: RunningState,
};

type DispatchProps = {
  run: () => void,
  setChunks: (chunks: ChunksUpdate) => void,
  insertChunk: (index: number, text: string) => void,
  undo: () => void,
  redo: () => void,
};

function mapStateToProps(state: State): StateProps {
  const {
    chunks,
    enterNewline,
    editorLayout,
    running,
  } = state;

  return {
    chunks,
    enterNewline,
    editorLayout,
    running,
  };
}

function mapDispatchToProps(dispatch: (action: Action) => any): DispatchProps {
  return {
    run() {
      dispatch({ type: 'run', key: 'runSegments' });
    },
    setChunks(chunks: ChunksUpdate) {
      dispatch({ type: 'update', key: 'chunks', value: chunks });
    },
    undo() {
      dispatch({ type: 'undo' });
    },
    redo() {
      dispatch({ type: 'redo' });
    },
    insertChunk(index: number, text: string) {
      dispatch({
        type: 'chunk', key: 'insert', index, text,
      });
    },
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type DefChunksProps = PropsFromRedux & DispatchProps & StateProps;

function Chatitor({
  run,
  chunks,
  enterNewline,
  setChunks,
  editorLayout,
  undo,
  redo,
  insertChunk,
  running,
}: DefChunksProps) {
  const [mountedEditor, setEditor] = (
    React.useState<(CodeMirror.Editor & CodeMirror.Doc) | null>(null)
  );
  const [enterSendRender, setEnterSendRender] = (
    React.useState<boolean>(false as boolean)
  );
  const [isFocused, setIsFocused] = (
    React.useState<boolean>(false as boolean)
  );
  const [isEmpty, setIsEmpty] = React.useState<boolean>(true as boolean);
  function documentKeys(event: KeyboardEvent) {
    if (event.ctrlKey || event.metaKey) {
      if (event.key.toLowerCase() === 'y' || (event.shiftKey && event.key.toLowerCase() === 'z')) {
        redo();
      } else if (event.key.toLowerCase() === 'z') {
        undo();
      }
    }
  }
  React.useEffect(() => {
    document.addEventListener('keydown', documentKeys);
    return () => {
      document.removeEventListener('keydown', documentKeys);
    };
  });
  // UnControlled continues to have stale closures for no reason, ref is an easy
  // solution
  const chunksRef = React.useRef(chunks);
  chunksRef.current = chunks;
  const enterNewlineRef = React.useRef(enterNewline);
  enterNewlineRef.current = enterNewline;

  // Merge a contract followed by an examples block followed by a function
  // definition. Assume contract and examples are at the end of chunks, and
  // definition has not yet been added to the chunks (passed in a string)
  // Returns true if a merge occured and the chat shouldn't be sent normally,
  // false otherwise
  function mergeDesignRecipe(definition: string): boolean {
    if (chunksRef.current.length < 2) {
      return false;
    }
    const contract = chunksRef.current[chunksRef.current.length - 2];
    const examples = chunksRef.current[chunksRef.current.length - 1];
    const split = (s: string) => s.trim().split(/[ \n]+/);
    const tokens = (chunk: Chunk) => split(chunk.editor.getValue());
    // An extremely awful way to parse Pyret syntax
    const isDesignRecipe = tokens(contract)[1] === '::' && tokens(examples)[0] === 'examples:' && split(definition)[0] === 'fun';
    if (isDesignRecipe) {
      const newChunks = [
        ...chunksRef.current.slice(0, -2),
        emptyChunk({ editor: { getValue: () => `${contract.editor.getValue()}\n${examples.editor.getValue()}\n${definition}` } }),
      ];
      setChunks({ chunks: newChunks, modifiesText: true });
    }
    return isDesignRecipe;
  }

  function setupChunk(chunk: Chunk, index: number) {
    return (
      <Chat
        key={chunk.id}
        index={index}
        focusNewChat={() => mountedEditor?.getInputField().focus()}
      />
    );
  }

  const allChunks = chunks.map(setupChunk);

  const tooltipStyle = { margin: '0 0.5em' };
  const shiftEnterStyle = { ...tooltipStyle, color: enterNewlineRef.current ? 'grey' : 'black' };
  const layout = editorLayout === EditorLayout.Compact ? 'chat-layout-compact' : 'chat-layout-normal';
  // Why plus one? Surely you want zero on zero and 1 on done? No, because of
  // the transition, we want the bar to move *towards* the state it's next going
  // to be in to look good
  const runWidth = running.type === 'segments' ? `${(100 * (running.done + 1)) / running.total}%` : '0%';
  const height = running.type === 'segments' ? '0.8em' : '0';
  return (
    <div className={`${layout} chatitor-container`}>
      <div className="progress-bar" style={{ width: runWidth, height }} />
      <div className="chat-scroll">
        <div className="chats">
          {allChunks}
          <div style={{ clear: 'both' }} />
        </div>
      </div>
      <UnControlled
        className="new-expr"
        options={{
          mode: 'pyret',
          theme: 'default',
          lineWrapping: true,
          autofocus: true,
          extraKeys: { Tab: 'indentAuto' },
        }}
        editorDidMount={((editor: CodeMirror.Editor & CodeMirror.Doc) => {
          editor.setSize(null, 'auto');
          setEditor(editor);
        }) as (editor: CodeMirror.Editor) => void}
        onChange={((editor: CodeMirror.Editor & CodeMirror.Doc) => {
          setIsEmpty(editor.getValue() === '');
          setEnterSendRender(enterShouldSend(editor, enterNewlineRef.current));
        }) as any}
        onSelection={((
          editor: CodeMirror.Editor & CodeMirror.Doc,
          { ranges }: {ranges: [{head: CodeMirror.Position, anchor: CodeMirror.Position}]},
        ) => {
          setEnterSendRender(
            enterShouldSend(editor, enterNewlineRef.current, undefined, ranges[0].head),
          );
        }) as any}
        onFocus={() => setIsFocused(true)}
        onBlur={() => setIsFocused(false)}
        onKeyDown={((editor: CodeMirror.Editor & CodeMirror.Doc, event: KeyboardEvent) => {
          event.stopPropagation();
          switch ((event as any).key) {
            case 'Enter': {
              if (enterShouldSend(editor, enterNewlineRef.current, event)) {
                if (editor.getValue() !== '') {
                  const value = editor.getValue();
                  if (!mergeDesignRecipe(value)) {
                    insertChunk(chunksRef.current.length, value);
                  }
                  editor.setValue('');
                  run();
                  event.preventDefault();
                } else {
                  event.preventDefault();
                }
              }
              break;
            }
            case 'ArrowUp': {
              const pos = editor.getCursor();
              if (pos.line === 0 && isWrapFirst(editor, pos)) {
                const lastEditor = chunksRef.current[chunksRef.current.length - 1].editor;
                if (isInitializedEditor(lastEditor)) {
                  lastEditor.getInputField().focus();
                }
                event.preventDefault();
              }
              break;
            }
            case 'Escape':
              editor.getInputField().blur();
              break;
            default:
          }
        }) as any}
        autoCursor
      />
      <div style={{
        width: '48em',
        textAlign: 'right',
        margin: '0.3em auto',
        transition: isFocused ? 'opacity 0.2s 1s ease-in' : 'opacity 0.2s ease-in',
        opacity: isFocused && !isEmpty ? '60%' : '0%',
      }}
      >
        <span style={shiftEnterStyle}>Shift-Enter: new line</span>
        <span style={tooltipStyle}>Ctrl-Enter: send</span>
        <span style={{
          ...tooltipStyle, display: 'inline-block', textAlign: 'left', width: '8.2em',
        }}
        >
          Enter:
          {' '}
          {enterSendRender ? 'send' : 'new line'}
        </span>
      </div>
    </div>
  );
}

export default connector(Chatitor);
