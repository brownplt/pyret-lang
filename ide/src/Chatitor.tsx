/* The container that chunks live in. Handles setting up and reordering chunks.
   Most of the interesting UI considerations about the chunk editor happens in
   DefChunks.tsx, not here. */

import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import CodeMirror from 'codemirror';
import { UnControlled } from 'react-codemirror2';
import { Action } from './action';
import {
  EditorLayout,
  RunningState,
  State,
} from './state';
import {
  Chunk, ChunkResults, isInitializedEditor,
} from './chunk';
import Chat from './Chat';
import { CMEditor, enterShouldSend, isWrapFirst } from './utils';
import ChatResult from './ChatResult';

type StateProps = {
  chunks: Chunk[],
  enterNewline: boolean,
  editorLayout: EditorLayout
  running: RunningState,
  topChunk: Chunk | undefined
};

type DispatchProps = {
  run: () => void,
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
    topChunk
  } = state;

  return {
    chunks,
    enterNewline,
    editorLayout,
    running,
    topChunk
  };
}

function mapDispatchToProps(dispatch: (action: Action) => any): DispatchProps {
  return {
    run() {
      dispatch({ type: 'run' });
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
  editorLayout,
  undo,
  redo,
  insertChunk,
  running,
  topChunk,
}: DefChunksProps) {
  const [mountedEditor, setEditor] = (
    React.useState<(CMEditor) | null>(null)
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

  function setupChunk(chunk: Chunk, index: number) {
    return (
      <Chat
        key={chunk.id}
        index={index}
        focusNewChat={() => mountedEditor?.getInputField().focus()}
      />
    );
  }

  // Sends the prompt chat
  function send(editor: CMEditor) {
    if (editor.getValue() !== '') {
      const value = editor.getValue();
      insertChunk(chunksRef.current.length, value);
      editor.setValue('');
      run();
    }
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
  const prompt = (
    <div className="prompt">
      <UnControlled
        className="new-expr"
        options={{
          mode: 'pyret',
          theme: 'default',
          lineWrapping: true,
          autofocus: true,
          extraKeys: { Tab: 'indentAuto' },
        }}
        editorDidMount={((editor: CMEditor) => {
          editor.setSize(null, 'auto');
          setEditor(editor);
        }) as (editor: CodeMirror.Editor) => void}
        onChange={((editor: CMEditor) => {
          setIsEmpty(editor.getValue() === '');
          setEnterSendRender(enterShouldSend(editor, enterNewlineRef.current));
        }) as any}
        onSelection={((
          editor: CMEditor,
          { ranges }: {ranges: [{head: CodeMirror.Position, anchor: CodeMirror.Position}]},
        ) => {
          setEnterSendRender(
            enterShouldSend(editor, enterNewlineRef.current, undefined, ranges[0].head),
          );
        }) as any}
        onFocus={() => setIsFocused(true)}
        onBlur={() => setIsFocused(false)}
        onKeyDown={((editor: CMEditor, event: KeyboardEvent) => {
          event.stopPropagation();
          switch ((event as any).key) {
            case 'Enter': {
              if (enterShouldSend(editor, enterNewlineRef.current, event)) {
                send(editor);
                event.preventDefault();
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
      <button
        className="text-button send-button"
        type="button"
        onClick={() => {
          if (mountedEditor === null) {
            throw new Error('cannot send before editor mounts');
          }
          const value = mountedEditor.getValue();
          insertChunk(chunksRef.current.length, value);
          mountedEditor.setValue('');
          run();
        }}
      >
        ➤
      </button>
    </div>
  );
  const promptHint = (
    <div style={{
      maxWidth: '48em',
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
  );

  let topChunkPart = <></>;
  if(topChunk) {
    const pendingRerunClass = topChunk.outdated ? 'partially-outdated' : '';
    const isError = topChunk.results.status === 'failed' && isInitializedEditor(topChunk.editor);
    const isErrorClass = isError ? 'chatitor-error' : '';
    topChunkPart = <div className={`chat-and-result ${pendingRerunClass} ${isErrorClass}`}>
      <ChatResult
        editor={topChunk.editor}
        results={topChunk.results}
        id={"topChunk"}
        technicallyOutdated={topChunk.outdated}/>
      </div>;
  }
  return (
    <div className={`${layout} chatitor-container`}>
      <div className="progress-bar" style={{ width: runWidth, height }} />
      <div className="chat-scroll">
        <div className="chats">
          {topChunkPart}
          {allChunks}
          <div style={{ clear: 'both' }} />
        </div>
        <div style={{ marginBottom: '10em' }}>
          { prompt }
          { promptHint }
        </div>
      </div>
    </div>
  );
}

export default connector(Chatitor);
