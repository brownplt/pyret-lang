import {
  Action, ActionOfType, ActionType,
} from './action';
import {
  CompileState, EditorMode, makeResult, State, initialState, CHUNKSEP, Chunk,
} from './state';
import {
  applyMatchingStateUpdate,
  guard,
  guardUpdates,
  SemiReducer,
  combineSemiReducers,
  PartialState,
} from './semiReducer';

const semiReducers: Array<SemiReducer<ActionType>> = [
  guardUpdates('beginStartup', [{
    state: CompileState.Uninitialized,
    change: { compileState: CompileState.NeedsStartup },
  }]),
  guardUpdates('startupCompleted', [{
    state: CompileState.NeedsStartup,
    change: { compileState: CompileState.Startup },
  }]),
  guardUpdates('finishSetup', [
    {
      state: CompileState.Startup,
      change: (state): PartialState => {
        if (state.editorMode === EditorMode.Chunks) {
          return { compileState: CompileState.ChunkNeedsRepl };
        }
        return { compileState: CompileState.Ready };
      },
    },
  ]),
  guard('queueRun', (): PartialState => ({ updateQueued: true })),
  guardUpdates('finishCreateRepl', [
    {
      state: CompileState.ChunkNeedsRepl,
      change: { compileState: CompileState.Ready },
    },
  ]),
  guardUpdates('finishRun', [
    {
      state: CompileState.Running,
      change: { compileState: CompileState.Ready },
    },
    {
      state: CompileState.RunningWithStops,
      change: { compileState: CompileState.Ready },
    },
    {
      state: CompileState.RunningWithStopsNeedsStop,
      change: { compileState: CompileState.Ready },
    },
  ]),
  guardUpdates('stop', [
    {
      state: CompileState.RunningWithStops,
      change: { compileState: CompileState.RunningWithStopsNeedsStop },
    },
  ]),
  guardUpdates('compile', [
    {
      state: CompileState.Ready,
      change: { compileState: CompileState.Compile, updateQueued: false },
    },
  ]),
  guardUpdates('compileFailure', [
    {
      state: CompileState.Compile,
      change: (state, action): PartialState => {
        const places: any = [];
        for (let i = 0; i < action.errors.length; i += 1) {
          const matches = action.errors[i].match(/:\d+:\d+-\d+:\d+/g);
          if (matches !== null) {
            matches.forEach((m: any) => {
              places.push(m.match(/\d+/g)!.map(Number));
            });
          }
        }
        return {
          compileState: CompileState.Ready,
          interactionErrors: action.errors,
          definitionsHighlights: places,
        };
      },
    },
  ]),
  guardUpdates('runFailure', (() => {
    function makeRunFailureResult(newState: CompileState) {
      return (state: State, action: ActionOfType<'runFailure'>): PartialState => ({
        compileState: newState,
        interactionErrors: [action.errors.toString()],
      });
    }
    return [
      {
        state: CompileState.Running,
        change: makeRunFailureResult(CompileState.Ready),
      },
      {
        state: CompileState.RunningWithStops,
        change: makeRunFailureResult(CompileState.Ready),
      },
      {
        state: CompileState.RunningWithStopsNeedsStop,
        change: makeRunFailureResult(CompileState.Ready),
      },
      {
        state: CompileState.Compile, // TODO how does this happen?
        change: makeRunFailureResult(CompileState.Compile),
      },
    ];
  })()),
  guard('lintFailure', (): PartialState => {
    console.log('lintFailure not yet implemented');
    return {};
  }),
  guard('lintSuccess', (): PartialState => {
    console.log('lintSucccess not yet implemented');
    return {};
  }),
  guardUpdates('compileSuccess', [
    {
      state: CompileState.Compile,
      change: (state): PartialState => {
        const newCompileState = state.updateQueued
          ? CompileState.Ready : CompileState.NeedsRun;
        return {
          compileState: newCompileState,
          interactionErrors: [],
          definitionsHighlights: [],
        };
      },
    },
  ]),
  guard('runFinished', (state, action): PartialState => {
    function makeData(): PartialState {
      if (action.result !== undefined
          && action.result.result.error === undefined
          && state.currentFile === undefined) {
        throw new Error('state.currentFile should not be undefined');
      } else if (action.result !== undefined
                 && action.result.result.error === undefined) {
        const results = makeResult(action.result.result, `file:// ${state.currentFile}`);

        if (results[0] !== undefined
            && results[0].name === 'error') {
          return {
            interactions: results,
            checks: action.result.result.$checks,
            interactionErrors: action.result.result.error,
          };
        }
        return {
          interactions: results,
          checks: action.result.result.$checks,
        };
      } else if (action.result !== undefined) {
        return {
          interactionErrors: [action.result.result.error],
        };
      } else {
        return {};
      }
    }

    const data = makeData();

    const makeAction = (newState: CompileState) => () => ({ compileState: newState, ...data });

    const readyAction = makeAction(CompileState.Ready);

    return applyMatchingStateUpdate('runFinished', state, action, [
      {
        state: CompileState.RunningWithStops,
        change: readyAction,
      },
      {
        state: CompileState.RunningWithStopsNeedsStop,
        change: readyAction,
      },
      {
        state: CompileState.Running,
        change: readyAction,
      },
    ]);
  }),
  guardUpdates('runStarted', [
    {
      state: CompileState.NeedsRun,
      change: { compileState: CompileState.RunningWithStops },
    },
  ]),
  guard('updateContents', (state, action): PartialState => ({
    currentFileContents: action.contents,
    needLoadFile: false,
    updateQueued: state.autoRun,
  })),
  guard('updateChunkContents', (state, action): PartialState => {
    const TMPchunks = [...state.TMPchunks];
    TMPchunks[action.index] = {
      startLine: TMPchunks[action.index].startLine,
      id: TMPchunks[action.index].id,
      text: action.contents,
    };
    return {
      needLoadFile: false,
      updateQueued: state.autoRun,
      firstUpdatableChunk: action.index,
      TMPchunks,
    };
  }),
  guard('traverseUp', (state, action): PartialState => ({ browsePath: action.path })),
  guard('traverseDown', (state, action): PartialState => ({ browsePath: action.path })),
  guard('expandChild', (state, action): PartialState => ({
    currentFile: action.path,
    needLoadFile: true,
  })),
  guard('setEditorMode', (state, action): PartialState => {
    if (action.mode === EditorMode.Text && state.editorMode === EditorMode.Chunks) {
      if (state.TMPchunks.length === 0) {
        return {
          editorMode: EditorMode.Text,
          currentFileContents: '',
        };
      }
      return {
        editorMode: EditorMode.Text,
        currentFileContents: state.TMPchunks.map((chunk) => chunk.text).join(CHUNKSEP),
      };
    } if (action.mode === EditorMode.Chunks && state.editorMode === EditorMode.Text) {
      if (state.currentFileContents !== undefined) {
        let totalLines = 0;
        const TMPchunks: Chunk[] = [];

        state.currentFileContents.split(CHUNKSEP).forEach((chunkString, i) => {
          TMPchunks.push({
            text: chunkString,
            id: String(i),
            startLine: totalLines,
          });

          totalLines += chunkString.split('\n').length;
        });

        return {
          editorMode: EditorMode.Chunks,
          TMPchunks,
        };
      }

      return {
        editorMode: EditorMode.Chunks,
        TMPchunks: [],
      };
    }
    return {};
  }),
  guard('setChunks', (state, action): PartialState => ({
    TMPchunks: action.chunks,
  })),
];

const rootReducer = combineSemiReducers(semiReducers);

export default function ideApp(state = initialState, action: Action): State {
  return { ...rootReducer(state, action) };
}
