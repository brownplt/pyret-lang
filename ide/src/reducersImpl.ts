import * as action from './action';
import { CompileState, makeResult } from './State';
import { dispatchCompileState, on, onDispatch } from './dispatch';
import * as control from './control';

// IMPORTANT: only Redux reducers should be exported from this file.

export const beginStartup = onDispatch("beginStartup", [{
  state: CompileState.Uninitialized,
  action: { compileState: CompileState.TextNeedsStartup }
}]);

export const startupCompleted = onDispatch("startupCompleted", [{
  state: CompileState.TextNeedsStartup,
  action: { compileState: CompileState.TextStartup }
}]);

export const finishSetup = onDispatch("finishSetup", [
  {
    state: CompileState.TextStartup,
    action: { compileState: CompileState.TextReady }
  },
  {
    state: CompileState.TextStartupQueue,
    action: { compileState: CompileState.TextReadyQueue }
  },
  {
    state: CompileState.ChunkStartup,
    action: { compileState: CompileState.ChunkNeedsRepl }
  },
  {
    state: CompileState.ChunkStartupQueue,
    action: { compileState: CompileState.ChunkNeedsReplQueue }
  }
]);

export const queueRun = onDispatch("queueRun", [
  {
    state: CompileState.TextStartup,
    action: { compileState: CompileState.TextStartupQueue }
  },
  {
    state: CompileState.TextStartupQueue,
    action: { compileState: CompileState.TextStartupQueue }
  },
  {
    state: CompileState.ChunkStartup,
    action: { compileState: CompileState.ChunkStartupQueue }
  },
  {
    state: CompileState.ChunkNeedsRepl,
    action: { compileState: CompileState.ChunkNeedsReplQueue }
  },
  {
    state: CompileState.ChunkStartupQueue,
    action: { compileState: CompileState.ChunkStartupQueue }
  },
  {
    state: CompileState.ChunkNeedsReplQueue,
    action: { compileState: CompileState.ChunkNeedsReplQueue }
  }
]);

export const finishCreateRepl = onDispatch("finishCreateRepl", [
  {
    state: CompileState.ChunkNeedsRepl,
    action: { compileState: CompileState.ChunkReady }
  },
  {
    state: CompileState.ChunkNeedsReplQueue,
    action: { compileState: CompileState.ChunkReadyQueue }
  }
]);

export const runText = onDispatch("runText", [
  {
    state: CompileState.TextReady,
    action: { compileState: CompileState.TextReadyQueue }
  },
  {
    state: CompileState.TextReadyQueue,
    action: { compileState: CompileState.TextReadyQueue }
  }
]);


export const finishRunText = onDispatch("finishRunText", [
  {
    state: CompileState.TextRunning,
    action: { compileState: CompileState.TextReady }
  },
  {
    state: CompileState.TextRunningWithStops,
    action: { compileState: CompileState.TextReady }
  },
  {
    state: CompileState.TextRunningQueue,
    action: { compileState: CompileState.TextReadyQueue }
  },
  {
    state: CompileState.TextRunningWithStopsQueue,
    action: { compileState: CompileState.TextReadyQueue }
  },
  {
    state: CompileState.TextRunningWithStopsNeedsStop,
    action: { compileState: CompileState.TextReady }
  }
]);

export const stopText = onDispatch("stopText", [
  {
    state: CompileState.TextRunningWithStops,
    action: { compileState: CompileState.TextRunningWithStopsNeedsStop }
  }
]);

export const textCompileQueue = onDispatch("textCompileQueue", [
  {
    state: CompileState.TextReadyQueue,
    action: { compileState: CompileState.TextCompileQueue }
  }
]);

export const textCompileFailure = onDispatch("textCompileFailure", [
  {
    state: CompileState.TextCompileQueue,
    action: (state: any, action: action.textCompileFailure) => {
      const places: any = [];
      for (let i = 0; i < action.errors.length; i++) {
        const matches = action.errors[i].match(/:\d+:\d+-\d+:\d+/g);
        if (matches !== null) {
          matches.forEach((m) => {
            places.push(m.match(/\d+/g)!.map(Number));
          });
        }
      }
      return {
        compileState: CompileState.TextReady,
        interactionErrors: action.errors,
        definitionsHighlights: places
      };
    }
  }
]);

export const textRunFailure = onDispatch("textRunFailure", (() => {
  function makeResult(newState: CompileState) {
    return (state: any, action: action.ideAction) => ({
      compileState: newState,
      interactionErrors: [(action as action.textRunFailure).errors.toString()]
    })
  }
  return [
    {
      state: CompileState.TextRunning,
      action: makeResult(CompileState.TextReady)
    },
    {
      state: CompileState.TextRunningWithStops,
      action: makeResult(CompileState.TextReady)
    },
    {
      state: CompileState.TextRunningQueue,
      action: makeResult(CompileState.TextReadyQueue)
    },
    {
      state: CompileState.TextRunningWithStopsQueue,
      action: makeResult(CompileState.TextReadyQueue)
    },
    {
      state: CompileState.TextRunningWithStopsNeedsStop,
      action: makeResult(CompileState.TextReady)
    }
  ];
})());

export const textLintFailure = on("textLintFailure", () => {
  console.log("textLintFailure not yet implemented");
  return {};
});

export const textLintSuccess = on("textLintSuccess", () => {
  console.log("textLintSucccess not yet implemented");
  return {};
});

export const textCompileSuccess = onDispatch("textCompileSuccess", [
  {
    state: CompileState.TextCompileQueue,
    action: { compileState: CompileState.TextNeedsRun }
  }
]);

export const textRunFinished = on("textRunFinished", (state: any, action: any) => {
  const data = (() => {
    if (action.result !== undefined) {
      if (action.result.result.error === undefined) {
        const results =
          makeResult(
            action.result.result,
            "file:// " +
              control.bfsSetup.path.join(
                control.path.compileBase,
                state.currentFileName));

        console.log(results);

        if (results[0] !== undefined && results[0].name === "error") {
          return {
            interactions: results,
            checks: action.result.result.$checks,
            interactionErrors: action.result.result.error
          };
        }

        return {
          interactions: results,
          checks: action.result.result.$checks,
        };
      }

      return {
        interactionErrors: [action.result.result.error]
      };
    }

    return {};
  })();

  console.log("data", data);

  const makeAction = (newState: CompileState) => () => {
    return Object.assign({}, {compileState: newState}, data);
  }

  return dispatchCompileState(state, action, [
    {
      state: CompileState.TextRunningWithStops,
      action: makeAction(CompileState.TextReady)
    },
    {
      state: CompileState.TextRunningWithStopsQueue,
      action: makeAction(CompileState.TextReadyQueue)
    },
    {
      state: CompileState.TextRunningWithStopsNeedsStop,
      action: makeAction(CompileState.TextReady)
    },
    {
      state: CompileState.TextRunning,
      action: makeAction(CompileState.TextReady)
    },
    {
      state: CompileState.TextRunningQueue,
      action: makeAction(CompileState.TextReadyQueue)
    }
  ]);
});

export const textRunStarted = onDispatch("textRunStarted", [
  {
    state: CompileState.TextNeedsRun,
    action: { compileState: CompileState.TextRunningWithStops }
  }
]);

export const textUpdateContents = on("textUpdateContents", (state: any, action: any) => {
  return {
    currentFileContents: action.contents,
    needLoadFile: false
  };
});

export const traverseUp = on("traverseUp", (state: any, action: any) => {
  return { browsePath: action.path };
});

export const traverseDown = on("traverseDown", (state: any, action: any) => {
  return { browsePath: action.path };
});

export const expandChild = on("expandChild", (state: any, action: any) => {
  return {
    currentFileDirectory: state.browsePath,
    currentFileName: action.child,
    needLoadFile: true
  };
});
