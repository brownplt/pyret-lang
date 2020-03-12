import { CompileState } from './State';
import * as action from './action';

export function dispatchCompileState<a>(name: action.ideActionType,
                                        state: any,
                                        theAction: action.ideAction,
                                        actions: {state: CompileState, action: (state: any, action: action.ideAction) => a | a}[]) {
  const compileState = state.compileState;
  for (let i = 0; i < actions.length; i++) {
    const action = actions[i];
    if (action.state === compileState) {
      console.log(`${name}: dispatching state ${CompileState[compileState]}`);
      if (typeof action.action === "function") {
        return action.action(state, theAction);
      } else {
        return action.action;
      }
    }
  }

  throw new Error(`dispatchCompileState: ${name}: no action for state ${CompileState[compileState]}`);
}

export function on(actionType: action.ideActionType, makeResult: (state: any, action: action.ideAction) => any) {
  return (state: any, theAction: action.ideAction) => {
    switch (theAction.type) {
      case actionType:
        return makeResult(state, theAction);
      default:
        return state;
    }
  };
}

export function onDispatch(actionType: action.ideActionType,
                           results: { state: CompileState, action: any }[] | (() => { state: CompileState, action: any }[])) {
  return on(actionType, (state, action) => {
    if (typeof results === "function") {
      return dispatchCompileState(actionType, state, action, results());
    } else {
      return dispatchCompileState(actionType, state, action, results);
    }
  });
}
