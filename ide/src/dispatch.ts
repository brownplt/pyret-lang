import { CompileState } from './State';
import * as action from './action';

export function dispatchCompileState<a>(name: string,
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

export function on(actionType: string, makeResult: (state: any, action: action.ideAction) => any) {
  return (state: any, theAction: any) => {
    switch (theAction.type) {
      case actionType:
        return makeResult(state, theAction);
      default:
        return state;
    }
  };
}

export function onDispatch(actionType: string, results: { state: CompileState, action: any }[]) {
  return on(actionType, (state, action) => {
    return dispatchCompileState(actionType, state, action, results);
  });
}
