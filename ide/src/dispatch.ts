import { CompileState } from './State';
import * as A from './action';

// A semiReducer is like a reducer, except that its return value is meant to represent an
// update to an existing state, not an entirely new state.
type semiReducer<a> = (state: any, action: A.ideAction) => a;

type change<a> = a | semiReducer<a>;

function isSemiReducer<a>(change: change<a>): change is semiReducer<a> {
  return typeof change === "function";
}

// A stateUpdate<a> represents a change that may be applied to a certain state.
type stateUpdate<a> = {
  state: CompileState,
  change: change<a>;
};

// A list of updates that may be applied to a state.
type stateUpdates<a> = Array<stateUpdate<a>>;

function findMatchingChange<a>(
  state: any,
  stateUpdates: stateUpdates<a>): change<a> | undefined
{
  const matchingStateUpdate = stateUpdates
    .find(update => update.state === state.compileState);

  if (matchingStateUpdate === undefined) {
    return undefined;
  } else {
    return matchingStateUpdate.change;
  }
}

export function applyMatchingStateUpdate<a>(
  name: A.ideActionType, // to improve error messages
  state: any,
  action: A.ideAction,
  stateUpdates: stateUpdates<a>): a
{
  const matchingChange = findMatchingChange(state, stateUpdates);

  if (matchingChange === undefined) {
    throw new Error(`${name}: no state update for state ${CompileState[state.compileState]}`);
  } else {
    if (isSemiReducer(matchingChange)) {
      return matchingChange(state, action);
    } else {
      return matchingChange;
    }
  }
}

// Creates a semiReducer that returns {} when the action passed to it is not the same as
// actionType, functioning like semiReducer otherwise.
export function guard<a>(
  actionType: A.ideActionType,
  semiReducer: semiReducer<a>): semiReducer<a | {}>
{
  return (state: any, action: A.ideAction) => {
    switch (action.type) {
      case actionType:
        return semiReducer(state, action);
      default:
        return {};
    }
  };
}

// Creates a semiReducer that applies the correct state update from stateUpdates, or returns
// {} if the action it receives is not equal to actionType.
// For convenience, stateUpdates can be a thunk to allow for internal definitions. It is
// immediately applied.
export function guardUpdates<a>(
  actionType: A.ideActionType,
  stateUpdates: stateUpdates<a> | (() => stateUpdates<a>)): semiReducer<a | {}>
{
  if (typeof stateUpdates === "function") {
    const stateUpdatesDethunk = stateUpdates();
    return guard(actionType, (state, action) => {
      return applyMatchingStateUpdate(actionType, state, action, stateUpdatesDethunk);
    });
  } else {
    return guard(actionType, (state, action) => {
      return applyMatchingStateUpdate(actionType, state, action, stateUpdates);
    });
  }
}
