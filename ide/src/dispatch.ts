import { CompileState, State } from './state';
import { Action, ActionType } from './action';

// Like ideAppState, but doesn't require every single key.
type PartialIdeAppState = Partial<State>;

// A semiReducer is like a reducer, except that its return value is meant to represent an
// update to an existing state, not an entirely new state.
export type SemiReducer = (state: State, action: Action) => PartialIdeAppState;

// Represents a change that can be applied to an IdeAppState. If necessary, the change can
// use the values from a particular IdeAppState and IdeAction (in the SemiReducer case).
type Change = PartialIdeAppState | SemiReducer;

function isSemiReducer(change: Change): change is SemiReducer {
  return typeof change === "function";
}

// Represents a change that may be applied to a certain state.
type StateUpdate = {
  state: CompileState,
  change: Change;
};

// A list of updates that may be applied to a state.
type StateUpdates = Array<StateUpdate>;

function findMatchingChange(
  state: any,
  stateUpdates: StateUpdates): Change | undefined
{
  const matchingStateUpdate = stateUpdates
    .find(update => update.state === state.compileState);

  if (matchingStateUpdate === undefined) {
    return undefined;
  } else {
    return matchingStateUpdate.change;
  }
}

// Applies the first stateUpdate with a .state field that matches state.compileState to the state and action.
// Throws an error if there is no matching stateUpdate.
export function applyMatchingStateUpdate(
  name: ActionType, // to improve error messages
  state: State,
  action: Action,
  stateUpdates: StateUpdates): PartialIdeAppState
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

// Creates a SemiReducer that returns {} when the action passed to it is not the same as
// actionType, functioning like semiReducer otherwise.
export function guard(
  actionType: ActionType,
  semiReducer: SemiReducer): SemiReducer
{
  return (state: any, action: Action) => {
    switch (action.type) {
      case actionType:
        return semiReducer(state, action);
      default:
        return {};
    }
  };
}

// Creates a SemiReducer that applies the correct state update from stateUpdates, or returns
// {} if the action it receives is not equal to actionType.
// For convenience, stateUpdates can be a thunk to allow for internal definitions. It is
// immediately applied.
export function guardUpdates(
  actionType: ActionType,
  stateUpdates: StateUpdates | (() => StateUpdates)): SemiReducer
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

// A full Redux reducer (as opposed to a SemiReducer which is defined in this file).
type Reducer = (state: State, action: Action) => State;

// Creates a reducer out of an array of SemiReducers.
export function combineSemiReducers(semiReducers: Array<SemiReducer>): Reducer {
  return (state, action) => {
    return semiReducers.reduce(
      (state, r) => {
        return Object.assign({}, state, r(state, action));
      },
      state);
  }
}
