import { CompileState, State } from './state';
import { Action, ActionType, isActionType, ActionOfType } from './action';

// Like State, but doesn't require every single key.
export type PartialState = Partial<State>;

// A semiReducer is like a reducer, except that its return value is meant to represent an
// update to a part of an existing state, not an entirely new state.
// note: When writing a SemiReducer, it's important to (though not required by the Typescript
//       compiler) to annotate its return type as PartialState. If you do not explicitly
//       use this annotation, Typescript will allow your reducer to return any object, not
//       just a partial implementations of State.
//       [ see https://github.com/microsoft/TypeScript/issues/241 ]
export type SemiReducer<K extends ActionType> = (state: State, action: ActionOfType<K>) => PartialState;

// Represents a change that can be applied to an IdeAppState. If necessary, the change can
// use the values from a particular IdeAppState and IdeAction (in the SemiReducer case).
type Change<K extends ActionType> = PartialState | SemiReducer<K>;

function isSemiReducer<K extends ActionType>(change: Change<K>): change is SemiReducer<K> {
  return typeof change === "function";
}

// Represents a change that may be applied to a certain state.
export type StateUpdate<K extends ActionType> = {
  state: CompileState,
  change: Change<K>;
};

// A list of updates that may be applied to a state.
type StateUpdates<K extends ActionType> = Array<StateUpdate<K>>;

function findMatchingChange<K extends ActionType>(
  state: State,
  stateUpdates: StateUpdates<K>): Change<K> | undefined
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
export function applyMatchingStateUpdate<K extends ActionType>(
  name: K,
  state: State,
  action: ActionOfType<K>,
  stateUpdates: StateUpdates<K>): PartialState
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
export function guard<K extends ActionType>(
  actionType: K,
  semiReducer: SemiReducer<K>): SemiReducer<ActionType>
{
  return (state: State, action: Action): PartialState => {
    if (isActionType(actionType, action)) {
      return semiReducer(state, action);
    } else {
      return {};
    }
  };
}

// Creates a SemiReducer that applies the correct state update from stateUpdates, or returns
// {} if the action it receives is not equal to actionType.
// For convenience, stateUpdates can be a thunk to allow for internal definitions. It is
// immediately applied.
export function guardUpdates<K extends ActionType>(
  actionType: K,
  stateUpdates: StateUpdates<K> | (() => StateUpdates<K>)): SemiReducer<ActionType>
{
  if (typeof stateUpdates === "function") {
    const stateUpdatesDethunk = stateUpdates();
    return guard(actionType, (state: State, action: ActionOfType<K>) => {
      if (isActionType(actionType, action)) {
        return applyMatchingStateUpdate(actionType, state, action, stateUpdatesDethunk);
      } else {
        throw new Error();
      }
    });
  } else {
    return guard(actionType, (state: State, action: ActionOfType<K>) => {
      if (isActionType(actionType, action)) {
        return applyMatchingStateUpdate(actionType, state, action, stateUpdates);
      } else {
        throw new Error();
      }
    });
  }
}

// A full Redux reducer (as opposed to a SemiReducer which is defined in this file).
type Reducer = (state: State, action: Action) => State;

// Creates a reducer out of an array of SemiReducers.
export function combineSemiReducers(semiReducers: Array<SemiReducer<ActionType>>): Reducer {
  return (state, action) => {
    return semiReducers.reduce(
      (state, r) => {
        return Object.assign({}, state, r(state, action));
      },
      state);
  }
}
