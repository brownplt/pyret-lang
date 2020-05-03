import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { State } from './state';
import { Action } from './action';

type stateProps = {
  fontSize: number,
};

function mapStateToProps(state: State): stateProps {
  const { fontSize } = state;
  return {
    fontSize,
  };
}

type dispatchProps = {
  onIncrease: (oldSize: number) => void,
  onDecrease: (oldSize: number) => void,
  onReset: () => void,
};

function mapDispatchToProps(dispatch: (action: Action) => any): dispatchProps {
  return {
    onIncrease(oldSize: number): void {
      dispatch({ type: 'update', key: 'fontSize', value: oldSize + 1 });
    },
    onDecrease(oldSize: number): void {
      dispatch({ type: 'update', key: 'fontSize', value: oldSize - 1 });
    },
    onReset(): void {
      dispatch({ type: 'update', key: 'fontSize', value: 12 });
    },
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type FontSizeProps = PropsFromRedux & dispatchProps & stateProps;

function FontSize({
  onIncrease, onDecrease, onReset, fontSize,
}: FontSizeProps) {
  return (
    <div className="font-size-options">
      <button
        className="font-minus"
        onClick={() => onDecrease(fontSize)}
        type="button"
      >
        -
      </button>
      <button
        className="font-label"
        onClick={onReset}
        type="button"
      >
        Font (
        {fontSize}
        {' '}
        px)
      </button>
      <button
        className="font-plus"
        onClick={() => onIncrease(fontSize)}
        type="button"
      >
        +
      </button>
    </div>
  );
}

export default connector(FontSize);
