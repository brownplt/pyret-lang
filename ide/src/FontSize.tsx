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
    <div
      style={{
        display: 'flex',
        flexDirection: 'row',
        height: '2.7em',
      }}
    >
      <button
        className="option"
        onClick={() => onDecrease(fontSize)}
        type="button"
        style={{
          width: '2.7em',
        }}
      >
        -
      </button>
      <button
        className="option"
        onClick={onReset}
        type="button"
        style={{
          flexGrow: 2,
        }}
      >
        Font (
        {fontSize}
        {' '}
        px)
      </button>
      <button
        className="option"
        onClick={() => onIncrease(fontSize)}
        type="button"
        style={{
          width: '2.7em',
        }}
      >
        +
      </button>
    </div>
  );
}

export default connector(FontSize);
