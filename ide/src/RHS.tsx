import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import RenderedValue from './RenderedValue';
import { State } from './state';
import {
  RHSObject,
  RHSObjects,
  isSpyValue,
  isSpyMessage,
  isTrace,
  isLocation,
  isRHSCheck,
  getRow,
} from './rhsObject';
import {
  Chunk,
  findChunkFromSrcloc,
} from './chunk';
import {
  Action,
} from './action';

type StateProps = {
  rhs: RHSObjects,
  fontSize: number,
  chunks: Chunk[],
  currentFile: string,
  focusedChunk: number | undefined,
};

function mapStateToProps(state: State): StateProps {
  const {
    rhs,
    fontSize,
    chunks,
    currentFile,
    focusedChunk,
  } = state;
  return {
    rhs,
    fontSize,
    chunks,
    currentFile,
    focusedChunk,
  };
}

type DispatchProps = {
  setFocusedChunk: (index: number) => void,
};

function mapDispatchToProps(dispatch: (action: Action) => any): DispatchProps {
  return {
    setFocusedChunk(index: number) {
      dispatch({ type: 'update', key: 'focusedChunk', value: index });
    },
  };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type RHSProps = StateProps & PropsFromRedux & DispatchProps;

function RHS({
  rhs,
  fontSize,
  chunks,
  currentFile,
  focusedChunk,
  setFocusedChunk,
}: RHSProps) {
  function compareRHSObjects(a: RHSObject, b: RHSObject): number {
    return getRow(a) - getRow(b);
  }

  const objects = [...rhs.objects, ...rhs.spyData].sort(compareRHSObjects);

  const elements = (
    objects.map((rhsObject) => {
      const row = getRow(rhsObject);
      const chunk = findChunkFromSrcloc(
        chunks,
        [`file://${currentFile}`, row],
        currentFile,
      );
      const isSelected = !rhs.outdated && focusedChunk !== undefined && chunk === focusedChunk;
      const selectedStyle = {
        background: isSelected ? '#d7d4f0' : 'rgba(0, 0, 0, 0)',
        borderTop: isSelected ? '2px solid #c8c8c8' : '2px solid rgba(0, 0, 0, 0)',
        borderBottom: isSelected ? '2px solid #c8c8c8' : '2px solid rgba(0, 0, 0, 0)',
      };

      function selectThisChunk() {
        if (chunk !== false) {
          setFocusedChunk(chunk);
        }
      }

      if (isSpyMessage(rhsObject)) {
        return (
          <pre
            key={rhsObject.key}
            style={{
              paddingLeft: '1em',
              ...selectedStyle,
            }}
          >
            <RenderedValue value={rhsObject.value} />
          </pre>
        );
      }

      if (isSpyValue(rhsObject)) {
        return (
          <pre
            key={rhsObject.key}
            style={{
              paddingLeft: '1em',
              ...selectedStyle,
            }}
          >
            <RenderedValue value={rhsObject.value} />
          </pre>
        );
      }

      if (isTrace(rhsObject)) {
        return (
          <pre
            key={rhsObject.key}
            style={{
              paddingLeft: '1em',
              ...selectedStyle,
            }}
            onMouseEnter={selectThisChunk}
          >
            <RenderedValue value={rhsObject.value} />
          </pre>
        );
      }

      if (isLocation(rhsObject)) {
        return (
          <pre
            key={rhsObject.key}
            style={{
              display: 'flex',
              alignItems: 'center',
              paddingLeft: '1em',
              ...selectedStyle,
            }}
            onMouseEnter={selectThisChunk}
          >
            {rhsObject.name}
            {' '}
            =&nbsp;
            <RenderedValue value={rhsObject.value} />
          </pre>
        );
      }

      if (isRHSCheck(rhsObject)) {
        return (
          <pre
            key={rhsObject.key}
            style={{
              paddingLeft: '1em',
              ...selectedStyle,
            }}
            onMouseEnter={selectThisChunk}
          >
            Test
            {' '}
            {rhsObject.success ? 'succeeded' : 'failed'}
            {' '}
            at
            {' '}
            {rhsObject.loc}
            {rhsObject.success === false && (
              <div style={{
                paddingLeft: '1em',
                display: 'flex',
                flexDirection: 'column',
              }}
              >
                <div style={{
                  display: 'flex',
                  alignItems: 'center',
                }}
                >
                  {'The left side was: '}
                  {rhsObject.lhs.exception === true ? (
                    <RenderedValue value={rhsObject.lhs.exception_val} />
                  ) : (
                    <RenderedValue value={rhsObject.lhs.value} />
                  )}
                </div>
                <div style={{
                  display: 'flex',
                  alignItems: 'center',
                }}
                >
                  {'The right side was: '}
                  {rhsObject.rhs.exception === true ? (
                    <RenderedValue value={rhsObject.rhs.exception_val} />
                  ) : (
                    <RenderedValue value={rhsObject.rhs.value} />
                  )}
                </div>
              </div>
            )}
          </pre>
        );
      }

      throw new Error(`RHS: malformed RHSObject, ${JSON.stringify(rhsObject)}`);
    }));

  const outdatedBackground = 'repeating-linear-gradient(45deg, #c8c8c8, #c8c8c8 4em, #979797 4em, #979797 8em)';

  return (
    <div
      style={{
        width: '100%',
        height: '100%',
        background: rhs.outdated ? outdatedBackground : '#fff',
        fontSize,
        position: 'relative',
      }}
    >
      {rhs.outdated && (
        <div
          style={{
            position: 'sticky',
            display: 'flex',
            background: 'black',
            color: 'white',
            fontSize: '2em',
            justifyContent: 'center',
            top: '0',
          }}
        >
          Press Shift+Enter to re-run
        </div>
      )}
      {elements}
    </div>
  );
}

export default connector(RHS);
