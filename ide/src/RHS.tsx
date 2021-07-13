/* Manages the right hand side (RHS) of the editor. This is used for displaying
   the results of a program that ran. This component will gray itself out when
   the left hand side is updated.

   In chunk mode hovering over one of the values on the RHS will focus its
   corresponding chunk. The currently focused chunk draws a highlight around its
   corresponding RHS value(s).

   This class should not---by itself---render values to components, That should
   instead be done by RenderedValue.tsx. */

import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import { State } from './state';
import {
  RHSObject,
  RHSObjects,
  getRow,
} from './rhsObject';
import {
  Chunk,
  findChunkFromSrcloc,
} from './chunk';
import {
  Action,
} from './action';
import RHSObjectComponent from './RHSObjectComponent';

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

  const objects = rhs.objects.sort(compareRHSObjects);

  const elements = (
    objects.map((rhsObject) => {
      const row = getRow(rhsObject);
      const chunk = findChunkFromSrcloc(
        chunks,
        [`file://${currentFile}`, row],
        currentFile,
      );
      const isSelected = !rhs.outdated && focusedChunk !== undefined && chunk === focusedChunk;
      function selectThisChunk() {
        if (chunk !== false) {
          setFocusedChunk(chunk);
        }
      }
      return (
        <RHSObjectComponent
          isSelected={isSelected}
          rhsObject={rhsObject}
          onMouseEnter={selectThisChunk}
          key={rhsObject.key}
          className="chunks-rhs"
        />
      );
    }));

  /* Known issue: when the RHS is scrollable (because there are a lot of
     values), the gradient does not extend itself on scroll. */
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
