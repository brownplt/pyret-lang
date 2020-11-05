import React from 'react';
import { connect, ConnectedProps } from 'react-redux';

import { Action } from './action';
import { State } from './state';

import { Chunk, findChunkFromSrcloc } from './chunk';
import RenderedValue from './RenderedValue';

import {
  isSpyValue,
  isSpyMessage,
  RTMessages,
} from './rtMessages';

import { getRow } from './rhsObject';

type StateProps = {
  rtMessages: RTMessages,
  fontSize: number,
  chunks: Chunk[],
  currentFile: string,
  focusedChunk: number | undefined,
};

function mapStateToProps(state: State): StateProps {
  const {
    rtMessages,
    fontSize,
    chunks,
    currentFile,
    focusedChunk,
  } = state;
  return {
    rtMessages,
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
type RTProps = StateProps & PropsFromRedux & DispatchProps;

function RTMessageDisplay({
  rtMessages,
  fontSize,
  chunks,
  currentFile,
  focusedChunk,
  setFocusedChunk,
}: RTProps) {
  const objects = rtMessages.messages;

  // TODO(alex): Focusing on a RT message also focuses the interaction values associated
  //   with the chunk. Is this desired?
  const elements = (
    objects.map((rtMessage) => {
      const row = getRow(rtMessage.data);
      const chunk = findChunkFromSrcloc(
        chunks,
        [`file://${currentFile}`, row],
        currentFile,
      );
      // TODO(alex): need to take into account rtMessages.outdated
      // TODO(alex): probably want to extract into the overall property 'resultsOutdated'
      const isSelected = focusedChunk !== undefined && chunk === focusedChunk;
      // TODO(alex): unify/centralize styles
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

      // TODO(alex): selected style not applying
      if (isSpyMessage(rtMessage.data)) {
        return (
          <pre
            key={rtMessage.key}
            style={{
              paddingLeft: '1em',
              ...selectedStyle,
            }}
            onMouseEnter={selectThisChunk}
          >
            <RenderedValue value={rtMessage} />
          </pre>
        );
      }

      if (isSpyValue(rtMessage.data)) {
        return (
          <pre
            key={rtMessage.key}
            style={{
              paddingLeft: '1em',
              ...selectedStyle,
            }}
            onMouseEnter={selectThisChunk}
          >
            <RenderedValue value={rtMessage} />
          </pre>
        );
      }
      throw new Error(`RTMessage: malformed runtime message, ${JSON.stringify(rtMessage)}`);
    })
  );

  return (
    <div
      style={{
        width: '100%',
        height: '100%',
        background: '#fff',
        fontSize,
        position: 'relative',
      }}
    >
      {elements}
    </div>
  );
}

export default connector(RTMessageDisplay);
