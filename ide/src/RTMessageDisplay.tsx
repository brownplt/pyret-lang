import React from 'react';
import { connect, ConnectedProps } from 'react-redux';

import { Action } from './action';
import { State } from './state';

import { Chunk } from './chunk';
import RenderedValue from './RenderedValue';

import {
  isSpyValue,
  isSpyMessage,
  RTMessages,
} from './rtMessages';

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
}: RTProps) {
  const objects = rtMessages.messages;

  // TODO: chunk focusing
  const elements = (
    objects.map((rtMessage) => {
      if (isSpyMessage(rtMessage.data)) {
        return (
          <pre
            key={rtMessage.key}
            style={{
              paddingLeft: '1em',
            }}
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
            }}
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
