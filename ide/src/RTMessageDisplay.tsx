import {
  isSpyValue,
  isSpyMessage,
  RTMessages,
} from './rtMessage';

import { State } from './state';

import { connect, ConnectedProps } from 'react-redux';

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

  const elements = (
    objects.map((rtMessage) => {
    if (isSpyMessage(rtMessage)) {
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

      if (isSpyValue(rtMessage)) {
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
  }));

  return (
    <div
      style={{
        width: '100%',
        height: '100%',
        background: rtMessages.outdated ? outdatedBackground : '#fff',
        fontSize,
        position: 'relative',
      }}
    >
      {elements}
    </div>
  );
}


export default connector(RTMessageDisplay);
