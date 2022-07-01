import React from 'react';
import { connect, ConnectedProps } from 'react-redux';

import { State } from './state';

import RenderedValue from './reps/RenderedValueWithOutput';

import {
  isSpyValue,
  isSpyMessage,
  RTMessages,
} from './rtMessages';

type StateProps = {
  rtMessages: RTMessages,
  fontSize: number,
};

function mapStateToProps(state: State): StateProps {
  const {
    rtMessages,
    fontSize,
  } = state;
  return {
    rtMessages,
    fontSize,
  };
}

function mapDispatchToProps(): {} {
  return {};
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type RTProps = StateProps & PropsFromRedux & {};

function RTMessageDisplay({
  rtMessages,
  fontSize,
}: RTProps) {
  const objects = rtMessages.messages;

  // TODO(alex): Focusing on a RT message also focuses the interaction values associated
  //   with the chunk. Is this desired?
  const elements = (
    objects.map((rtMessage) => {
      // TODO(alex): need to take into account rtMessages.outdated
      // TODO(alex): probably want to extract into the overall property 'resultsOutdated'
      // TODO(alex): unify/centralize styles
      const selectedStyle = {
        background: 'rgba(0, 0, 0, 0)',
        borderTop: '2px solid rgba(0, 0, 0, 0)',
        borderBottom: '2px solid rgba(0, 0, 0, 0)',
      };

      // TODO(alex): selected style not applying
      if (isSpyMessage(rtMessage.data)) {
        return (
          <pre
            key={rtMessage.key}
            style={{
              paddingLeft: '1em',
              ...selectedStyle,
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
              ...selectedStyle,
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
