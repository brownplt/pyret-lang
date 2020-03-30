import React from 'react';
import RenderedValue from './RenderedValue';

type InteractionProps = {
  name: string;
  value: any;
};

export default function Interaction({ name, value }: InteractionProps) {
  if (name === '$checks' || name === '$answer') {
    return null;
  }

  return (
    <div className="interaction">
      {name !== '' ? (
        <pre className="interaction-identifier">
          {name}
          {' '}
          =&nbsp;
        </pre>
      ) : null}
      <RenderedValue value={value} />
    </div>
  );
}
