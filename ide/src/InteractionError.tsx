/* In text mode this class is used to display all errors. In chunk mode lint and
   compile errors are handled separately in DefChunk.tsx---this class therefore
   only handles runtime errors in chunk mode. */

import React from 'react';

type InteractionErrorProps = {
  fontSize: number,
  children: React.ReactNode,
};

export default function InteractionError({ fontSize, children }: InteractionErrorProps) {
  return (
    <div className="interaction-error">
      <p style={{
        fontSize,
        padding: 0,
        margin: 0,
      }}
      >
        {children}
      </p>
    </div>
  );
}
