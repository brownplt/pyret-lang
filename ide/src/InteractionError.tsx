import React from 'react';

export type InteractionErrorProps = {
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
