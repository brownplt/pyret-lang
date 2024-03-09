import React from 'react';
import { RHSCheck } from '../rhsObject';

type CheckProps = {value: RHSCheck, RenderedValue: React.ReactType};

export default function Check({ value, RenderedValue }: CheckProps) {
  console.log("Check result: ", value);
  // NOTE(luna): We don't get back the type of test operator used :(
  const sideStyle = {
    display: 'inline-block',
    width: '45%',
    textAlign: 'center' as 'center',
    padding: '0.5em',
  };
  return (
    <div
      style={{
        margin: '0.2em',
        border: `5px solid ${value.success ? '#27cc78' : '#dc4064'}`,
      }}
    >
      <div style={{ ...sideStyle }}>
        {value.lhs.exception === true ? (
          <RenderedValue value={value.lhs.exception_val} />
        ) : (
          <RenderedValue value={value.lhs.value} />
        )}
      </div>
      <span style={{ fontSize: '2em' }}>
        {value.success ? '✓' : '✕'}
      </span>
      <div style={sideStyle}>
        {value.rhs.exception === true ? (
          <RenderedValue value={value.rhs.exception_val} />
        ) : (
          <RenderedValue value={value.rhs.value} />
        )}
      </div>
    </div>
  );
}
