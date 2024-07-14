import React from 'react';
import { RHSCheck } from '../rhsObject';

type CheckProps = {value: RHSCheck, RenderedValue: React.ReactType};

export default function Check({ value, RenderedValue }: CheckProps) {
  console.log("In <Check>, result: ", value);
  // NOTE(luna): We don't get back the type of test operator used :(
  const sideStyle = {
    display: 'inline-block',
    width: '45%',
    textAlign: 'center' as const,
    padding: '0.5em',
  };
  const success = ('$name' in value && value['$name'] === 'success') || value.success;
  const hasLeftVal = ('left' in value || 'val' in value)
  const hasRightVal = ('right' in value)
  return (
    <div
      style={{
        margin: '0.2em',
        border: `5px solid ${success ? '#27cc78' : '#dc4064'}`,
      }}
    >
      <div style={{ ...sideStyle, textAlign: hasLeftVal ? 'center' : undefined }}>
        {'left' in value ? (
          <RenderedValue value={value.left} />
        ) : ('val' in value ? 
          <RenderedValue value={value.val} />
          :
          <RenderedValue value={JSON.stringify(value, null, 2)} />
        )}
      </div>
      <span style={{ fontSize: '2em' }}>
        {success ? '✓' : '✕'}
      </span>
      <div style={{ ...sideStyle, textAlign: hasRightVal ? 'center' : undefined }}>
        {'right' in value ? (
          <RenderedValue value={value.right} />
        ) : (
          <RenderedValue value={JSON.stringify(value, null, 2)} />
        )}
      </div>
    </div>
  );
}
