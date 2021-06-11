import React from 'react';

export default function Tooltip(props: {top: number, left: number}) {
  const { top, left } = props;
  const style = {
    position: 'absolute' as 'absolute', // ???? https://github.com/microsoft/TypeScript/issues/11465#issuecomment-252453037
    top,
    left,
  };
  return <div key="tooltip" style={style}>&nbsp;⏎ Press Enter to run</div>;
}
