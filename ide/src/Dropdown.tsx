// TODO (michael): improve accessibilty by enabling these rules
/* eslint-disable jsx-a11y/click-events-have-key-events */
/* eslint-disable jsx-a11y/no-static-element-interactions */

import React from 'react';

export default function Dropdown({ children }: { children: any }) {
  return (
    <div
      className="run-dropdown"
      onClick={(e) => e.stopPropagation()}
    >
      {children}
    </div>
  );
}
