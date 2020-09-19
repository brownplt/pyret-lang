/* Simple class that contains the header components of the page (run button,
   file button, options button) */

import React from 'react';

export default function Header({ children }: { children: any }) {
  return (
    <div
      className="header-container"
      style={{
        display: 'flex',
        justifyContent: 'space-between',
      }}
    >
      {children}
    </div>
  );
}
