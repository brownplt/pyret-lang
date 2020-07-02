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
