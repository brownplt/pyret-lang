import React from 'react';

type TabProps = {
  name: string,
  icon: any,
  children: any
};

export default function Tab({ children }: TabProps) {
  return <div className="menu-content">{children}</div>;
}
