/* A single menu tab, containing menu children. This is what is displayed when
   "files" or "options" is clicked on the top left of the page. */

import React from 'react';

type TabProps = {
  name: string,
  icon: any,
  children: any
};

export default function Tab({ children }: TabProps) {
  return <div className="menu-content">{children}</div>;
}
