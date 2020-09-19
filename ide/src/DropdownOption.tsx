/* Creates an option in the run button dropdown menu (autorun, stopify, type
   check). This class is used in Run.tsx. These should be children of the class
   in Dropdown.tsx */

// TODO (michael): improve accessibilty by enabling these rules
/* eslint-disable jsx-a11y/click-events-have-key-events */
/* eslint-disable jsx-a11y/no-static-element-interactions */

import React from 'react';

export type DropdownOptionProps = {
  enabled: boolean,
  onClick: () => void,
  children: React.ReactNode,
};

export default function DropdownOption({ enabled, onClick, children }: DropdownOptionProps) {
  const internalName = `DropdownOption${new Date().getTime()}`;

  return (
    <div
      className={enabled ? 'run-option-enabled' : 'run-option-disabled'}
      onClick={onClick}
    >
      <label
        htmlFor={internalName}
        className="run-option-label"
      >
        {children}
      </label>
    </div>
  );
}
