/* Creates an option in the run button dropdown menu (autorun, stopify, type
   check). This class is used in Run.tsx. These should be children of the class
   in Dropdown.tsx */

// TODO (michael): improve accessibilty by enabling these rules
/* eslint-disable jsx-a11y/click-events-have-key-events */
/* eslint-disable jsx-a11y/no-static-element-interactions */

import React from 'react';

type DropdownOptionProps = {
  id?: string,
  enabled: boolean,
  onClick: () => void,
  children: React.ReactNode,
};

export default function DropdownOption(props: DropdownOptionProps) {
  const {
    id,
    enabled,
    onClick,
    children,
  } = props;

  const internalName = `DropdownOption${new Date().getTime()}`;

  return (
    <div
      id={id}
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
