import React from 'react';

type ExpandButtonProps = {
  expanded: boolean,
  setExpanded: (exp: boolean) => void,
  showArrow?: boolean,
  children?: React.ReactNode,
};

export default function ExpandButton({
  expanded, setExpanded, showArrow, children,
}: ExpandButtonProps) {
  const label = expanded ? 'Click to collapse' : 'Click to expand';
  const style = {
    // from Normalize, from css-tricks
    // https://css-tricks.com/overriding-default-button-styles/
    fontFamily: 'inherit',
    lineHeight: 1.15,
    margin: 0,
    // Doesn't seem to be enough in chrome:
    border: 0,
    background: 'none',
    padding: '0.3em',
    cursor: 'pointer',
  };
  // down arrow, right arrow
  const arrow = expanded ? '\u25BC' : '\u25B6';
  return (
    <button className="expander" type="button" onClick={() => setExpanded(!expanded)} aria-label={label} style={style}>
      {showArrow ? arrow : ''}
      {children}
    </button>
  );
}

ExpandButton.defaultProps = { showArrow: true, children: '' };
