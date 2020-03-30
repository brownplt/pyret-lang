import React from 'react';

type FontSizeProps = {
  onIncrease: () => void,
  onDecrease: () => void,
  onReset: () => void,
  size: number,
};

export default function FontSize({
  onIncrease, onDecrease, onReset, size,
}: FontSizeProps) {
  return (
    <div className="font-size-options">
      <button
        className="font-minus"
        onClick={onDecrease}
        type="button"
      >
        -
      </button>
      <button
        className="font-label"
        onClick={onReset}
        type="button"
      >
        Font (
        {size}
        {' '}
        px)
      </button>
      <button
        className="font-plus"
        onClick={onIncrease}
        type="button"
      >
        +
      </button>
    </div>
  );
}
