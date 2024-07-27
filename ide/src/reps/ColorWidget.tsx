import React from 'react';
import type * as R from '../../../src/runtime/runtime';
import { 
  getAsyncRuntime,
  getAsyncModuleByName,
  builtinPath
} from '../runner';
import ValueSkeletonWidget from './ValueSkeletonWidget';
import type * as I from '../../../src/runtime/image.arr';
import type * as C from '../../../src/runtime-arr/color.arr';
import Brush from '../img/brush.svg';
import Checkers from '../img/checkers.svg';
import Paint from '../img/paint.svg';

import './ColorWidget.css';

type CWProps = {
  color: C.Color,
  skeleton: R.ValueSkeleton,
};

import NUMBER from "../../../src/runtime/js-numbers";

export default function ColorWidget(props: CWProps) {
  const [expanded, setExpanded] = React.useState(false);
  const Image = getAsyncModuleByName(builtinPath('image.arr')) as typeof I;
  function clampFixed(n: any, min: number, max: number): number {
    return Image.clamp(NUMBER.toFixnum(n), min, max);
  }
  const { color, skeleton } = props;
  const name =  Image.colorDb.colorName(color);
  const r = clampFixed(color.red, 0, 255);
  const g = clampFixed(color.green, 0, 255);
  const b = clampFixed(color.blue, 0, 255);
  const a = clampFixed(color.alpha, 0, 1.0);
  const rgba = `rgba(${r}, ${g}, ${b}, ${a})`
  return <span>
    <span onClick={() => setExpanded(!expanded)}>
      <img src={Brush} title={rgba} className='paintBrush' />
      <span className='paintSpan'>
        <span className='checkersBlob' />
        <span className='paintBlob' style={{ 
          backgroundColor: rgba,
          marginRight: '0.25em'
        }} />
      </span>
    </span>
    {expanded ? <ValueSkeletonWidget value={skeleton} /> : name}
  </span>
}