// Heavily adapted from Iodide, MPL 2.0
// https://github.com/iodide-project/iodide/blob/master/src/reps/serialization/split-index-range.js

import { ContainerRange } from './Range';

const RANGE_SPLIT_THRESHOLD = 50;
const TARGET_LEAF_ELTS = 75;

const ln = Math.log;

function targetNumSubRanges(
  rangeSize: number,
  targetNumSubRangesPerSplit: number,
  targetLeafBinSize: number,
): number {
  // this function gets a target number of subranges for
  // the given
  // (1) the number of elements in the range
  // (2) the number of subranges desired in case of a range split
  // (3) the number of leaf elements to show in a range that will not be split
  // To do this, figure out the integer tree depth that comes closest
  // to satisfying that objective. Then, given that integer depth,
  // return the number of sub ranges that would be needed
  // to build a tree of that depth with the given constraints
  const [N, x, y] = [rangeSize, targetNumSubRangesPerSplit, targetLeafBinSize];
  // NOTE: x**Depth = N/y, therefore:
  const targetSubrangeDepth = Math.round(ln(N / y) / ln(x));
  return targetSubrangeDepth > 0 ? (N / y) ** (1 / targetSubrangeDepth) : N / y;
}

export default function splitIndexRange<T>(
  rangeDescriptor: ContainerRange<T>,
  minBinSize: number = 10,
  rangeSplitThreshold: number = RANGE_SPLIT_THRESHOLD,
): Array<ContainerRange<T>> {
  const { min, max, source } = rangeDescriptor;
  const s = source;

  const rangeSize = max - min;

  // no need to split bins smaller than a certain size
  if (rangeSize <= rangeSplitThreshold || rangeSize <= minBinSize) {
    return [rangeDescriptor];
  }

  const targetNumRanges = targetNumSubRanges(rangeSize, 20, TARGET_LEAF_ELTS);

  let binSize = Math.round(rangeSize / targetNumRanges);
  // these next couple lines are just a bit of footwork to give
  // our bins nice round edges in base 10
  const digits = Math.floor(Math.log10(binSize));
  binSize = Math.round(binSize / 10 ** digits) * 10 ** digits;

  // binSize should not be smaller than minBinSize
  binSize = Math.max(binSize, minBinSize);

  // this is a bit of footwork to set the lower bound of each bin after the
  // first to be a nice round number
  const secondBinMin = Math.ceil(min / binSize) * binSize;
  const ranges = (
    min < secondBinMin ? [new ContainerRange(s, min, secondBinMin - 1)] : []
  );

  let binLowBound;
  for (
    binLowBound = secondBinMin;
    binLowBound + binSize < max;
    binLowBound += binSize
  ) {
    ranges.push(new ContainerRange(s, binLowBound, binLowBound + binSize - 1));
  }
  ranges.push(new ContainerRange(s, binLowBound, max));

  return ranges;
}
