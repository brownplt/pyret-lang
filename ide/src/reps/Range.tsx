/* Specify a slice of a JS array and a reference to that array. Used for
 * pagination and chunking during rendering */

export class ContainerRange<T> {
  min: number;

  max: number;

  source: T[];

  constructor(source: T[], min: number, max: number) {
    this.source = source;
    this.min = min;
    this.max = max;
  }

  size() {
    return this.max - this.min + 1;
  }

  slice(): T[] {
    return this.source.slice(this.min, this.max + 1);
  }

  summaryString(): string {
    return `${this.min}-${this.max}`;
  }
}

export function abbreviated(arr: Array<any>): Array<any> {
  const MAX_LENGTH = 10;
  const SHOW = 5;
  if (arr.length > MAX_LENGTH) {
    const end = arr.length - SHOW;
    return [...arr.slice(0, SHOW),
      new ContainerRange(arr, SHOW, end - 1),
      ...arr.slice(end, arr.length)];
  }
  return arr;
}
