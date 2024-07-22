

const space = require('color-space').default;

type RGB = [number, number, number];

const goldenAngle = Math.PI * (3 - Math.sqrt(5));

export class Palette {
  
  saturation: number;
  
  cache: Map<number, RGB>;

  static hue = 0;
  
  static saturation = 0.5;
  
  static fullColorLuminance = 0.75;
  
  constructor(
    colorParams?: {
      hueOffset?: number,
      saturation?: number,
      luminanceVariance?: number,
      hueVariance?: number,
    },
  ) {
    const {
      saturation = Palette.saturation,
    } = colorParams ?? {};
    this.saturation = saturation;
    this.cache = new Map();
  }
  
  get(colorNum: number): RGB {
    if(!this.cache.has(colorNum)) {
      Palette.hue = ((Palette.hue + goldenAngle) % (Math.PI * 2));
      const hue = space.lab.rgb([
        74,
        40 * Math.cos(Palette.hue),
        40 * Math.sin(Palette.hue),
      ]);
      this.cache.set(colorNum, hue);
    }
    return this.cache.get(colorNum)!;
  }

  getCSS(colorNum: number): string {
    return `rgb(${this.get(colorNum).join(',')})`;
  }

}
