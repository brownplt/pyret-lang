/* A widget for displaying images. Image documentation:
   https://www.pyret.org/docs/latest/image.html

   Images are created in RenderedValue.tsx */

import React from 'react';

type ImageWidgetProps = {
  image: any
};
type ImageWidgetState = {};

export default class ImageWidget extends React.Component<ImageWidgetProps, ImageWidgetState> {
  canvas: HTMLCanvasElement | null;

  constructor(props : ImageWidgetProps) {
    super(props);
    this.canvas = null;
  }

  componentDidMount() {
    this.updateCanvas();
  }

  componentDidUpdate() {
    this.updateCanvas();
  }

  updateCanvas() {
    const { image } = this.props;
    const ctx = this.canvas!.getContext('2d');
    ctx!.clearRect(0, 0, image.getWidth(), image.getHeight());
    image.render(ctx, 0, 0);
  }

  render() {
    const { image } = this.props;
    return (
      <canvas
        width={image.getWidth()}
        height={image.getHeight()}
        ref={(canvas) => {
          this.canvas = canvas;
        }}
      />
    );
  }
}
