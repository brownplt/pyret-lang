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
      <div>
        <canvas
          width={image.getWidth()}
          height={image.getHeight()}
          ref={(canvas) => {
            this.canvas = canvas;
          }}
        />
      </div>
    );
  }
}
