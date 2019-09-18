import React from 'react';

type ImageWidgetProps = {
    image: any
};
type ImageWidgetState = {};

export class ImageWidget extends React.Component<ImageWidgetProps, ImageWidgetState> {
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
    const ctx = this.canvas!.getContext('2d');
    ctx!.clearRect(0, 0, this.props.image.getWidth(), this.props.image.getHeight());
    this.props.image.render(ctx, 0, 0);
  }
  render() {
      return (
        <div>
          <canvas
            width={this.props.image.getWidth()}
            height={this.props.image.getHeight()}
            ref={canvas => this.canvas = canvas}></canvas>
        </div>
      )
  }
}
