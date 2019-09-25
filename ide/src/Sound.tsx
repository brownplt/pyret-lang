import React from 'react';

type SoundWidgetProps = {
    sound: any
};
type SoundWidgetState = {
   
};

export class SoundWidget extends React.Component<SoundWidgetProps, SoundWidgetState> {
canvas: any;
  constructor(props : SoundWidgetProps) {
    super(props);
    this.canvas = React.createRef();
  }

  drawTest = () => {
    const canvas = this.canvas.current;
    const context = canvas.getContext('2d');
    context.lineWidth = 2;
    context.strokeStyle = '#000000';
    context.clearRect(0, 0,100, 100)
    context.beginPath();
    const height = canvas.height;
    const width = canvas.width;
    const deltaX = canvas.width / this.props.sound['data-array'].length;
    
    context.moveTo(0, 0);
    let x = 0;
    for (const num of this.props.sound['data-array']) {
        context.lineTo(x, Math.abs(num) * 5);
        x += deltaX;
    }
    context.stroke();
  }

  componentDidUpdate() {
    this.drawTest();
  }

  componentDidMount() {
    this.drawTest();
  }

  
  render() {
      return (
          <div>
          <button onClick={this.props.sound.play}>Play</button>
          <canvas
            width={100}
            height={100}
            ref={this.canvas}></canvas>
          </div>
      )
  }
}
