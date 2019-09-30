import React from 'react';

type SoundWidgetProps = {
    sound: any
};
type SoundWidgetState = {
   progress: number,
   isPlaying: boolean
};

export class SoundWidget extends React.Component<SoundWidgetProps, SoundWidgetState> {
  waveformCanvas: any;
  progressCanvas: any;
  HEIGHT: number = 100;
  WIDTH: number = 600;
  FPS: number = 30.0;
  source: any;
  audioCtx: any;
  constructor(props : SoundWidgetProps) {
    super(props);
    this.waveformCanvas = React.createRef();
    this.progressCanvas = React.createRef();
    this.audioCtx = new AudioContext();
    this.state = {
      progress: 0, // 1 unit = 1/10 second
      isPlaying: false
    }
  }

  togglePlay = () => {
    
    if (!this.state.isPlaying) {
      const dataArray = this.props.sound['data-array'];
    const numChannels = dataArray.length;
    const duration = this.props.sound.duration;
    const sampleRate = this.props.sound['sample-rate'];
    const frameCount = duration * sampleRate;
   
      var myArrayBuffer = this.audioCtx.createBuffer(numChannels, frameCount, sampleRate);
      for (var channel = 0; channel < myArrayBuffer.numberOfChannels; channel++) {
        var nowBuffering = myArrayBuffer.getChannelData(channel);
        let soundArray = dataArray[channel];
        
        for (var i = 0; i < myArrayBuffer.length; i++) {
          nowBuffering[i] = soundArray[i];
        }
      }
      
      this.source = this.audioCtx.createBufferSource();
      this.source.buffer = myArrayBuffer;
      this.source.connect(this.audioCtx.destination);
      this.source.start();
      }
      else {
        this.source.stop();
      }
    this.setState({ isPlaying: !this.state.isPlaying });
  }

  
  drawProgress = () => {
    const canvas = this.progressCanvas.current;
    const context = canvas.getContext('2d');
    context.lineWidth = 1;
    context.strokeStyle = "#FF0000";
    context.clearRect(0, 0,this.WIDTH, this.HEIGHT);
    context.beginPath();
    let lineX = ((this.state.progress/this.FPS / (this.props.sound.duration))) * this.WIDTH;
    context.moveTo(lineX, 0);
    context.lineTo(lineX, this.HEIGHT);
    context.stroke();
  }
  drawWaveForm = () => {
    const canvas = this.waveformCanvas.current;
    const context = canvas.getContext('2d');
    context.lineWidth = 1;
    context.strokeStyle = '#000000';
    context.clearRect(0, 0,this.WIDTH, this.HEIGHT);
    context.beginPath();
    context.moveTo(0, this.HEIGHT / 2.0);
    const channel0 = this.props.sound['data-array'][0]; //first 100 elements for now
   // const channel0 = Array.from({length: 100}, () => Math.random() * 2 - 1);
    const deltaX = canvas.width / channel0.length;
    let x = 0;
    channel0.forEach((y:number) => {
      let y_coord =  (this.HEIGHT / 2 ) - y * (this.HEIGHT / 2);
      context.lineTo(x,y_coord);
      x += deltaX;
    });
    context.stroke();

    
  }

  componentDidUpdate() {
    //this.drawWaveForm();
    this.drawProgress();
  }

  componentDidMount() {
    this.drawWaveForm();
    this.drawProgress();
    setInterval(this.updateProgress, 1000 / this.FPS);
  }

  updateProgress = () => {
    if(this.state.isPlaying) {
      this.setState({progress : this.state.progress + 1});
      if(this.state.progress / this.FPS > this.props.sound.duration) {
        this.setState({progress: this.props.sound.duration * this.FPS});
        if(this.state.isPlaying) {
          this.togglePlay();
        }
      }
    }
  }

  handleReset = () => {
    if(this.state.isPlaying) {
      this.togglePlay();
    }
    this.setState({progress: 0});
    
    
  }

  
  render() {
      return (
          <div style={{ border: "1px solid red"}}>
          <button onClick={this.togglePlay}>{!this.state.isPlaying ? "Play" : "Stop"}</button>
          <button onClick={this.handleReset}>Reset</button>
          <div style={{position: "relative", width: this.WIDTH, height: this.HEIGHT}}>
          <canvas
            width={this.WIDTH}
            height={this.HEIGHT}
            style={{ position: "absolute", top: "0", left: "0", border: "1px solid blue"}}
            ref={this.waveformCanvas}></canvas>
          <canvas
            width={this.WIDTH}
            height={this.HEIGHT}
            style={{ position: "absolute", top: "0", left: "0",border: "1px solid pink"}}
            ref={this.progressCanvas}></canvas>
          </div>
          
          
          </div>
      )
  }
}
