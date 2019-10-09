import React from 'react';
//props = para to constr to widget
type SoundWidgetProps = {
    sound: any
};
type SoundWidgetState = {
   progress: number,
   isPlaying: boolean,
   isMouseDown: boolean,
   startBox: number,
   endBox: number,
   startIndex: number, //zoom related
   endIndex: number,
   focusDuration: number
};

export class SoundWidget extends React.Component<SoundWidgetProps, SoundWidgetState> {
  waveformCanvas: any;
  progressCanvas: any;
  HEIGHT: number = 100;
  WIDTH: number = 500;
  FPS: number = 30.0;
  source: any;
  audioCtx: any;
  MIN_ZOOM_BOX_WIDTH: number = 5;
  MIN_PIXELS_VIEW: number = 40;
  constructor(props : SoundWidgetProps) {
    super(props);
    this.waveformCanvas = React.createRef();
    this.progressCanvas = React.createRef();
    this.audioCtx = new AudioContext();
    this.state = {
      progress: 0, // 1 unit = 1/10 second
      isPlaying: false,
      isMouseDown: false,
      startBox: 0,
      endBox: -1,
      startIndex: 0,
      endIndex: this.props.sound.duration * this.props.sound['sample-rate'],
      focusDuration: this.props.sound.duration
    }
  }

  playSound = () => {
    const dataArray = this.props.sound['data-array'];
   // const numChannels = dataArray.length;
    const timePassed = this.state.progress / this.FPS;
    const duration = this.props.sound.duration - timePassed;
    const sampleRate = this.props.sound['sample-rate'];
    const frameCount = duration * sampleRate;
    let startIndex = Math.round((timePassed / this.state.focusDuration) * (this.state.endIndex-this.state.startIndex) + this.state.startIndex);
    console.log("starting to play at  " + startIndex + ". Endindex=" + this.state.endIndex + " start: " + this.state.startIndex);
    var myArrayBuffer = this.audioCtx.createBuffer(1, (this.state.focusDuration - timePassed) * sampleRate, sampleRate);
    for (var channel = 0; channel < myArrayBuffer.numberOfChannels; channel++) {
      var nowBuffering = myArrayBuffer.getChannelData(channel);
      let soundArray = dataArray[channel];
      for (var i = startIndex; i < this.state.endIndex; i++) {
        nowBuffering[i-startIndex] = soundArray[i];
      }
    }
    this.source = this.audioCtx.createBufferSource();
    this.source.buffer = myArrayBuffer;
    this.source.connect(this.audioCtx.destination);
    this.source.start();
  }

  togglePlay = () => {
    if (!this.state.isPlaying) {
      this.playSound();
      }
      else {
        this.source.stop();
      }
    this.setState({ isPlaying: !this.state.isPlaying });
  }

  handleMouseDown = (e:any) => {
    
    var rect = e.target.getBoundingClientRect();
    let x = e.clientX - rect.left;
    this.setState({isMouseDown: true, startBox: x});
  }
  handleMouseUp = (e:any) => {
    if(this.state.isMouseDown) {
      var rect = e.target.getBoundingClientRect();
      let x = e.clientX - rect.left;
      //console.log("Box from: " + this.state.startBox + " to " + this.state.endBox);
      let startPixel = this.state.startBox; //starting pixel
      let endPixel = x; //ending pixel
      if(x < this.state.startBox) {
        startPixel = x;
        endPixel = this.state.startBox;
      }
      if(endPixel - startPixel >= this.MIN_ZOOM_BOX_WIDTH) {
        const startIndex = Math.round(this.state.startIndex + (startPixel / this.WIDTH) * this.state.focusDuration * this.props.sound['sample-rate']);
        const endIndex = Math.round(this.state.startIndex + (endPixel / this.WIDTH) * this.state.focusDuration * this.props.sound['sample-rate']);
        const focusDuration = (endPixel - startPixel) / this.WIDTH * this.state.focusDuration;
        console.log("Box:" + startIndex + " to " + endIndex + " duration: " + focusDuration);
        if(endIndex - startIndex > this.MIN_PIXELS_VIEW) {
          this.setState({progress: 0, startIndex, endIndex, focusDuration });
          setTimeout(this.drawWaveForm, 100);
        }

      }
      this.setState({ isMouseDown: false, startBox: 0, endBox: -1});
     
     
    }
  }
  handleMouseMove = (e:any) => {
    if(this.state.isMouseDown) {
      var rect = e.target.getBoundingClientRect();
      let x = e.clientX - rect.left;
      this.setState({endBox: x});
    }
  }
  handleClick = (e : any) => {
   
    var rect = e.target.getBoundingClientRect();
    let x = e.clientX - rect.left
    //console.log(e.clientX - rect.left);
    let maxProgress = this.FPS * this.state.focusDuration;
    let newProg = Math.round((x / this.WIDTH) * (maxProgress));
    
    this.setState({progress: newProg});
    
   if(this.state.isPlaying) {
    this.togglePlay();
    setTimeout(this.togglePlay, 100);
   
   }
  }

  
  drawProgress = () => {
    const canvas = this.progressCanvas.current;
    const context = canvas.getContext('2d');
    context.lineWidth = 1;
    context.strokeStyle = "#FF0000";
    context.clearRect(0, 0,this.WIDTH, this.HEIGHT);
    context.beginPath();
    let lineX = ((this.state.progress/this.FPS / (this.state.focusDuration))) * this.WIDTH;
    context.moveTo(lineX, 0);
    context.lineTo(lineX, this.HEIGHT);
    context.stroke();
    context.strokeStyle = "#0000FF";
    if(this.state.endBox > 0) {
      context.strokeRect(this.state.startBox,0,this.state.endBox - this.state.startBox, this.HEIGHT);
    }
    
  }

  drawWaveForm = () => {
    console.log("drawing wave form start: " + this.state.startIndex);
    const canvas = this.waveformCanvas.current;
    const context = canvas.getContext('2d');
    context.lineWidth = 1;
    context.strokeStyle = '#000000';
    context.clearRect(0, 0,this.WIDTH, this.HEIGHT);
    context.beginPath();
    context.moveTo(0, this.HEIGHT / 2.0);
    const channel0 = this.props.sound['data-array'][0]; //first 100 elements for now
   // const channel0 = Array.from({length: 100}, () => Math.random() * 2 - 1);
    const deltaX = this.WIDTH / (this.state.endIndex - this.state.startIndex);
    let x = 0;
    for(let i = this.state.startIndex; i < this.state.endIndex; i++) {
      let y_coord =  (this.HEIGHT / 2 ) - channel0[i] * (this.HEIGHT / 2);
      context.lineTo(x,y_coord);
      x += deltaX;
    }
   
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
      if(this.state.progress / this.FPS > this.state.focusDuration) {
        this.setState({progress: 0});
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

  handleResetZoom = () => {
    this.setState({progress: 0, startIndex: 0, endIndex: this.props.sound.duration * this.props.sound['sample-rate'], focusDuration: this.props.sound.duration });
    setTimeout(this.drawWaveForm, 25);
  }
  render() {
      return (
          <div style={{ border: "1px solid red"}}>
          <button onClick={this.togglePlay}>{!this.state.isPlaying ? "Play" : "Stop"}</button>
          <button onClick={this.handleReset}>{this.state.progress}</button>
          <button onClick={this.handleResetZoom}>Reset Zoom </button>
          <div style={{position: "relative", width: this.WIDTH, height: this.HEIGHT}}
            onDoubleClick={this.handleClick}
            onMouseDown={this.handleMouseDown}
            onMouseUp={this.handleMouseUp}
            onMouseMove={this.handleMouseMove}>
          <canvas
            width={this.WIDTH}
            height={this.HEIGHT}
            style={{ position: "absolute", top: "0", left: "0", border: "1px solid blue"}}
            ref={this.waveformCanvas} 
            ></canvas>
            
          <canvas
            width={this.WIDTH}
            height={this.HEIGHT}
            style={{ position: "absolute", top: "0", left: "0",border: "1px solid pink"}}
            ref={this.progressCanvas}
            ></canvas>
          </div>
          </div>
      )
  }
}