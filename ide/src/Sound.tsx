import React from 'react';
import './Sound.css';

const blackDownloadIcon = require('./SoundWidgetImages/download_black.png');
const whitePlayIcon = require('./SoundWidgetImages/play_white.png');
const blackPlayIcon = require('./SoundWidgetImages/play_black.png');
const whiteZoomIcon = require('./SoundWidgetImages/zoomout_white.png');
const blackZoomIcon = require('./SoundWidgetImages/zoomout_black.png');
const whitePauseIcon = require('./SoundWidgetImages/pause_white.png');
const blackPauseIcon = require('./SoundWidgetImages/pause_black.png');
const whiteResetIcon = require('./SoundWidgetImages/reset_white.png');
const blackResetIcon = require('./SoundWidgetImages/reset_black.png');

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
   focusDuration: number,
   hoverLoc: number,
   zoomLog: number[][],
   progressDisplay: number
};

export class SoundWidget extends React.Component<SoundWidgetProps, SoundWidgetState> {
  waveformCanvas: any;
  progressCanvas: any;
  HEIGHT: number = 100;
  WIDTH: number = 425;
  FPS: number = 50.0;
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
      focusDuration: this.props.sound.duration,
      hoverLoc: 0,
      zoomLog: [],
      progressDisplay: 0
    }
  }

  playSound = () => {
    const dataArray = this.props.sound['data-array'];
    const timePassed = this.state.progress / this.FPS;
    const duration = this.props.sound.duration - timePassed;
    const sampleRate = this.props.sound['sample-rate'];
    const frameCount = duration * sampleRate;
    let startIndex = Math.round((timePassed / this.state.focusDuration) * (this.state.endIndex-this.state.startIndex) + this.state.startIndex);
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
      let startPixel = this.state.startBox; 
      let endPixel = x;
      if(x < this.state.startBox) {
        startPixel = x;
        endPixel = this.state.startBox;
      }
      if(endPixel - startPixel >= this.MIN_ZOOM_BOX_WIDTH) {
        const startIndex = Math.round(this.state.startIndex + (startPixel / this.WIDTH) * this.state.focusDuration * this.props.sound['sample-rate']);
        const endIndex = Math.round(this.state.startIndex + (endPixel / this.WIDTH) * this.state.focusDuration * this.props.sound['sample-rate']);
        const focusDuration = (endPixel - startPixel) / this.WIDTH * this.state.focusDuration;

        if(endIndex - startIndex > this.MIN_PIXELS_VIEW) {
          this.state.zoomLog.push([this.state.startIndex, this.state.endIndex, this.state.focusDuration]);
          if(this.state.isPlaying) {
            this.togglePlay();
          }
          this.setState({progress: 0, startIndex, endIndex, focusDuration, progressDisplay: -1 });
          
        }

      }
      this.setState({ isMouseDown: false, startBox: 0, endBox: -1});
     
     
    }
  }
  handleMouseMove = (e:any) => {
    var rect = e.target.getBoundingClientRect();
    let x = e.clientX - rect.left;
    this.setState({hoverLoc: x});
    if(this.state.isMouseDown) {
      this.setState({endBox: x});
    }
  }
  handleClick = (e : any) => {
   
    var rect = e.target.getBoundingClientRect();
    let x = e.clientX - rect.left
    let maxProgress = this.FPS * this.state.focusDuration;
    let newProgDisplay = (x / this.WIDTH) * (maxProgress);
    let newProg = Math.round((x / this.WIDTH) * (maxProgress));
    this.setState({progress: newProg, progressDisplay: newProgDisplay});
    
   if(this.state.isPlaying) {
    this.togglePlay();
    setTimeout(this.togglePlay, 0);
   }
  }

  componentDidMount() {
    setInterval(this.updateProgress, 1000 / this.FPS);
  }

  updateProgress = () => {
    if(this.state.isPlaying) {
      this.setState({progress : this.state.progress + 1, progressDisplay: -1});
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
    if(this.state.isPlaying) {
      this.togglePlay();
    }
    this.setState({progress: 0, startIndex: 0, endIndex: this.props.sound.duration * this.props.sound['sample-rate'], focusDuration: this.props.sound.duration });
  }

  getCurrentIndex = () => {
    let progress = this.state.progressDisplay > 0 ? this.state.progressDisplay : this.state.progress;
    return Math.round((progress / this.FPS) * this.props.sound['sample-rate'] + this.state.startIndex);
  }

  getAmplitudeAt = (index : number) : string =>  {
    let amp = this.props.sound['data-array'][0][index]
    if(amp === undefined) {
      amp = 0;
    }
    return amp < 0 ? "" + amp.toFixed(4) : " " + amp.toFixed(4);
  }

  getHoverIndex = () => {
     return Math.round(this.state.hoverLoc / this.WIDTH * this.state.focusDuration * this.props.sound['sample-rate'] + this.state.startIndex);
  }

  getResetIcon = () => {
    if(this.state.progress === 0) {
      return whiteResetIcon;
    }
    return blackResetIcon;
  }

  getPlayIcon = () => {
    if(this.state.isPlaying) {
      return blackPauseIcon;
    }
    return blackPlayIcon;
  }

  getZoomIcon = () => {
    if(this.state.focusDuration === this.props.sound.duration) {
      return whiteZoomIcon;
    }
    return blackZoomIcon;
  }

  getDownloadIcon = () => {
    return blackDownloadIcon;
  }

  getTimeString = () => {
    let duration = Math.round(this.props.sound.duration);
    let seconds = Math.round(this.props.sound.duration % 60);
    let minutes = (duration - seconds) / 60;
    let duration_string = this.twoPlaces(minutes) + ":" + this.twoPlaces(seconds);

    let timePassed = Math.round(this.getCurrentIndex() / this.props.sound['sample-rate'])
    seconds = timePassed % 60;
    minutes = Math.round((timePassed - seconds) / 60);
    return this.twoPlaces(minutes) + ":" + this.twoPlaces(seconds) + "/" + duration_string;

  }

  twoPlaces = (a : number) => {
    if(a < 10) return "0" + a;
    return a;
  }

  handleZoomOut = () => {
    let prev = this.state.zoomLog.pop();
    if(prev === undefined) return;
    if(this.state.isPlaying) {
      this.togglePlay();
    }
    this.setState({progress: 0, startIndex: prev[0], endIndex: prev[1], focusDuration: prev[2]});

  }

  //returns true if the operation was successful
  setFocus = (startIndex: number, endIndex: number) : boolean => {

    return true;
  }
  render() {
      return (
          <div>
          <div className="ButtonBar" style={{background: "#3790cc", display:"flex", maxWidth: "150px", paddingLeft: "10px", paddingTop: "5px", paddingBottom: "5px"}}>
            <MyButton onClick={this.handleReset} icon={this.getResetIcon()} isDisabled={this.state.progress === 0}/>
            <MyButton onClick={this.togglePlay} icon={this.getPlayIcon()} isDisabled={false}/>
            <MyButton onClick={this.handleResetZoom} icon={this.getZoomIcon()} isDisabled={this.state.focusDuration === this.props.sound.duration}/>
            <MyButton onClick={() => {}} icon={this.getDownloadIcon()} isDisabled={true}/>
            <MyButton onClick={this.handleZoomOut} icon={this.getZoomIcon()} isDisabled={false} />
          </div>
          <div style={{background: "#3790cc", paddingBottom: "20px", textAlign: "center"}}>
            {this.props.sound['data-array'].map((channel: number[]) => {
              return <OverlayedWaveForm 
                handleClick={this.handleClick}
                handleMouseDown={this.handleMouseDown}
                handleMouseUp={this.handleMouseUp}
                handleMouseMove={this.handleMouseMove}
                width={this.WIDTH}
                height={this.HEIGHT}
                startBox={this.state.startBox}
                endBox={this.state.endBox}
                progressDisplay={this.state.progressDisplay}
                progress={this.state.progress}
                focusDuration={this.state.focusDuration}
                FPS={this.FPS}
                channel={channel}
                startIndex={this.state.startIndex}
                endIndex={this.state.endIndex}
                 />
            })}
            <p style={{margin: "5px 0 0 0",color: "white"}}>{this.getTimeString()}</p>
            <div className="DataContainer" style={{color: "white", display: "flex"}}>
              <div className="Index"> 
                <p>{"Index: " + this.getCurrentIndex()}</p>
                <p>{"Amp:  " + this.getAmplitudeAt(this.getCurrentIndex())}</p> 
              </div>
              <div className="Index">
                <p>{"Hover Index: " + this.getHoverIndex()}</p>
                <p>{"Hover Amp:  " + this.getAmplitudeAt(this.getHoverIndex())}</p>
              </div>
            </div>
          </div>
          </div>
          
          
      )
  }


}

type OverlayedWaveFormProps = {
  handleClick: any,
  handleMouseDown: any,
  handleMouseUp: any,
  handleMouseMove: any,
  width: number,
  height: number,
  startBox: number,
  endBox: number,
  progressDisplay: number,
  progress: number,
  focusDuration: number,
  FPS: number,
  channel: number[],
  startIndex: number,
  endIndex: number
}



class OverlayedWaveForm extends React.Component<OverlayedWaveFormProps, {}> {
  progressCanvas : any;
  constructor(props: OverlayedWaveFormProps) {
    super(props);
    this.progressCanvas = React.createRef();
  }
  componentDidMount() {
    this.drawProgress();
  }
  componentDidUpdate() {
    this.drawProgress();
  }
  drawProgress = () => {
    const canvas = this.progressCanvas.current;
    const context = canvas.getContext('2d');
    context.lineWidth = 2;
    context.strokeStyle = "#FF0000";
    context.clearRect(0, 0,this.props.width, this.props.height);
    context.beginPath();
    let progress = this.props.progressDisplay > 0 ?  this.props.progressDisplay : this.props.progress;
    let lineX = ((progress/this.props.FPS / (this.props.focusDuration))) * this.props.width;
    context.moveTo(Math.max(1, lineX), 0);
    context.lineTo(Math.max(1,lineX), this.props.height);
    context.stroke();
    context.strokeStyle = "#0000FF";
    if(this.props.endBox > 0) {
      context.strokeRect(this.props.startBox,0,this.props.endBox - this.props.startBox, this.props.height);
    }
    
  }
  render() {
    return <div className="CanvasWrapper" style={{marginTop: "2px", background: "#3790cc" , textAlign: "center", width: this.props.width + 50, height: this.props.height + 10}}>
              <div className="CanvasContainer" style={{marginTop: "10px", display: "inline-block", position: "relative", width: this.props.width, height: this.props.height}}
                onDoubleClick={this.props.handleClick}
                onMouseDown={this.props.handleMouseDown}
                onMouseUp={this.props.handleMouseUp}
                onMouseMove={this.props.handleMouseMove}>
              <WaveForm width={this.props.width} height={this.props.height} endIndex={this.props.endIndex} startIndex={this.props.startIndex} dataArray={this.props.channel}/>
              <canvas
              width={this.props.width}
              height={this.props.height}
              style={{ position: "absolute", top: "0", left: "0"}}
              ref={this.progressCanvas}
              ></canvas>
              </div>
              </div>
  }
}

type WaveFormProps = {
  width: number,
  height: number,
  startIndex: number,
  endIndex: number,
  dataArray: number[]

}

type WaveFormState = {

}

class WaveForm extends React.Component<WaveFormProps, WaveFormState> {

waveformCanvasRef : any;
  constructor(props: WaveFormProps) {
    super(props);
    this.waveformCanvasRef = React.createRef();
  }

  drawWaveForm = () => {
    const canvas = this.waveformCanvasRef.current;
    const context = canvas.getContext('2d');
    context.lineWidth = 1;
    context.fillStyle = '#FFFFFF';
    context.fillRect(0, 0,this.props.width, this.props.height);
    context.strokeStyle = '#000000';
    context.beginPath();
    context.moveTo(0, this.props.height / 2.0);
    const channel0 = this.props.dataArray; //first 100 elements for now
    const deltaX = this.props.width / (this.props.endIndex - this.props.startIndex);
    let x = 0;
    for(let i = this.props.startIndex; i < this.props.endIndex; i++) {
      let y_coord =  (this.props.height / 2 ) - channel0[i] * (this.props.height / 2);
      context.lineTo(x,y_coord);
      x += deltaX;
    }
   
    context.stroke();  
  }

  componentDidMount() {
    this.drawWaveForm();
  }

  shouldComponentUpdate(nextProps : WaveFormProps, nextState: WaveFormState) {
    return nextProps.startIndex != this.props.startIndex || nextProps.endIndex != this.props.endIndex;
  }

  componentDidUpdate() {
    this.drawWaveForm();
  }
  render() {
    
    return <canvas
    width={this.props.width}
    height={this.props.height}
    style={{ position: "absolute", top: "0", left: "0"}}
    ref={this.waveformCanvasRef}
    ></canvas>;
  }
}

type MyButtonProps = {
  onClick: any,
  icon: any,
  isDisabled: boolean
}

class MyButton extends React.Component<MyButtonProps, {}> {
  render() {
    return <div  onClick={this.props.isDisabled ? () => {} : this.props.onClick}> 
      <div className={this.props.isDisabled ? "fake" : "hoverable"} style={{ maxHeight: "25px", minHeight: "25px", height: "25px", marginRight: "10px" }}>
        <img style={{ display: "block", maxHeight: "100%", minHeight: "100%" }} src={this.props.icon} />
      </div>
    </div>

  }
}


