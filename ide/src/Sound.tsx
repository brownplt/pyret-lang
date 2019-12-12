import React from 'react';
import './Sound.css';


const blackDownloadIcon = require('./SoundWidgetImages/download_black.png');
const blackPlayIcon = require('./SoundWidgetImages/play_black.png');
const whiteZoomOutIcon = require('./SoundWidgetImages/zoomout_white.png');
const blackZoomOutIcon = require('./SoundWidgetImages/zoomout_black.png');
const whiteZoomInIcon = require('./SoundWidgetImages/zoomin_white.png');
const blackZoomInIcon = require('./SoundWidgetImages/zoomin_black.png');
const whiteResetZoomIcon = require('./SoundWidgetImages/resetzoom_white.png');
const blackResetZoomIcon = require('./SoundWidgetImages/resetzoom_black.png');
const blackPauseIcon = require('./SoundWidgetImages/pause_black.png');
const blackResetIcon = require('./SoundWidgetImages/reset_black.png');
const whiteResetIcon = require('./SoundWidgetImages/reset_white.png');

const FileSaver = require('file-saver');

type SoundWidgetProps = {
  sound: any
};

type SoundWidgetState = {
  progress: number, //updated FPS times per second. Used in calculation of where to draw bar.
  isPlaying: boolean,
  isMouseDown: boolean,
  startBox: number, // starting pixel of zoom box, can be -1 if not holding.
  endBox: number, // ending pixel of zoom box, as above.
  startIndex: number, // first index in focus
  endIndex: number, // las index in focus
  focusDuration: number, //the duration of the section of the sound in focus
  hoverLoc: number, //location of the mouse
  progressDisplay: number, //for small sounds, increments of progress are too large, progressDisplay can be fractional.
  //^used for when the user clicks to move progress. Snaps to be equal to progress once sound starts.
  focusedChannel: number
};

export class SoundWidget extends React.Component<SoundWidgetProps, SoundWidgetState> {
  waveformCanvas: any;
  progressCanvas: any;
  HEIGHT: number = 100;
  WIDTH: number = 425;
  FPS: number = 40.0; // rate at which progress is updated
  source: any;
  audioCtx: any;
  MIN_ZOOM_BOX_WIDTH: number = 5; // minimum number of pixels in a valid zoom box selection
  MIN_FOCUSED_SAMPLES: number = 10; // minimum number of samples the focused sound can be zoomed to.
  MAX_DURATION: number = 180;
  constructor(props: SoundWidgetProps) {
    super(props);
    this.waveformCanvas = React.createRef();
    this.progressCanvas = React.createRef();
    this.audioCtx = new AudioContext();
    this.state = {
      progress: 0,
      isPlaying: false,
      isMouseDown: false,
      startBox: 0,
      endBox: -1,
      startIndex: 0,
      endIndex: this.props.sound['data-array'][0].length - 1,
      focusDuration: this.props.sound.duration,
      hoverLoc: 0,
      progressDisplay: 0,
      focusedChannel: 0
    }
  }

  componentWillReceiveProps(nextProps: SoundWidgetProps) {
    this.setState({ //reset the state if the sound passed as props is new.
      progress: 0,
      isPlaying: false,
      isMouseDown: false,
      startBox: 0,
      endBox: -1,
      startIndex: 0,
      endIndex: nextProps.sound['data-array'][0].length - 1,
      focusDuration: nextProps.sound.duration,
      hoverLoc: 0,
      progressDisplay: 0,
      focusedChannel: 0
    });
  }
  playSound = () => {
    const dataArray = this.props.sound['data-array'];
    const timePassed = this.state.progress / this.FPS;
    const sampleRate = this.props.sound['sample-rate'];
    let startIndex = Math.round((timePassed / this.state.focusDuration) * (this.state.endIndex - this.state.startIndex) + this.state.startIndex);
    let numSamples = (this.state.focusDuration - timePassed) * sampleRate;
    if (numSamples < 1) return;
    var myArrayBuffer = this.audioCtx.createBuffer(1, (this.state.focusDuration - timePassed) * sampleRate, sampleRate);
    for (var channel = 0; channel < myArrayBuffer.numberOfChannels; channel++) {
      var nowBuffering = myArrayBuffer.getChannelData(channel);
      let soundArray = dataArray[channel];
      for (var i = startIndex; i < this.state.endIndex; i++) {
        nowBuffering[i - startIndex] = soundArray[i];
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

  handleMouseLeave = (e: any) => {
    if (this.state.isMouseDown) {
      this.setState({ progress: 0, startBox: 0, endBox: 0, progressDisplay: -1, isMouseDown: false });
    }
  }

  /*
    Mouse down sets startBox, one side of a zoom box.
  */
  handleMouseDown = (e: any) => {
    var rect = e.target.getBoundingClientRect();
    let x = e.clientX - rect.left;
    this.setState({ isMouseDown: true, startBox: x });
  }

  /*
    When mouse up, check to see if valid zoom box, and execute the zoom.
  */
  handleMouseUp = (e: any) => {
    if (this.state.isMouseDown) {
      var rect = e.target.getBoundingClientRect();
      let x = e.clientX - rect.left;
      let startPixel = this.state.startBox;
      let endPixel = x;
      if (x < this.state.startBox) {
        startPixel = x;
        endPixel = this.state.startBox;
      }
      if (endPixel - startPixel >= this.MIN_ZOOM_BOX_WIDTH) {
        const startIndex = Math.round(this.state.startIndex + (startPixel / this.WIDTH) * this.state.focusDuration * this.props.sound['sample-rate']);
        const endIndex = Math.round(this.state.startIndex + (endPixel / this.WIDTH) * this.state.focusDuration * this.props.sound['sample-rate']);
        if (endIndex - startIndex > this.MIN_FOCUSED_SAMPLES) {
          this.setFocus(startIndex, endIndex);

        }

      }
      this.setState({ isMouseDown: false, startBox: 0, endBox: -1 });
    }
  }

  handleMouseMove = (e: any) => {
    var rect = e.target.getBoundingClientRect();
    let x = e.clientX - rect.left;
    this.setState({ hoverLoc: x });
    if (this.state.isMouseDown) {
      this.setState({ endBox: x });
    }
  }

  //Double click handler, sets progress to clicked point.
  handleClick = (e: any) => {
    var rect = e.target.getBoundingClientRect();
    let x = e.clientX - rect.left
    let maxProgress = this.FPS * this.state.focusDuration;
    let newProgDisplay = (x / this.WIDTH) * (maxProgress);
    let newProg = Math.round((x / this.WIDTH) * (maxProgress));
    this.setState({ progress: newProg, progressDisplay: newProgDisplay });

    if (this.state.isPlaying) {
      this.togglePlay();
      setTimeout(this.togglePlay, 0);
    }
  }

  componentDidMount() {
    setInterval(this.updateProgress, 1000 / this.FPS); //we update progress at the rate defined by FPS
  }

  updateProgress = () => {
    if (this.state.isPlaying) {
      this.setState({ progress: this.state.progress + 1, progressDisplay: -1 });
      if (this.state.progress / this.FPS > this.state.focusDuration) {
        this.setState({ progress: 0 });
        if (this.state.isPlaying) {
          this.togglePlay();
        }
      }
    }
  }

  handleReset = () => {
    if (this.state.isPlaying) {
      this.togglePlay();
    }
    this.setState({ progress: 0 });
  }

  handleResetZoom = () => {
    this.setFocus(0, this.props.sound['data-array'][0].length - 1);
  }


  getCurrentIndex = () => {
    let progress = this.state.progressDisplay > 0 ? this.state.progressDisplay : this.state.progress;
    return Math.round((progress / this.FPS) * this.props.sound['sample-rate'] + this.state.startIndex);
  }

  getAmplitudeAt = (index: number, channel: number): string => {
    let amp = this.props.sound['data-array'][channel][index]
    if (amp === undefined) {
      amp = 0;
    }
    return amp < 0 ? "" + amp.toFixed(4) : " " + amp.toFixed(4); //string formatting
  }

  getHoverIndex = () => {
    return Math.floor(this.state.hoverLoc / this.WIDTH * this.state.focusDuration * this.props.sound['sample-rate'] + this.state.startIndex);
  }

  getPlayIcon = () => {
    if (this.state.isPlaying) {
      return blackPauseIcon;
    }
    return blackPlayIcon;
  }

  getDownloadIcon = () => {
    return blackDownloadIcon;
  }

  //used to produce the 00:00/03:00 string on widget.
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

  twoPlaces = (a: number) => {
    if (a < 10) return "0" + a;
    return a;
  }

  handleShift = (change: number) => {
    if (this.state.startIndex + change < 0 || this.state.endIndex + change > this.props.sound.duration * this.props.sound['sample-rate']) return;
    if (this.state.isPlaying) {
      this.togglePlay();
    }
    this.setState({ progress: 0, startIndex: this.state.startIndex + change, endIndex: this.state.endIndex + change });
  }

  handleShiftRight = () => {
    let change = Math.round((this.state.endIndex - this.state.startIndex) * 0.1);
    this.handleShift(change);
  }

  handleShiftLeft = () => {
    let change = Math.round((this.state.endIndex - this.state.startIndex) * 0.1);
    this.handleShift(-1 * change);
  }

  handleDownload = () => {
    FileSaver.saveAs(this.createWav(), "pyret_sound.wav");
  }

  createWav = () => {
    let dataArray = this.props.sound['data-array'];
    var numOfChan = dataArray.length,
      headerBytes = 44, //equal to number of bytes set below
      totalBytes = dataArray[0].length * numOfChan * 2 + headerBytes,
      buffer = new ArrayBuffer(totalBytes),
      view = new DataView(buffer),
      currentSampleIndex = 0,
      currentByteOffset = 0;

    function setUint16(data: any) {
      view.setUint16(currentByteOffset, data, true);
      currentByteOffset += 2;
    }

    function setUint32(data: any) {
      view.setUint32(currentByteOffset, data, true);
      currentByteOffset += 4;
    }

    setUint32(0x46464952);                         // "RIFF"
    setUint32(totalBytes - 8);                         // file length - 8
    setUint32(0x45564157);                         // "WAVE"

    setUint32(0x20746d66);                         // "fmt " chunk
    setUint32(16);                                 // length = 16
    setUint16(1);                                  // PCM (uncompressed)
    setUint16(numOfChan);
    setUint32(this.props.sound['sample-rate']);
    setUint32(this.props.sound['sample-rate'] * 2 * numOfChan); // avg. bytes/sec
    setUint16(numOfChan * 2);                      // block-align
    setUint16(16);                                 // 16-bit (hardcoded in this demo)

    setUint32(0x61746164);                         // "data" - chunk
    setUint32(totalBytes - currentByteOffset - 4);                   // chunk length

    while (currentByteOffset < totalBytes) {
      for (let i = 0; i < numOfChan; i++) {             // interleave channels
        let sample = Math.max(-1, Math.min(1, dataArray[i][currentSampleIndex])); // clamp to [-1, 1]
        let sample16 = Math.round(sample < 0 ? sample * 32768 : sample * 32767);
        view.setInt16(currentByteOffset, sample16, true);          // write 16-bit sample
        currentByteOffset += 2;
      }
      currentSampleIndex += 1;                                     // next source sample
    }
    return new Blob([buffer], { type: "audio/wav" });
  }

  shiftProgress = (right: boolean) => {
    if(this.soundOutOfBounds()) return;
    let delta = right ? 1 : -1;
    let maxProgress = this.FPS * this.state.focusDuration;
    let newProg = (this.state.progressDisplay == -1 ? this.state.progress : this.state.progressDisplay) + delta * maxProgress * 0.01;
    if (newProg >= maxProgress) newProg = maxProgress;
    if (newProg < 0) newProg = 0;
    this.setState({ progress: Math.round(newProg), progressDisplay: newProg });
    if (this.state.isPlaying) {
      this.togglePlay();
      setTimeout(this.togglePlay, 0);
    }
  }

  setFocus = (start: number, end: number) => {
    if(this.soundOutOfBounds()) return;
    if (end - start < this.MIN_FOCUSED_SAMPLES) {
      end = start + this.MIN_FOCUSED_SAMPLES;
    }
    if (start < 0) start = 0;
    if (end >= this.props.sound['data-array'][0].length) end = this.props.sound['data-array'][0].length - 1;
    if (this.state.isPlaying) {
      this.togglePlay();
    }
    this.setState({ progress: 0, startIndex: start, endIndex: end, focusDuration: (end - start) / this.props.sound['sample-rate'], progressDisplay: -1 });
  }

  handleKeyZoomIn = () => {
    let range = this.state.endIndex - this.state.startIndex;
    if (range <= this.MIN_FOCUSED_SAMPLES) return;
    let start = this.state.startIndex + 0.05 * range;
    let end = this.state.endIndex - 0.05 * range;
    this.setFocus(Math.round(start), Math.round(end));
  }

  handleKeyZoomOut = () => {
    let oldRange = this.state.endIndex - this.state.startIndex;
    let newRange = oldRange / 0.9;
    let middle = this.state.startIndex + oldRange * 0.5;
    let start = middle - 0.5 * newRange;
    let end = middle + 0.5 * newRange;
    this.setFocus(Math.round(start), Math.round(end));
  }

  handleKeyPress = (event: any) => {
    if (event.key == 'j') {
      this.shiftProgress(false);
    }
    if (event.key == 'k') {
      this.shiftProgress(true);
    }
    if (event.key == 'w') {
      this.handleKeyZoomIn();
    }
    if (event.key == 's') {
      this.handleKeyZoomOut();
    }
    if (event.key == ' ') {
      this.togglePlay();
    }
    if (event.key == 'a') {
      this.handleShiftLeft();
    }
    if (event.key == 'd') {
      this.handleShiftRight();
    }
  }

  zoomOutDisabled = () => {
    return this.state.startIndex == 0 && this.state.endIndex >= this.props.sound['sample-rate'] * this.props.sound.duration - 2;
  }

  zoomInDisabled = () => {
    return this.state.endIndex - this.state.startIndex <= this.MIN_FOCUSED_SAMPLES;
  }

  setFocusedChannel = (channel: number) => {
    this.setState({ focusedChannel: channel })
  }

  soundOutOfBounds = () => {
    return this.props.sound.duration > this.MAX_DURATION;
  }
  render() {
    return (
      <div aria-labelledby="tab_1" aria-label="Sound Widget" id="test" onKeyPress={this.handleKeyPress} tabIndex={0}>
        <div className="ButtonBar" style={{ background: "#3790cc", display: "flex", maxWidth: "210px", paddingLeft: "10px", paddingTop: "5px", paddingBottom: "5px" }}>
          <MyButton tabIndex={1} ariaLabel="reset" onClick={this.handleReset} icon={this.state.progress == 0 ? whiteResetIcon : blackResetIcon} isDisabled={this.state.progress === 0} />
          <MyButton tabIndex={2} ariaLabel={this.state.isPlaying ? "pause" : "play"} onClick={this.togglePlay} icon={this.getPlayIcon()} isDisabled={false} />
          <MyButton tabIndex={4} ariaLabel="zoom in" onClick={this.handleKeyZoomIn} icon={this.zoomInDisabled() || this.soundOutOfBounds() ? whiteZoomInIcon : blackZoomInIcon} isDisabled={this.zoomInDisabled() || this.soundOutOfBounds()} />
          <MyButton tabIndex={4} ariaLabel="zoom out" onClick={this.handleKeyZoomOut} icon={this.zoomOutDisabled() || this.soundOutOfBounds() ? whiteZoomOutIcon : blackZoomOutIcon} isDisabled={this.zoomOutDisabled() || this.soundOutOfBounds()} />
          <MyButton tabIndex={5} ariaLabel="reset zoom" onClick={this.handleResetZoom} icon={this.zoomOutDisabled() || this.soundOutOfBounds() ? whiteResetZoomIcon : blackResetZoomIcon} isDisabled={this.zoomOutDisabled() || this.soundOutOfBounds()} />
          <MyButton tabIndex={6} ariaLabel="download" onClick={this.handleDownload} icon={this.getDownloadIcon()} isDisabled={false} />
        </div>
        {this.soundOutOfBounds() ? <p style={{ margin: "0 0 0 0", color: "black", minWidth: "33.3%" }}>{this.getTimeString()}</p> : 
        <div style={{ background: "#3790cc", paddingBottom: "20px", textAlign: "center" }}>
          {this.props.sound['data-array'].map((channel: number[], channelNumber: number) => {
            return <div>
              <OverlayedWaveForm
                key={channelNumber}
                handleMouseLeave={this.handleMouseLeave}
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
                setFocusedChannel={() => this.setFocusedChannel(channelNumber)}
              />
              <div style={{ display: "flex", margin: "0 auto 0 auto" }}>
                <p style={{ color: "white", margin: "0px 20px 0px 25px" }}>{"Progress Index: " + this.getCurrentIndex()}</p>
                <p style={{ color: "white", margin: "0px 20px 0px 0px" }}>{"Progress Amp:  " + this.getAmplitudeAt(this.getCurrentIndex(), channelNumber)}</p>
              </div>
            </div>
          })} 

          <div className="DataContainer" style={{ color: "white", display: "flex", textAlign: "center" }}>
            <div className="Index" style={{ textAlign: "left", minWidth: "30%" }}>
              <p>{"Hover Channel: " + (this.state.focusedChannel)} </p>
              <p>{"Hover Index: " + this.getHoverIndex()}</p>
              <p>{"Hover Amp:  " + this.getAmplitudeAt(this.getHoverIndex(), this.state.focusedChannel)}</p>
            </div>
            <p style={{ margin: "25px 0 0 0", color: "white", minWidth: "33.3%" }}>{this.getTimeString()}</p>
            <div className="Index" style={{ textAlign: "left", minWidth: "33.3%" }}>
              <p>{"Focus Range"}</p>
              <p>{"[" + this.state.startIndex + ", " + this.state.endIndex + "]"} </p>
            </div>
          </div>
        </div>}
      </div>

    )
  }
}


// Props correspond to certain elements of Sound state
// also has several functions handed down from Sound.
type OverlayedWaveFormProps = {
  handleClick: any,
  handleMouseDown: any,
  handleMouseUp: any,
  handleMouseMove: any,
  handleMouseLeave: any,
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
  endIndex: number,
  setFocusedChannel: any
}


// The overlayed canvases needed to represent a channel of sound.
class OverlayedWaveForm extends React.Component<OverlayedWaveFormProps, {}> {
  progressCanvas: any;
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
    context.clearRect(0, 0, this.props.width, this.props.height);
    context.beginPath();
    let progress = this.props.progressDisplay > 0 ? this.props.progressDisplay : this.props.progress;
    let lineX = ((progress / this.props.FPS / (this.props.focusDuration))) * this.props.width;
    context.moveTo(Math.max(1, lineX), 0);
    context.lineTo(Math.max(1, lineX), this.props.height);
    context.stroke();
    context.strokeStyle = "#0000FF";
    if (this.props.endBox > 0) {
      context.strokeRect(this.props.startBox, 0, this.props.endBox - this.props.startBox, this.props.height);
    }

  }
  render() {
    return <div aria-label={"Waveform"} className="CanvasWrapper" style={{ marginTop: "2px", background: "#3790cc", textAlign: "center", width: this.props.width + 50, height: this.props.height + 10 }}>
      <div aria-hidden="true" className="CanvasContainer" style={{ marginTop: "10px", display: "inline-block", position: "relative", width: this.props.width, height: this.props.height }}
        onDoubleClick={this.props.handleClick}
        onMouseDown={this.props.handleMouseDown}
        onMouseUp={this.props.handleMouseUp}
        onMouseMove={this.props.handleMouseMove}
        onMouseLeave={this.props.handleMouseLeave}
        onMouseEnter={this.props.setFocusedChannel}>
        <WaveForm width={this.props.width} height={this.props.height} endIndex={this.props.endIndex} startIndex={this.props.startIndex} dataArray={this.props.channel} />
        <canvas
          width={this.props.width}
          height={this.props.height}
          style={{ position: "absolute", top: "0", left: "0" }}
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

//Just the waveform canvas.
class WaveForm extends React.Component<WaveFormProps, {}> {
  waveformCanvasRef: any;
  constructor(props: WaveFormProps) {
    super(props);
    this.waveformCanvasRef = React.createRef();
  }

  drawWaveForm = () => {
    const canvas = this.waveformCanvasRef.current;
    const context = canvas.getContext('2d');

    //clear the canvas
    context.fillStyle = '#FFFFFF';
    context.fillRect(0, 0, this.props.width, this.props.height);

    //begin path at first sample amplitude
    context.lineWidth = 1;
    context.strokeStyle = '#000000';
    context.beginPath();
    let startingY = (this.props.height / 2) - this.props.dataArray[0] * (this.props.height / 2);
    context.moveTo(0, startingY);

    let prev_is_above = false;
    let prev_is_below = false;

    let botRedLine: number[] = []; //list of points (unit=pixels) where waveform intersects amplitude = +1
    let topRedLine: number[] = []; //list of points (unit=pixels) where waveform intersects amplitude = -1


    if (startingY < 0) { // account for if first sample is above +1
      prev_is_above = true;
      topRedLine.push(0);
    }
    else if (startingY > this.props.height) { //account for if first sample is below -1
      prev_is_below = true;
      botRedLine.push(0);
    }

    let deltaSample = 1;
    const deltaX = this.props.width / (this.props.endIndex - this.props.startIndex);
    let pixelX = deltaX;
    for (let i = this.props.startIndex + 1; i <= this.props.endIndex; i = i + deltaSample) {

      let y = (this.props.height / 2) - this.props.dataArray[i] * (this.props.height / 2);
      let y_prev = (this.props.height / 2) - this.props.dataArray[i - 1] * (this.props.height / 2);

      if (y < 0) { // if y is above +1
        if (!prev_is_above) { // if previous not above, intersection point.
          let goal = this.calculateIntersect(pixelX, deltaX, y, y_prev, true);
          topRedLine.push(goal);
          prev_is_above = true;
        }
      } 
      else { // y is not above + 1
        if (prev_is_above) { // but previous was, so intersection.
          let goal = this.calculateIntersect(pixelX, deltaX, y, y_prev, true);
          topRedLine.push(goal);
          prev_is_above = false;
        }
      }
      if (y > this.props.height) { // same as above, but for bottom boundary (-1)
        if (!prev_is_below) {
          let goal = this.calculateIntersect(pixelX, deltaX, y, y_prev, false);
          botRedLine.push(goal);
          prev_is_below = true;
        }
      }
      else {
        if (prev_is_below) {
          let goal = this.calculateIntersect(pixelX, deltaX, y, y_prev, false);
          botRedLine.push(goal);
          prev_is_below = false;
        }
      }
      context.lineTo(pixelX, y); // draw the line to the point
      pixelX += deltaX; // increase the x pixel
    }
    context.stroke();

    if (prev_is_above) {
      topRedLine.push(pixelX);
    }
    if (prev_is_below) {
      botRedLine.push(pixelX);
    }

    //draw appropriate red line segments

    for (let i = 0; i < topRedLine.length; i = i + 2) {
      context.beginPath();
      context.moveTo(topRedLine[i], 1);
      context.lineTo(topRedLine[i + 1], 1);
      context.strokeStyle = "#FF0000";
      context.lineWidth = 1;
      context.stroke();
    }
    for (let i = 0; i < botRedLine.length; i = i + 2) {
      context.beginPath();
      context.moveTo(botRedLine[i], this.props.height - 1);
      context.lineTo(botRedLine[i + 1], this.props.height - 1);
      context.strokeStyle = "#FF0000";
      context.lineWidth = 1;
      context.stroke();
    }
  }

  //calculates intersection point (x) of waveform with boundary.
  calculateIntersect = (x: number, deltaX: number, y_coord: number, y_prev: number, top: boolean) => {
    let x_prev = x - deltaX;
    let m = (y_coord - y_prev) / deltaX;
    if (top) return (m * x_prev - y_prev) / m;
    else {
      return (m * x_prev - y_prev + this.props.height) / m;
    }

  }

  componentDidMount() {
    this.drawWaveForm();
  }

  shouldComponentUpdate(nextProps: WaveFormProps, nextState: any) {
    return nextProps.startIndex != this.props.startIndex || nextProps.endIndex != this.props.endIndex || !this.arrayEquals(nextProps.dataArray, this.props.dataArray);
  }

  arrayEquals = (arr1: number[], arr2: number[]) => {
    if (arr1.length != arr2.length) return false;
    for (let i = 0; i < arr1.length; i++) {
      if (arr1[i] != arr2[i]) return false;
    }
    return true;
  }

  componentDidUpdate() {
    this.drawWaveForm();
  }

  render() {
    return <canvas
      width={this.props.width}
      height={this.props.height}
      style={{ position: "absolute", top: "0", left: "0" }}
      ref={this.waveformCanvasRef}
    ></canvas>;
  }
}

type MyButtonProps = {
  onClick: any,
  icon: any,
  isDisabled: boolean,
  ariaLabel: string,
  tabIndex: number
}

class MyButton extends React.Component<MyButtonProps, {}> {
  buildHandleEnterKeyPress = (onClick: any) => ({ key }: any) => {
    if (key === 'Enter') {
      onClick();
    }
  };

  render() {
    return <div role="button" aria-label={this.props.ariaLabel} onKeyPress={this.props.isDisabled ? () => { } : this.buildHandleEnterKeyPress(this.props.onClick)} onClick={this.props.isDisabled ? () => { } : this.props.onClick} tabIndex={0} className={this.props.isDisabled ? "fake" : "hoverable"} style={{ maxHeight: "25px", minHeight: "25px", height: "25px", width: "25px", minWidth: "25px", marginRight: "10px" }}>
      <img aria-hidden="true" style={{ display: "block", maxHeight: "100%", minHeight: "100%" }} src={this.props.icon} />
    </div>

  }
}