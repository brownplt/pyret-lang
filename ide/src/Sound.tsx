import React from 'react';

type SoundWidgetProps = {
    sound: any
};
type SoundWidgetState = {};

export class SoundWidget extends React.Component<SoundWidgetProps, SoundWidgetState> {
  constructor(props : SoundWidgetProps) {
    super(props);
  }
  render() {
      return (
          <button onClick={this.props.sound.play}>play</button>
      )
  }
}
