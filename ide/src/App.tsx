/* This is the toplevel component for the IDE. Not much happens here other than
   setting up the Redux store through the Provider component. All of the
   interesting things start to happen in the ConnectedEditor component from
   Editor.tsx. */

import React from 'react';
import { Provider } from 'react-redux';
import ConnectedEditor from './Editor';
import store from './store';

export default function App() {
  return (
    <Provider store={store}>
      <ConnectedEditor />
    </Provider>
  );
}
