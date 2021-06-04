/* This is the toplevel component for the IDE. Not much happens here other than
   setting up the Redux store through the Provider component. All of the
   interesting things start to happen in the ConnectedEditor component from
   Editor.tsx. */
/* eslint-disable */
import React from 'react';
import { Provider } from 'react-redux';
import ConnectedEditor, { Editor } from './Editor';
import store from './store';
import EditorPlayground from './editor-playground/EditorPlayground';

export default function App() {
  return <EditorPlayground />;
  return (
    <Provider store={store}>
      <ConnectedEditor />
    </Provider>
  );
}
