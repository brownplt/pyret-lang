import React from 'react';
import { Provider } from 'react-redux';
import { Editor } from './Editor';
import { store } from './store';

export default function App() {
  return (
    <Provider store={store}>
      <Editor />
    </Provider>
  );
}
