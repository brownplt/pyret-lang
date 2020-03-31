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
