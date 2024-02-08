import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import './index.css';
import App from './App';
import * as serviceWorker from './serviceWorker';
import EmbedableEditor from './EmbedableEditor';
import store from './store';
import { emptyChunk } from './chunk';
import { Action } from './action';



export function startApp() {
  ReactDOM.render(<App />, document.getElementById('root'));
  // If you want your app to work offline and load faster, you can change
  // unregister() to register() below. Note this comes with some pitfalls.
  // Learn more about service workers: https://bit.ly/CRA-PWA
  serviceWorker.unregister();
}

export function renderParley(container : HTMLElement) {
  ReactDOM.render(<Provider store={store}><EmbedableEditor /></Provider>, container);
  return {
    getChunks: () => store.getState().chunks,
    addChunk: (chunkText : string) => {
      const index = store.getState().chunks.length;
      store.dispatch({ type: 'chunk', key: 'insert', index, text: chunkText });
    },
    resetChunks: (texts: string[]) => {
      const chunks = texts.map((t) => emptyChunk({}, t));
      const action : Action = {
        type: 'update',
        key: 'chunks',
        value: { chunks, modifiesText: true },
      };
      store.dispatch(action);
    },
    run: () => store.dispatch({ type: 'run', key: 'runSegments' }),
    onReady: (cb : () => void) => {
      store.dispatch({
        type: 'update',
        key: 'addReadyCallback',
        value: cb,
      });
    },
  };
}
