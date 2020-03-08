import { createStore } from 'redux';
import { ideApp } from './reducers';

export const store = createStore(ideApp);
