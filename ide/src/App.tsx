import React from 'react';
import { Provider } from 'react-redux';
import Editor from './Editor';
import { store } from './store';

type AppProps = {};
type AppState = {};

class App extends React.Component<any, any> {
    render() {
        return (
            <Provider store={store}>
                <Editor/>
            </Provider>
        );
    };
}

export default App;
