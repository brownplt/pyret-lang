import React from 'react';
import { Provider } from 'react-redux';
import { Editor } from './Editor';
import { store } from './store';

type AppProps = {};
type AppState = {};

class App extends React.Component<AppProps, AppState> {
    render() {
        return (
            <Provider store={store}>
                <Editor browseRoot="/"
                        browsePath={["/", "projects"]}
                        currentFileDirectory={["/", "projects"]}
                        currentFileName="program.arr">
                </Editor>
            </Provider>
        );
    };
}

export default App;
