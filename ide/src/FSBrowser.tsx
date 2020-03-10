import React from 'react';
import * as control from './control';
import { connect, ConnectedProps } from 'react-redux';

type FSItemProps = {
    onClick: () => void;
    path: string;
    selected: boolean;
};

type FSItemState = {};

class FSItem extends React.Component<FSItemProps, FSItemState> {
    render() {
        const path = this.props.path;

        const stats = control.fs.statSync(path);

        const label = (() => {
            if (stats.isDirectory()) {
                return <div>&#x1f4c2;</div>;
            } else if (stats.isFile()) {
                return <div>&#128441;</div>;
            } else {
                return "?";
        }})();

        const background = this.props.selected ? "darkgray": "rgba(0, 0, 0, 0.3)";

        return (
            <button onClick={this.props.onClick}
                    style={{
                        background: background,
                        border: 0,
                        height: "2.7em",
                        color: "#fff",
                        textAlign: "left",
                        flex: "none",
                        cursor: "pointer",
                    }}>
                <div style={{
                    display: "flex",
                    flexDirection: "row",
                }}>
                    <div style={{
                        width: "1em",
                        paddingRight: "1em",
                    }}>
                        {label}
                    </div>
                    <div>
                        {control.bfsSetup.path.parse(this.props.path).base}
                    </div>
                </div>
            </button>
        );
    }
}

type stateProps = {
    browseRoot: string,
    browsePath: string
};

function mapStateToProps(state: any): stateProps {
    return {
        browseRoot: state.browseRoot,
        browsePath: state.browsePath
    };
}

type dispatchProps = {
    onTraverseUp: (path: string) => void,
    onTraverseDown: (path: string) => void,
    onExpandChild: (path: string) => void,
}

function mapDispatchToProps(dispatch: any): dispatchProps {
    return {
        onTraverseUp: (path: string) => dispatch({type: "traverseUp", path}),
        onTraverseDown: (path: string) => dispatch({type: "traverseDown", path}),
        onExpandChild: (path: string) => dispatch({
            type: "expandChild",
            path
        })
    };
}

const connector = connect(mapStateToProps, mapDispatchToProps);

type PropsFromRedux = ConnectedProps<typeof connector>;
type FSBrowserProps = PropsFromRedux & dispatchProps & stateProps;

enum EditType {
    CreateFile,
    CreateDirectory
}

type FSBrowserState = {
    editType: EditType | undefined,
    editValue: string,
    selected: string | undefined,
};

class FSBrowser extends React.Component<FSBrowserProps, FSBrowserState> {
    constructor(props: FSBrowserProps) {
        super(props);

        this.state = {
            editType: undefined,
            editValue: "",
            selected: undefined,
        };
    }

    get browsePathString() {
        return this.props.browsePath;
    }

    get browsingRoot() {
        return this.props.browsePath === this.props.browseRoot;
    }

    static compareFSItemPair =
        (a: [string, FSItem],
         b: [string, FSItem]): any => {
             if (a[0] < b[0]) {
                 return -1;
             } else if (a[0] > b[0]) {
                 return 1;
             } else {
                 return 0;
             }
         };

    traverseUp = (): void => {
        const newPath = control.bfsSetup.path.join(this.props.browsePath, "..");

        this.setState({
            selected: undefined,
        });

        this.props.onTraverseUp(newPath);
    };

    traverseDown = (childDirectory: string): void => {
        const newPath = control.bfsSetup.path.join(this.props.browsePath, childDirectory);

        this.setState({
            selected: undefined,
        });

        this.props.onTraverseDown(newPath);
    };

    expandChild = (child: string): void => {
        const fullChildPath =
            control.bfsSetup.path.join(this.browsePathString, child);
        const stats = control.fs.statSync(fullChildPath);

        if (stats.isDirectory()) {
            this.traverseDown(child);
        } else if (stats.isFile()) {
            this.setState({
                selected: child,
            });

            this.props.onExpandChild(fullChildPath);
        }
    }

    createFSItemPair = (filePath: string): [string, any] => {
        return [
            filePath,
            <FSItem key={filePath}
                    onClick={() => this.expandChild(filePath)}
                    path={control.bfsSetup.path.join(this.props.browsePath, filePath)}
                    selected={filePath === this.state.selected}/>
        ];
    };

    toggleEditFile = (): void => {
        if (this.state.editType === EditType.CreateFile) {
            this.setState({
                editType: undefined,
            });
        } else {
            this.setState({
                editType: EditType.CreateFile,
            });
        }
    };

    toggleEditDirectory = (): void => {
        if (this.state.editType === EditType.CreateDirectory) {
            this.setState({
                editType: undefined,
            });
        } else {
            this.setState({
                editType: EditType.CreateDirectory,
            });
        }
    };

    handleSubmit = (value: React.SyntheticEvent): void => {
        value.preventDefault();

        const name = this.state.editValue;
        const path = control.bfsSetup.path.join(this.props.browsePath, name);

        if (this.state.editType === EditType.CreateFile) {
            control.createFile(path);
        } else {
            control.createDirectory(path);
        }

        this.setState({
            editType: undefined,
            editValue: "",
        });
    };

    onChange = (event: React.SyntheticEvent): void => {
        this.setState({
            editValue: (event.target as HTMLInputElement).value,
        });
    };

    deleteSelected = (): void => {
        if (this.state.selected === undefined) {
            control.removeDirectory(this.browsePathString);

            this.traverseUp();
        } else {
            control.removeFile(
                control.bfsSetup.path.join(this.props.browsePath, this.state.selected))

            this.setState({
                selected: undefined,
            });
        }
    };

    selectCurrentDirectory = (): void => {
        this.setState({
            selected: undefined,
        });
    };

    uploadFile = (event: any): void => {
        const currentDirectory = this.props.browsePath;
        const file = event.target.files[0];

        if (file) {
            const reader = new FileReader();

            reader.onload = (e: any) => {
                const data = e.target.result;
                const name = file.name;

                control.bfsSetup.fs.writeFileSync(
                    control.bfsSetup.path.join(currentDirectory, name),
                    data);

                this.setState(this.state);
            };

            reader.readAsText(file);
        }
    };

    render() {
        const editor = this.state.editType !== undefined &&
                       <div style={{
                           display: "flex",
                           flexDirection: "row",
                           alignItems: "center",
                       }}>
                           <pre style={{
                               paddingLeft: "1em",
                               paddingRight: "1em",
                           }}>
                               {this.state.editType === EditType.CreateFile ? (
                                   <div>&#128441; Name</div>
                               ) : (
                                   <div>&#x1f4c2; Name</div>
                               )}
                           </pre>
                           <form onSubmit={this.handleSubmit}
                                 style={{
                                     height: "100%",
                                     flexGrow: 1,
                                 }}>
                               <input type="text"
                                      value={this.state.editValue}
                                      onChange={this.onChange}
                                      style={{
                                          border: 0,
                                          padding: 0,
                                          width: "100%",
                                          height: "100%",
                                      }}>
                               </input>
                           </form>
                       </div>;

        return (
            <div style={{display: "flex", flexDirection: "column"}}>
                <div style={{display: "flex", flexDirection: "column"}}>
                    <div className="fs-browser-item"
                         style={{
                             display: "flex",
                             flexDirection: "row",
                             height: "auto",
                         }}>
                        <div onClick={this.selectCurrentDirectory}
                             style={{
                                 cursor: "pointer",
                                 fontFamily: "monospace",
                                 display: "flex",
                                 alignItems: "center",
                                 paddingLeft: "1em",
                                 paddingRight: "1em",
                                 background: this.state.selected ? "none" : "darkgray",
                             }}>
                            {control.bfsSetup.path.parse(this.props.browsePath).base || "/"}
                        </div>
                        <div style={{
                            flexGrow: 1,
                            display: "flex",
                            flexDirection: "row",
                            justifyContent: "flex-end",
                        }}>
                            <label className="fs-browser-item"
                                   style={{
                                       width: "2.3em",
                                       height: "100%",
                                       display: "flex",
                                       justifyContent: "center",
                                       alignContent: "center",
                                       alignItems: "center",
                                   }}>
                                <input type="file"
                                       onChange={this.uploadFile}
                                       style={{
                                           display: "none",
                                       }}>
                                </input>&#11193;
                            </label>
                            <button className="fs-browser-item"
                                    onClick={this.toggleEditFile}>
                                +&#128441;
                            </button>
                            <button className="fs-browser-item"
                                    onClick={this.toggleEditDirectory}>
                                +&#x1f4c2;
                            </button>
                            {!this.browsingRoot &&
                             <button className="fs-browser-item"
                                     onClick={this.deleteSelected}>
                            &#10060;
                             </button>}
                        </div>
                    </div>
                    {editor}
                    {!this.browsingRoot && (
                        <FSItem onClick={this.traverseUp}
                                path={".."}
                                selected={false}>
                        </FSItem>
                    )}
                    {
                        control.fs
                               .readdirSync(this.browsePathString)
                               .map(this.createFSItemPair)
                               .sort(FSBrowser.compareFSItemPair)
                               .map((x: [string, FSItem]) => x[1])
                    }
                </div>
            </div>
        );
    }
}

export default connector(FSBrowser);
