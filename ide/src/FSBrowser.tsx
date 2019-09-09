import React from 'react';
import * as control from './control';

type FSItemProps = {
    onClick: () => void;
    path: string[];
    selected: boolean;
};

type FSItemState = {};

class FSItem extends React.Component<FSItemProps, FSItemState> {
    render() {
        const path = control.bfsSetup.path.join(...this.props.path);

        const stats = control.fs.statSync(path);

        const label = (() => {
            if (stats.isDirectory()) {
                return "D";
            } else if (stats.isFile()) {
                return "F";
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
                        {this.props.path[this.props.path.length - 1]}
                    </div>
                </div>
            </button>
        );
    }
}

type FSBrowserProps = {
    root: string,
    onTraverseUp: (path: string[]) => void,
    onTraverseDown: (path: string[]) => void,
    onExpandChild: (child: string, fullChildPath: string) => void,
    browsePath: string[],
};

enum EditType {
    CreateFile,
    CreateDirectory
}

type FSBrowserState = {
    editType: EditType | undefined,
    editValue: string,
    selected: string | undefined,
};

export class FSBrowser extends React.Component<FSBrowserProps, FSBrowserState> {
    constructor(props: FSBrowserProps) {
        super(props);

        this.state = {
            editType: undefined,
            editValue: "",
            selected: undefined,
        };
    }

    get browsePathString() {
        return control.bfsSetup.path.join(...this.props.browsePath);
    }

    get browsingRoot() {
        return control.bfsSetup.path.join(...this.props.browsePath) ===
            this.props.root;
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
        const newPath = this.props.browsePath.slice();
        newPath.pop();

        this.setState({
            selected: undefined,
        });

        this.props.onTraverseUp(newPath);
    };

    traverseDown = (childDirectory: string): void => {
        const newPath = this.props.browsePath.slice();
        newPath.push(childDirectory);

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

            this.props.onExpandChild(child, fullChildPath);
        }
    }

    createFSItemPair = (filePath: string): [string, any] => {
        return [
            filePath,
            <FSItem key={filePath}
                    onClick={() => this.expandChild(filePath)}
                    path={[...this.props.browsePath, filePath]}
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
        const path = control.bfsSetup.path.join(...this.props.browsePath, name);

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
                control.bfsSetup.path.join(...this.props.browsePath, this.state.selected))

            this.setState({
                selected: undefined,
            });
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
                           {this.state.editType === EditType.CreateFile ? "F" : "D"}
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
                <div className="fs-browser-item"
                     style={{
                         display: "flex",
                         flexDirection: "row",
                         height: "auto",
                     }}>
                    <div style={{
                        fontFamily: "monospace",
                        display: "flex",
                        alignItems: "center",
                        paddingLeft: "1em",
                    }}>
                        {this.props.browsePath[this.props.browsePath.length - 1]}
                    </div>
                    <div style={{
                        flexGrow: 1,
                        display: "flex",
                        flexDirection: "row",
                        justifyContent: "flex-end",
                    }}>
                        <button className="fs-browser-item"
                                onClick={this.toggleEditFile}>
                            +F
                        </button>
                        <button className="fs-browser-item"
                                onClick={this.toggleEditDirectory}>
                            +D
                        </button>
                        <button className="fs-browser-item"
                                onClick={this.deleteSelected}>
                            X
                        </button>
                    </div>
                </div>
                {editor}
                {!this.browsingRoot && (
                    <FSItem onClick={this.traverseUp}
                            path={[".."]}
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
        );
    }
}
