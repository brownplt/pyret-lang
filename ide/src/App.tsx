import React from 'react';
import './App.css';
import { Interaction } from './Interaction';
import { SingleCodeMirrorDefinitions } from './SingleCodeMirrorDefinitions';
import { Menu, EMenu } from './Menu';
import { Footer } from './Footer';
import { FontSize } from './FontSize';
import { FSBrowser } from './FSBrowser';
import * as control from './control';
import SplitterLayout from 'react-splitter-layout';
import 'react-splitter-layout/lib/index.css';

control.installFileSystem();
control.loadBuiltins();

type AppProps = {};
type AppState = {};

function makeResult(result: any): { name: string, value: any }[] {
    return Object.keys(result).sort().map((key) => {
        return {
            name: key,
            value: result[key]
        }
    });
}

type EditorProps = {
    browseRoot: string;
    browsePath: string[];
    currentFileDirectory: string[];
    currentFileName: string;
};

type EditorState = {
    browseRoot: string;
    browsePath: string[];
    currentFileDirectory: string[];
    currentFileName: string;
    currentFileContents: string;
    typeCheck: boolean;
    interactions: { name: string, value: any }[];
    interactionErrors: string[];
    interactErrorExists: boolean;
    runKind: control.backend.RunKind;
    autoRun: boolean;
    updateTimer: NodeJS.Timer;
    dropdownVisible: boolean;
    fontSize: number;
    menu: EMenu;
    menuVisible: boolean;
    message: string;
    definitionsHighlights: number[][];
    fsBrowserVisible: boolean;
};

class Editor extends React.Component<EditorProps, EditorState> {
    constructor(props: EditorProps) {
        super(props);

        control.setupWorkerMessageHandler(
            console.log,
            (errors: string[]) => {
                this.setMessage("Compilation failed with error(s)")
                const places: any = [];
                for (let i = 0; i < errors.length; i++) {
                    const matches = errors[i].match(/:\d+:\d+-\d+:\d+/g);
                    if (matches !== null) {
                        matches.forEach((m) => {
                            places.push(m.match(/\d+/g)!.map(Number));
                        });
                    }
                }
                this.setState(
                    {
                        interactionErrors: errors,
                        interactErrorExists: true,
                        definitionsHighlights: places
                    }
                );
            },
            (errors: string[]) => {
                this.setState(
                    {
                        interactionErrors: [errors.toString()],
                        interactErrorExists: true,
                    }
                );
            },
            () => {
                this.setMessage("Run started");
                control.run(
                    control.path.runBase,
                    control.path.runProgram,
                    (runResult: any) => {
                        console.log(runResult);
                        if (runResult.result !== undefined) {
                            if (runResult.result.error === undefined) {
                                this.setMessage("Run completed successfully");

                                this.setState({
                                    interactions: makeResult(runResult.result)
                                });
                                if (makeResult(runResult.result)[0].name === "error") {
                                    this.setState(
                                        {
                                            interactionErrors: runResult.result.error,
                                            interactErrorExists: true
                                        }
                                    );
                                }
                            } else {
                                this.setMessage("Run failed with error(s)");

                                this.setState({
                                    interactionErrors: [runResult.result.error],
                                    interactErrorExists: true
                                });
                            }
                        }
                    },
                    this.state.runKind);
            });

        this.state = {
            browseRoot: this.props.browseRoot,
            browsePath: this.props.browsePath,
            currentFileDirectory: this.props.currentFileDirectory,
            currentFileName: this.props.currentFileName,
            currentFileContents: control.openOrCreateFile(
                control.bfsSetup.path.join(
                    ...this.props.currentFileDirectory,
                    this.props.currentFileName)),
            typeCheck: true,
            interactions: [{
                name: "Note",
                value: "Press Run to compile and run"
            }],
            interactionErrors: [],
            interactErrorExists: false,
            runKind: control.backend.RunKind.Async,
            autoRun: true,
            updateTimer: setTimeout(this.update, 2000),
            dropdownVisible: false,
            menu: EMenu.Options,
            menuVisible: false,
            fontSize: 12,
            message: "Ready to rock",
            definitionsHighlights: [],
            fsBrowserVisible: false,
        };
    };

    get isPyretFile() {
        return /\.arr$/.test(this.currentFile);
    }

    get browsePathString
    () {
        return control.bfsSetup.path.join(...this.state.browsePath);
    }

    get currentFile() {
        return control.bfsSetup.path.join(
            ...this.state.currentFileDirectory,
            this.state.currentFileName);
    }

    get currentFileName() {
        return this.state.currentFileName;
    }

    get currentFileDirectory() {
        return control.bfsSetup.path.join(...this.state.currentFileDirectory);
    }

    get browsingRoot() {
        return control.bfsSetup.path.join(...this.state.browsePath) ===
            this.state.browseRoot;
    }

    run = () => {
        this.setState(
            {
                interactionErrors: [],
                interactErrorExists: false
            }
        );
        if (this.isPyretFile) {
            this.setMessage("Compilation started");
            control.compile(
                this.currentFileDirectory,
                this.currentFileName,
                this.state.typeCheck);
        } else {
            this.setMessage("Visited a non-pyret file");
            this.setState({
                interactions: [
                    {
                        name: "Error",
                        value: "Run is not supported on this file type"
                    },
                    {
                        name: "File",
                        value: this.currentFile
                    }],
                interactionErrors: ["Error: Run is not supported on this file type"],
                interactErrorExists: true
            });
        }
    };

    update = (): void => {
        control.fs.writeFileSync(
            this.currentFile,
            this.state.currentFileContents);
        if (this.state.autoRun) {
            this.run();
        }
    }

    onEdit = (value: string): void => {
        clearTimeout(this.state.updateTimer);
        this.setState({
            currentFileContents: value,
            updateTimer: setTimeout(this.update, 250),
        });
    }

    onTraverseDown = (path: string[]) => {
        this.setState({
            browsePath: path,
        });
    };

    onTraverseUp = (path: string[]) => {
        this.setState({
            browsePath: path,
        });
    };

    onExpandChild = (child: string, fullChildPath: string): void => {
        this.setState({
            interactions: [{
                name: "Note",
                value: "Press Run to compile and run"
            }],
            currentFileDirectory: this.state.browsePath,
            currentFileName: child,
            currentFileContents: control.fs
                                        .readFileSync(fullChildPath, "utf-8"),
        });
    };

    toggleFSBrowser = () => {
        if (this.state.menu === EMenu.FSBrowser) {
            this.setState({
                menuVisible: !this.state.menuVisible,
            });
        } else if (this.state.menu === EMenu.Options) {
            this.setState({
                menu: EMenu.FSBrowser,
                menuVisible: true,
            });
        }
    };

    loadBuiltins = (e: React.MouseEvent<HTMLElement>): void => {
        control.loadBuiltins();
    };

    removeRootDirectory = (e: React.MouseEvent<HTMLElement>): void => {
        control.removeRootDirectory();
    };

    makeHeaderButton = (text: string, enabled: boolean, onClick: () => void) => {
        return (
            <button className={(enabled ? "run-option-enabled" : "run-option-disabled")}
                    onClick={onClick}>
                {text}
            </button>
        );
    };

    makeDropdownOption = (text: string, enabled: boolean, onClick: () => void) => {
        return (
            <div className={enabled ? "run-option-enabled" : "run-option-disabled"}
                 onClick={onClick}>
                <input type="checkBox"
                       checked={enabled}
                       name={text}
                       className="run-option-checkbox"
                       readOnly={true}>
                </input>
                <label htmlFor={text}
                       className="run-option-label">
                    {text}
                </label>
            </div>
        );
    };

    toggleDropdownVisibility = (e: any) => {
        this.setState({
            dropdownVisible: !this.state.dropdownVisible
        });
    };

    toggleAutoRun = () => {
        this.setState({
            autoRun: !this.state.autoRun
        });
    };

    toggleStopify = () => {
        if (this.state.runKind === control.backend.RunKind.Async) {
            this.setState({
                runKind: control.backend.RunKind.Sync
            });
        } else {
            this.setState({
                runKind: control.backend.RunKind.Async
            })
        }
    };

    toggleTypeCheck = () => {
        this.setState({
            typeCheck: !this.state.typeCheck
        });
    };

    toggleOptionsVisibility = () => {
        if (this.state.menu === EMenu.Options) {
            this.setState({
                menuVisible: !this.state.menuVisible,
            });
        } else if (this.state.menu === EMenu.FSBrowser) {
            this.setState({
                menu: EMenu.Options,
                menuVisible: true,
            });
        }
    };

    onDecreaseFontSize = () => {
        if (this.state.fontSize > 1) {
            this.setState({
                fontSize: this.state.fontSize - 1
            });
        }
    };

    onIncreaseFontSize = () => {
        this.setState({
            fontSize: this.state.fontSize + 1
        });
    };

    onResetFontSize = () => {
        this.setState({
            fontSize: 12
        });
    };

    removeDropdown = () => {
        this.setState({
            dropdownVisible: false
        });
    };

    setMessage = (newMessage: string) => {
        this.setState({
            message: newMessage
        });
    };

    render() {
        const interactionValues =
            <pre className="interactions-area"
                 style={{ fontSize: this.state.fontSize }}>
                {
                    this.state.interactions.map(
                        (i) => {
                            return <Interaction key={i.name}
                                                name={i.name}
                                                value={i.value}
                                                setMessage={this.setMessage} />
                        })
                }
            </pre>;
        const definitions = <SingleCodeMirrorDefinitions
                                text={this.state.currentFileContents}
                                onEdit={this.onEdit}
                                highlights={this.state.definitionsHighlights}
                                interactErrorExists={this.state.interactErrorExists}>
        </SingleCodeMirrorDefinitions>;
        return (
            <div className="page-container">
                <div className="header-container">
                    <button className="menu"
                            onClick={this.toggleOptionsVisibility}>
                        Options
                    </button>
                    <button className="menu"
                            onClick={this.toggleFSBrowser}>
                        Files
                    </button>
                    {this.state.runKind === control.backend.RunKind.Async ? (
                        <button className="stop-available">
                            Stop
                        </button>
                    ) : (
                        <button className="stop-unavailable">
                            Stop
                        </button>
                    )}
                    <div className="run-container">
                        <button className="run-ready"
                                onClick={this.run}>
                            Run
                        </button>
                        <button className="run-options"
                                onClick={this.toggleDropdownVisibility}
                                onBlur={this.removeDropdown}>&#8628;{
                                    this.state.dropdownVisible && (
                                        <div className="run-dropdown"
                                             onClick={(e) => e.stopPropagation()}>
                                            {this.makeDropdownOption("Auto Run", this.state.autoRun, this.toggleAutoRun)}
                                            {this.makeDropdownOption("Stopify", this.state.runKind === control.backend.RunKind.Async, this.toggleStopify)}
                                            {this.makeDropdownOption("Type Check", this.state.typeCheck, this.toggleTypeCheck)}
                                        </div>
                                    )}
                        </button>
                    </div>
                </div>
                <div className="code-container">
                    {this.state.menuVisible && (
                        <Menu menu={this.state.menu}
                              tabs={[
                                  [
                                      <FSBrowser
                                          browsingRoot={this.browsingRoot}
                                          onTraverseUp={this.onTraverseUp}
                                          onTraverseDown={this.onTraverseDown}
                                          onExpandChild={this.onExpandChild}
                                          browsePath={this.state.browsePath}>
                                      </FSBrowser>
                                  ],
                                  [
                                      <FontSize onIncrease={this.onIncreaseFontSize}
                                                onDecrease={this.onDecreaseFontSize}
                                                onReset={this.onResetFontSize}
                                                size={this.state.fontSize}
                                                key="FontSize">
                                      </FontSize>
                                  ]
                              ]}
                              currentTab={this.state.menu}>
                        </Menu>)}
                    <SplitterLayout vertical={false}
                                    percentage={true}>
                        <div className="edit-area-container"
                             style={{ fontSize: this.state.fontSize }}>
                            {definitions}
                        </div>
                        <div className="interactions-area-container">
                            {this.state.interactErrorExists ? (
                                <SplitterLayout vertical={true}>
                                    {interactionValues}
                                    <div className="interaction-error">
                                        <p style={{ fontSize: this.state.fontSize }}>
                                            {this.state.interactionErrors}
                                        </p>
                                    </div>
                                </SplitterLayout>
                            ) : interactionValues}
                        </div>
                    </SplitterLayout>
                </div>
                <Footer message={this.state.message}></Footer>
            </div>
        );
    }
}

class App extends React.Component<AppProps, AppState> {
    render() {
        return (
            <Editor browseRoot="/"
                    browsePath={["/", "projects"]}
                    currentFileDirectory={["/", "projects"]}
                    currentFileName="program.arr">
            </Editor>
        );
    };
}

export default App;
