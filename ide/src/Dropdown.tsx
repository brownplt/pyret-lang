import React from 'react';

export type DropdownOptionProps = {
    enabled: boolean,
    onClick: () => void,
    text: string,
};

export type DropdownOptionState = {};

export class DropdownOption extends React.Component<DropdownOptionProps, DropdownOptionState> {
    render() {
        return (
            <div className={this.props.enabled ? "run-option-enabled" : "run-option-disabled"}
                 onClick={this.props.onClick}>
                <input type="checkBox"
                       checked={this.props.enabled}
                       name={this.props.text}
                       className="run-option-checkbox"
                       readOnly={true}>
                </input>
                <label htmlFor={this.props.text}
                       className="run-option-label">
                    {this.props.text}
                </label>
            </div>
        );
    }
}

export type DropdownProps = {};

export type DropdownState = {};

export class Dropdown extends React.Component<DropdownProps, DropdownState> {
    render() {
        return (
            <div className="run-dropdown"
                 onClick={(e) => e.stopPropagation()}>
                {this.props.children}
            </div>
        );
    }
}
