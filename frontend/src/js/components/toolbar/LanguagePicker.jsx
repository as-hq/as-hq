/* @flow */

import React from 'react';
import ReactDOM from 'react-dom';
import {Styles, FontIcon} from 'material-ui';

import ASCell from '../../classes/ASCell';
import ASRange from '../../classes/ASRange';

import ToolbarController from './ToolbarController.jsx';
import ToolbarTextField from './ToolbarTextField.jsx';
import GenerateToolbarMenu from './GenerateToolbarMenu.jsx';
import ExpStore from '../../stores/ASExpStore';
import ExpActionCreator from '../../actions/ASExpActionCreators.js';

import type {
  MenuProps,
  ToolbarControlProps
} from '../../types/Toolbar';

import type {
  ASLanguage
} from '../../types/Eval';

type LanguageMenuOption = {
  name: string;
  shortcut: string;
};

type LanguagePickerProps = {
};

type LanguagePickerDefaultProps = {
};

type LanguagePickerState = {
  language: ASLanguage;
};

export default class LanguagePicker
  extends React.Component<LanguagePickerDefaultProps, LanguagePickerProps, LanguagePickerState>
{
  /*************************************************************************************************************************/
  // Sub-component generation

  languages: Array<LanguageMenuOption>;

  constructor(props: LanguagePickerProps) {
    super(props);

    this.state = {
      language: ExpStore.getDefaultLanguage()
    }

    this.languages = [
      {name: 'Excel', shortcut: 'Ctrl+1'},
      {name: 'Python', shortcut: 'Ctrl+2'},
      {name: 'R', shortcut: 'Ctrl+3'},
      {name: 'SQL', shortcut: 'Ctrl+4'}
    ];
  }

  // Return a bunch of menu items
  getMenuProps(): Array<MenuProps> {
    let menuItems = this.languages.map((lang) => {
      return {tag: 'MenuItem', primaryText: lang.name, secondaryText: lang.shortcut, value: lang.name};
    });
    return menuItems;
  }

  toolbarControlProps(): ToolbarControlProps {
    return {
      displayValue: this.state.language,
      tooltip: "Languages",
      showTooltip: true,
      width: 85
    };
  }

  /*************************************************************************************************************************/
  // Mounting (we listen to toggle language changes from the ExpStore, and rerender if the language changed)
  // This happens when user presses Ctrl + 1, etc; toggles the  language from an external source

  componentDidMount() {
    ExpStore.addChangeListener(this._onToggleLanguage.bind(this));
  }

  componentWillUnmount() {
    ExpStore.removeChangeListener(this._onToggleLanguage.bind(this));
  }

  _onToggleLanguage() {
    if (ExpStore.getLanguage() !== this.state.language) {
      this.setState({language: ExpStore.getLanguage()});
    }
  }

  /*************************************************************************************************************************/
  // Helper methods to pass to generator

  // When the active cell changes to a new cell, get the new menu value that should be selected/checked
  _getMenuValueFromCell(cell: ASCell): ASLanguage {
    console.log("Language picker cell ", cell)
    if (cell != null && cell.expression.language != null) { // #cellrefactor ()
      return cell.expression.language;
    } else {
      return ExpStore.getDefaultLanguage();
    }
  }

  _propagateControlStateChange(nextValue: string, rng: ASRange) {
    console.log("Propagating language change: " + nextValue);
    ExpActionCreator.handleToggleLanguage(nextValue);
  }

  // Update the toolbar control props given a the menu visibility, menuValue, and current toolbarProps.
  _toolbarControlPropTransform(menuVisible: boolean, menuValue: string, toolbarControlProps: ToolbarControlProps): ToolbarControlProps {
    toolbarControlProps.showTooltip = !menuVisible;
    toolbarControlProps.displayValue = menuValue;
    return toolbarControlProps;
  }

  /*************************************************************************************************************************/
  //Render

  render(): React.Element {
    let ButtonWithMenu = GenerateToolbarMenu(ToolbarTextField);
    return (
      <ButtonWithMenu
        toolbarControlProps={this.toolbarControlProps()}
        menuProps={this.getMenuProps()}
        getMenuValueFromCell={this._getMenuValueFromCell.bind(this)}
        toolbarControlPropTransform={this._toolbarControlPropTransform.bind(this)}
        propagateControlStateChange={this._propagateControlStateChange.bind(this)}
        initialValue={this.state.language}
        menuWidth={65}
        toolbarControlWidth={85}
        id="LanguagePicker" />
    );
  }

}
