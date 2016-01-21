/* @flow */

import type {
  StoreLink
} from '../../types/React';

import React from 'react';
import ReactDOM from 'react-dom';
import {Styles, FontIcon} from 'material-ui';

import API from '../../actions/ASApiActionCreators';
import Util from '../../AS/Util';

import ASCell from '../../classes/ASCell';
import ASRange from '../../classes/ASRange';

import ToolbarController from './ToolbarController.jsx';
import ToolbarTextField from './ToolbarTextField.jsx';
import GenerateToolbarMenu from './GenerateToolbarMenu.jsx';

import ExpStore from '../../stores/ASExpStore';
import SelectionStore from '../../stores/ASSelectionStore';
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

export default class LanguagePicker
  extends React.Component<LanguagePickerDefaultProps, LanguagePickerProps, {}>
{
  /*************************************************************************************************************************/
  // Sub-component generation

  $storeLinks: Array<StoreLink>;
  languages: Array<LanguageMenuOption>;

  constructor(props: LanguagePickerProps) {
    super(props);

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
      displayValue: ExpStore.getLanguage(),
      tooltip: "Languages",
      showTooltip: true,
      width: 85
    };
  }

  /*************************************************************************************************************************/
  // Mounting (we listen to toggle language changes from the ExpStore, and rerender if the language changed)
  // This happens when user presses Ctrl + 1, etc; toggles the  language from an external source

  componentDidMount() {
    Util.React.addStoreLinks(this, [ // this forceUpdates every time ExpStore updates but this is fast enough.
      { store: ExpStore }
    ]);
  }

  componentWillUnmount() {
    Util.React.removeStoreLinks(this);
  }

  /*************************************************************************************************************************/
  // Helper methods to pass to generator

  // When the active cell changes to a new cell, get the new menu value that should be selected/checked
  _getMenuValueFromCell(cell: ASCell): ASLanguage {
    if (cell != null && cell.expression.language != null) { // #cellrefactor ()
      return cell.expression.language;
    } else {
      return ExpStore.getDefaultLanguage();
    }
  }

  _propagateControlStateChange(nextValue: string, rng: ASRange) {
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
        initialValue={ExpStore.getLanguage()}
        menuWidth={65}
        toolbarControlWidth={85}
        id="LanguagePicker" />
    );
  }

}
