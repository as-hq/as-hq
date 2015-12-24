import React from 'react';
import ReactDOM from 'react-dom';
import {Styles, FontIcon} from 'material-ui';

import ToolbarController from './ToolbarController.jsx';
import ToolbarTextField from './ToolbarTextField.jsx';
import GenerateToolbarMenu from './GenerateToolbarMenu.jsx';
import ExpStore from '../../stores/ASExpStore';
import ExpActionCreator from '../../actions/ASExpActionCreators.js';


export default React.createClass({

  /*************************************************************************************************************************/
  // Sub-component generation

  getInitialState() {
    return  {
      language: ExpStore.getDefaultLanguage()
    }
  },

  languages: [
    {name: 'Excel', shortcut: 'Ctrl+1'},
    {name: 'Python', shortcut: 'Ctrl+2'},
    {name: 'SQL', shortcut: 'Ctrl+3'},
    {name: 'R', shortcut: 'Ctrl+4'},
  ],

  // Return a bunch of menu items
  getMenuProps() {
    let menuItems = this.languages.map((lang) => {
      return {tag: 'MenuItem', primaryText: lang.name, secondaryText: lang.shortcut, value: lang.name};
    });
    return menuItems;
  },

  toolbarControlProps() {
    return {
      displayValue: this.state.language,
      tooltip: "Languages",
      showTooltip: true,
      width: 85
    };
  },

  /*************************************************************************************************************************/
  // Mounting (we listen to toggle language changes from the ExpStore, and rerender if the language changed)
  // This happens when user presses Ctrl + 1, etc; toggles the  language from an external source

  componentDidMount() {
    ExpStore.addChangeListener(this._onToggleLanguage);
  },

  componentWillUnmount() {
    ExpStore.removeChangeListener(this._onToggleLanguage);
  },

  _onToggleLanguage() {
    if (ExpStore.getLanguage() !== this.state.language) {
      this.setState({language: ExpStore.getLanguage()});
    }
  },

  /*************************************************************************************************************************/
  // Helper methods to pass to generator

  // When the active cell changes to a new cell, get the new menu value that should be selected/checked 
  _getMenuValueFromCell(cell) {
    console.log("Language picker cell ", cell)
    if (cell == null) {
      return ExpStore.getDefaultLanguage();
    } else {
      return cell.cellExpression.language;
    }
  },

  _propagateControlStateChange(nextValue, rng) {
    console.log("Propagating language change: " + nextValue);
    ExpActionCreator.handleToggleLanguage(nextValue);
  },

  // Update the toolbar control props given a the menu visibility, menuValue, and current toolbarProps.
  _toolbarControlPropTransform(menuVisible, menuValue, toolbarControlProps) {
    toolbarControlProps.showTooltip = !menuVisible;
    toolbarControlProps.displayValue = menuValue;
    return toolbarControlProps;
  },

  /*************************************************************************************************************************/
  //Render

  render() {
    let ButtonWithMenu = GenerateToolbarMenu(ToolbarTextField);
    return (
      <ButtonWithMenu
        toolbarControlProps={this.toolbarControlProps()}
        menuProps={this.getMenuProps()}
        getMenuValueFromCell={this._getMenuValueFromCell}
        toolbarControlPropTransform={this._toolbarControlPropTransform}
        propagateControlStateChange={this._propagateControlStateChange}
        initialValue={this.state.language}
        menuWidth={65} 
        toolbarControlWidth={85}
        id="LanguagePicker" />
    );
  }

});


