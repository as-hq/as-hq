import React from 'react';
import ReactDOM from 'react-dom';
import {Styles, FontIcon} from 'material-ui';

import ToolbarController from './ToolbarController.jsx';
import ToolbarTextField from './ToolbarTextField.jsx';
import GenerateToolbarMenu from './GenerateToolbarMenu.jsx';


export default React.createClass({

  /*************************************************************************************************************************/
  // Sub-component generation

  fontSizes: [6, 7, 8, 9, 10, 11, 12, 14, 18, 24, 36],

  getMenuProps() {
    return this.fontSizes.map((size) => {
      return {tag: 'MenuItem', primaryText: `${size}`, value: `${size}`};
    });
  },

  toolbarControlProps() {
    return {
      tooltip: "Font size",
      displayValue: '10',
      showTooltip: true,
      width: 45
    };
  },

  /*************************************************************************************************************************/
  // Helper methods to pass to generator

  // When the active cell changes to a new cell, get the new menu value that should be selected/checked 
  _getMenuValueFromCell(cell) {
    return "10"; // TODO: something like cell.cellProps.font.fontSize
  },

  _propagateControlStateChange(nextValue, rng) {
    // TODO: case on value, call some API function
    console.log("Propagating state change to backend: " + nextValue);
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
        initialValue="10"
        menuWidth={65} 
        toolbarControlWidth={45}
        id="FontSizePicker" />
    );
  }

});

