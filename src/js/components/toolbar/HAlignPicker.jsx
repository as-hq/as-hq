import React from 'react';
import ReactDOM from 'react-dom';
import {Styles, FontIcon} from 'material-ui';

import ToolbarButton from './ToolbarButton.jsx';
import GenerateToolbarMenu from './GenerateToolbarMenu.jsx';

export default React.createClass({

  /*************************************************************************************************************************/
  // Sub-component generation

  // Return a bunch of menu items
  getMenuProps() {
    let genIcon = (iconName) => {
      return <FontIcon 
        style={{backgroundColor: Styles.Colors.grey50, color: Styles.Colors.grey800}} 
        className="material-icons"> 
          {iconName} 
        </FontIcon>;
    };
    let menuItems = [
      {tag: 'MenuItem', primaryText: 'Left', value: 'left', rightIcon: genIcon('format_align_left')},
      {tag: 'MenuItem', primaryText: 'Center', value: 'center', rightIcon: genIcon('format_align_center')},
      {tag: 'MenuItem', primaryText: 'Right', value: 'right', rightIcon: genIcon('format_align_right')},
    ];
    return menuItems;
  },

  toolbarControlProps() {
    return {
      iconName: 'format_align_left',
      tooltip: 'Horizontal align',
      usePushState: false,
      includeDropdownArrow: true,
      showTooltip: true
    };
  },

  /*************************************************************************************************************************/
  // Helper methods to pass to generator

  // When the active cell changes to a new cell, get the new menu value that should be selected/checked 
  _getMenuValueFromCell(cell) {
    console.log("\n\n\nHAlign picker cell ", cell)
    return 'left'; // TODO: eventually cell.cellProps.halign
  },

  _propagateControlStateChange(nextValue, rng) {
    console.log("Propagating language change: " + nextValue);
    return; // TODO: eventually some API call
  },

  // Update the toolbar control props given a the menu visibility, menuValue, and current toolbarProps.
  // In this case, we want the icon to reflect the menu choice.
  _toolbarControlPropTransform(menuVisible, menuValue, toolbarControlProps) {
    toolbarControlProps.showTooltip = !menuVisible;
    toolbarControlProps.iconName = 'format_align_' + menuValue;
    return toolbarControlProps;
  },

  /*************************************************************************************************************************/
  //Render

  render() {
    let ButtonWithMenu = GenerateToolbarMenu(ToolbarButton);
    return (
      <ButtonWithMenu
        toolbarControlProps={this.toolbarControlProps()}
        menuProps={this.getMenuProps()}
        getMenuValueFromCell={this._getMenuValueFromCell}
        toolbarControlPropTransform={this._toolbarControlPropTransform}
        propagateControlStateChange={this._propagateControlStateChange}
        initialValue={'left'}
        menuWidth={65} 
        id="HAlignPicker" />
    );
  }

});


