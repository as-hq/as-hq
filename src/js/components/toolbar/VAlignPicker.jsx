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
        style={{backgroundColor: Styles.Colors.grey400, color: Styles.Colors.grey800}} 
        className="material-icons"> 
          {iconName} 
        </FontIcon>;
    };
    let menuItems = [
      {tag: 'MenuItem', primaryText: 'Top', value: 'top', rightIcon: genIcon('vertical_align_top')},
      {tag: 'MenuItem', primaryText: 'Middle', value: 'center', rightIcon: genIcon('vertical_align_center')},
      {tag: 'MenuItem', primaryText: 'Bottom', value: 'bottom', rightIcon: genIcon('vertical_align_bottom')},
    ];
    return menuItems;
  },

  toolbarControlProps() {
    return {
      iconName: 'vertical_align_bottom',
      tooltip: 'Vertical align',
      includeDropdownArrow: true,
      usePushState: false,
      showTooltip: true,
      spacing: 7,
      width: 41
    };
  },

  /*************************************************************************************************************************/
  // Helper methods to pass to generator

  // When the active cell changes to a new cell, get the new menu value that should be selected/checked 
  _getMenuValueFromCell(cell) {
    console.log("\n\n\nVAlign picker cell ", cell)
    return 'bottom'; // TODO: eventually cell.cellProps.valign
  },

  _propagateControlStateChange(nextValue, rng) {
    console.log("Propagating language change: " + nextValue);
    return; // TODO: eventually some API call
  },

  // Update the toolbar control props given a the menu visibility, menuValue, and current toolbarProps.
  // In this case, we want the icon to reflect the menu choice.
  _toolbarControlPropTransform(menuVisible, menuValue, toolbarControlProps) {
    toolbarControlProps.showTooltip = !menuVisible;
    toolbarControlProps.iconName = 'vertical_align_' + menuValue;
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
        initialValue={'bottom'}
        menuWidth={65} 
        id="VAlignPicker" />
    );
  }

});


