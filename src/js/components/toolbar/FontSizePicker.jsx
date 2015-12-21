import React from 'react';
import ReactDOM from 'react-dom';
import {Styles, FontIcon} from 'material-ui';

import ToolbarController from './ToolbarController.jsx';
import ToolbarTextField from './ToolbarTextField.jsx';
import MenuController from './MenuController.jsx';

export default React.createClass({

  /* We keep track of menu visiblility (needed for tooltip visibility) and the propTag used */
  getInitialState() {
    return {
      menuVisible: false
    }
  },

  /*************************************************************************************************************************/
  // Sub-component generation

  fontSize: [6, 7, 8, 9, 10, 11, 12, 14, 18, 24, 36],

  getMenuProps() {
    return this.fontSize.map((size) => {
      return {tag: 'MenuItem', primaryText: `${size}`, value: `${size}`};
    });
  },

  getFontSizeField() {
    return (
      <ToolbarTextField 
        ref="textfield"
        tooltip={"Font size"}
        onClick={this._onTextFieldClick}
        showTooltip={!this.state.menuVisible}
        width={45} />
    );
  },

  /*************************************************************************************************************************/
  // Respond to events

  /* When the textfield is clicked, make the menu visible if it already isn't and vice versa. This will automatically
  make the tooltip disappear */
  _onTextFieldClick(e) {
    this.setState({ menuVisible: !this.state.menuVisible});
    this.refs.menu.toggleMenuVisible();
  },

  /* 
    Prop passed to MenuController
    When a menu item is clicked, update the button and controller, then call a state change callback to update backend 
  */
  _onMenuClick(nextValue) {
    this.refs.textfield.setDisplayValue(nextValue);
    this.refs.controller.onControlStateChange(nextValue);
  },

  /* Given a font prop, extract the value (font size) from it */
  getValueFromProp(p) {
    return "10";
  },

  /* 
    Prop passed down to MenuController
  */
  _onMenuClose() {
    this.setState({menuVisible: false});
    this.refs.textfield.setActive(false);
  },

  /*************************************************************************************************************************/
  // ToolbarController callbacks

  /* Update backend given the nextState (the updated value of the menu) and the active range */
  _setBackendCellProp(nextState, rng) {
    console.log("Setting backend via api");
  },


  /* When the activeCell changes, update the menu with a new value, and update the button as well */
  _setControlStateFromCellProp(prop) {
    let menuValue = this.getValueFromProp(prop);
    this.refs.menu.setMenuValue(menuValue);
    this.refs.textfield.setDisplayValue(menuValue);
  },

  /*************************************************************************************************************************/
  //Render

  render() {
    let fontField = this.getFontSizeField(),
        menuProps = this.getMenuProps();
    let dropdown = 
      <MenuController 
        ref="menu"
        toolbarControlWidth={45}
        menuProps={menuProps} 
        toolbarControl={fontField} 
        initialCheckedValue="10"
        onMenuClick={this._onMenuClick}
        onMenuClose={this._onMenuClose} />;
    return (
      <ToolbarController 
        ref="controller"
        propTag="Font"
        setControlStateFromCellProp={this._setControlStateFromCellProp}
        setBackendCellProp={this._setBackendCellProp}
        control={dropdown} />
    );
  }

});
