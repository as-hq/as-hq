import React from 'react';
import ReactDOM from 'react-dom';
import {Styles, FontIcon} from 'material-ui';

import ToolbarController from './ToolbarController.jsx';
import ToolbarButton from './ToolbarButton.jsx';
import MenuController from './MenuController.jsx';


export default React.createClass({

  /*************************************************************************************************************************/
  // Prop and state methods

  propTypes: {

  },

  getDefaultProps() {
    return {
      
    };
  },

  /* We keep track of menu visiblility (needed for tooltip visibility) and the propTag used */
  getInitialState() {
    return {
      menuVisible: false,
      propTag: 'Money',
    }
  },

  /*************************************************************************************************************************/
  // Sub-component generation

  menuProps: [
    {tag: 'MenuItem', primaryText: 'Automatic', value: 'Automatic'},
    {tag: 'MenuItem', primaryText: 'Plain Text', value: 'Plain Text'},
    {tag: 'Divider', style:{marginTop: 5, marginBottom: 5}},
    {tag: 'MenuItem', primaryText: 'Number', value: 'Number', secondaryText: '1000.12'},
    {tag: 'MenuItem', primaryText: 'Percent', value: 'Percent', secondaryText: '10.12%'},
    {tag: 'MenuItem', primaryText: 'Scientific', value: 'Scientific', secondaryText: '1.01E+03'},
    {tag: 'Divider', style:{marginTop: 5, marginBottom: 5}},
    {tag: 'MenuItem', primaryText: 'Financial', value: 'Financial', secondaryText: '(1000.12)'},
    {tag: 'MenuItem', primaryText: 'Currency', value: 'Currency', secondaryText: '$1000.12'},
    {tag: 'Divider', style:{marginTop: 5, marginBottom: 5}},

  ],

  /*
  The button element has a tooltip, doesn't change color when pushed, and has a dropdown arrow */
  getButton() {
    return (
      <ToolbarButton 
        ref="button"
        tooltip={"More formats"}
        iconName={"wb_sunny"}
        onClick={this._onButtonClick}
        usePushState={false}
        showTooltip={!this.state.menuVisible}
        includeDropdownArrow={true}
        width={55} />
    );
  },

  /*************************************************************************************************************************/
  // Helper methods

  // This component deals with many propTags at once. Which one the toolbarController applies is dictated by the menu's
  // selected item. This converts from that value to the appropriate propTag. "Value" refers to the uid's used by the menu.
  getPropTagFromValue(v) {
    return v;
  },

  getValueFromProp(p) {
    return "Number"; //p.propTag;
  },

  // How should the button itself (icon, etc) change for this component if the value of the menu does 
  updateButtonGivenValue(v) {
    return;
  },

  /*************************************************************************************************************************/
  // Respond to events

  /* When the button is clicked, make the menu visible if it already isn't and vice versa. This will automatically
  make the tooltip disappear */
  _onButtonClick(e) {
    this.setState({ menuVisible: !this.state.menuVisible});
    this.refs.menu.toggleMenuVisible();
  },

  /* 
    Prop passed to MenuController
    When a menu item is clicked, update the button and controller, then call a state change callback to update backend 
  */
  _onMenuClick(nextValue) {
    this.updateButtonGivenValue(nextValue);
    let tag = this.getPropTagFromValue(nextValue);
    this.setState({propTag: tag}, this.refs.controller.onControlStateChange(nextValue));
  },

  /* 
    Prop passed down to MenuController
  */
  _onMenuClose() {
    this.setState({menuVisible: false});
    this.refs.button.setPushState(false);
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
    this.updateButtonGivenValue(menuValue);
  },

  /*************************************************************************************************************************/
  //Render

  render() {
    let button = this.getButton(),
        menuProps = this.menuProps;
    let dropdown = 
      <MenuController 
        ref="menu"
        toolbarControlWidth={55}
        menuProps={menuProps} 
        toolbarControl={button} 
        initialCheckedValue="Number"
        onMenuClick={this._onMenuClick}
        onMenuClose={this._onMenuClose} />;
    return (
      <ToolbarController 
        ref="controller"
        propTag={this.state.propTag}
        setControlStateFromCellProp={this._setControlStateFromCellProp}
        setBackendCellProp={this._setBackendCellProp}
        control={dropdown} />
    );
  }

});
