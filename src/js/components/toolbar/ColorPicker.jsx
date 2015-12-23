import React from 'react';
import ReactDOM from 'react-dom';
import {Styles, FontIcon} from 'material-ui';
let Paper = require('material-ui/lib/paper');
import ColorPicker from 'react-color';

import Constants from '../../Constants';
import API from '../../actions/ASApiActionCreators';
import U from '../../AS/Util';
let {
  Conversion: TC
} = U;

import MenuController from './MenuController.jsx';
import ToolbarButton from './ToolbarButton.jsx';

export default React.createClass({

  // Easy way to deal with closing menu upon clickaway
  mixins: [require('react-clickaway')],

  /*************************************************************************************************************************/
  // Props and state data

  propTypes: {
    tooltip: React.PropTypes.string.isRequired,
    iconName: React.PropTypes.string.isRequired,
    propTag: React.PropTypes.string.isRequired // TextColor, FillColor, etc
  },

  getInitialState() {
    return {
      color: Constants.DefaultColors[this.props.propTag],
      pickerVisible: false,
    }
  },

  /*************************************************************************************************************************/
  // Respond to events

  // When a color has been selected, close the menu and tell the toolbarcontroller, which will call propagate
  // This will also cause the button color to change due to the rerender
  handleColorChange(color) {
    let newColor =  '#'+ color.hex;
    this.setState({pickerVisible: false, color: newColor});
    this.refs.menuController.refs.controller.onControlStateChange(newColor);
  },

  _onButtonClick(e, pushState) {
    this.setState({pickerVisible: !this.state.pickerVisible});
  },

  _onPickerClose() {
    this.setState({pickerVisible: false});
  },

  // Close picker when user clicks away if picker is currently visible
  componentClickAway() {
    if (this.state.pickerVisible){
      this._onPickerClose();
    }
  },

  /*************************************************************************************************************************/
  // Methods to pass to ToolbarController from MenuController to monitor stores and act upon changes

  _setControlStateFromCell(cell) {
    let newColor = Constants.DefaultColors[this.props.propTag];
    let prop = (cell != null) ? U.Cell.getPropByTag(this.props.propTag, cell) : null;
    if (prop != null) {
      newColor = prop.contents;
    }
    this.setState({ color: newColor });
  },

  _propagateControlStateChange(nextState, rng) {
    API.setProp({
      tag: this.props.propTag,
      contents: nextState
    }, rng);
  },

  /*************************************************************************************************************************/
  // Styles and rendering

  getStyles() {
    return {
      colorPickerStyle:{
        position: 'absolute',
        width: 245
      }
    };
  },

  render() {
    let {colorPickerStyle, paperStyle} = this.getStyles();
    let button = 
      <ToolbarButton
        usePushState={false}
        spacing={7}
        width={43}
        showTooltip={!this.state.pickerVisible}
        iconName={this.props.iconName}
        includeDropdownArrow={true}
        tooltip={this.props.tooltip}
        onClick={this._onButtonClick} />;
    let colorPicker = this.state.pickerVisible ? 
        <div style={colorPickerStyle} >
          <ColorPicker
            onChangeComplete={this.handleColorChange}
            color={this.state.color}
            position="top"
            display='top'
            type="compact" />
        </div> : null;
    return (
      <MenuController
        ref="menuController"
        toolbarWidth={36}
        toolbarComponent={button}
        menuComponent={colorPicker}
        setControlStateFromCell={this._setControlStateFromCell}
        propagateControlStateChange={this._propagateControlStateChange}
        id={"ColorPicker" + this.props.propTag}
        onMenuShouldClose={this._onPickerClose} />
    );
  }

});


