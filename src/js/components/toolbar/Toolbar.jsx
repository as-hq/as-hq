import React from 'react';
import API from '../../actions/ASApiActionCreators';
import Constants from '../../Constants';

import {Toolbar, Styles} from 'material-ui';
import {IconMenu, MenuItem, IconButton} from 'material-ui';
let Tooltip = require("react-tooltip");

import ToolbarButton from './ToolbarButton.jsx';
import VaryPropButton from './VaryPropButton.jsx';

import MoreFormatDropdown from './MoreFormatDropdown.jsx';

export default React.createClass({

  /*************************************************************************************************************************/
  // React methods

  propTypes: {
    toolbarHeight: React.PropTypes.number
  },

  getDefaultProps() {
    return {
      toolbarHeight: 40
    };
  },

  /*************************************************************************************************************************/
  // Render

  render() {

    let toolbarStyle = {
      backgroundColor: Styles.Colors.grey700, 
      width: '100%', 
      position: 'relative', 
      height: this.props.toolbarHeight // height of toolbar
    };

    // Used to create a separating element between parts of toolbar
    // There is a ToolbarSeparator in material-ui but it didn't quite fit the bill; a simple div we control is better
    let separatorStyle = {
      backgroundColor: Styles.Colors.grey900,
      display: 'inline-block', 
      height: this.props.toolbarHeight,
      marginLeft: 10, // equal separation distance on both sides
      marginRight: 10,
      verticalAlign: 'top', // we want the separator to span the height of the whole pane
      width: 2
    };

    // Common id of tooltip for all toolbar components
    let tooltipId = "button";
    
    return (
      <Toolbar style={toolbarStyle}>

        <ToolbarButton tooltipId={tooltipId} iconName="print" tooltip="Print (Ctrl+P)" usePushState={false}
          onClick={() => {}} />

        <ToolbarButton tooltipId={tooltipId} iconName="undo" tooltip="Undo (Ctrl+Z)" usePushState={false}
          onClick={(state) => {API.undo()}} />
        <ToolbarButton tooltipId={tooltipId} iconName="redo" tooltip="Redo (Ctrl+Y)" usePushState={false}
          onClick={(state) => {API.redo()}} />
        <ToolbarButton tooltipId={tooltipId} iconName="format_paint" tooltip="Paint format" usePushState={false}
          onClick={() => {}} />


        <div style={separatorStyle} />

        <VaryPropButton
          propTag="Money"
          tooltipId={tooltipId}
          tooltip="Format as currency"
          iconName="attach_money"  />

        <VaryPropButton
          propTag="Percentage"
          tooltipId={tooltipId}
          tooltip="Format as percent"
          iconName="create" />

        <ToolbarButton tooltipId={tooltipId} iconName="zoom_in" tooltip="Decrease decimal places" usePushState={false}
          onClick={() => {}}/>
        <ToolbarButton tooltipId={tooltipId} iconName="zoom_out" tooltip="Increase decimal places" usePushState={false}
          onClick={() => {}}/>

        <MoreFormatDropdown tooltipId={tooltipId} />
        

        <div style={separatorStyle} />

       
        <div style={separatorStyle} />

        <VaryPropButton
          propTag="Bold"
          tooltipId={tooltipId}
          tooltip="Bold (Ctrl+B)"
          iconName="format_bold" />
        <VaryPropButton
          propTag="Italic"
          tooltipId={tooltipId}
          tooltip="Italic (Ctrl+I)"
          iconName="format_italic" />
        <VaryPropButton
          propTag="Strikethrough"
          tooltipId={tooltipId}
          tooltip="Strikethrough (Alt+Shift+5)"
          iconName="strikethrough_s" />

        
      
        <div style={separatorStyle} />

        <Tooltip 
          id="button" 
          place="bottom"
          type="info"
          effect="solid"
          offset="{'top': 10, 'left': 0}" />
      </Toolbar>

    );
  }

  
});
