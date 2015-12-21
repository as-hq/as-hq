import React from 'react';
import {Toolbar, Styles} from 'material-ui';
import {IconMenu, MenuItem, IconButton} from 'material-ui';

import API from '../../actions/ASApiActionCreators';
import Constants from '../../Constants';

import ToolbarButton from './ToolbarButton.jsx';
import VaryPropButton from './VaryPropButton.jsx';
import MoreFormatDropdown from './MoreFormatDropdown.jsx';
import FontPicker from './FontPicker.jsx';
import FontSizePicker from './FontSizePicker.jsx';

export default React.createClass({

  /*************************************************************************************************************************/
  // React methods

  propTypes: {
    toolbarHeight: React.PropTypes.number
  },

  getDefaultProps() {
    return {
      toolbarHeight: 70
    };
  },

  /*************************************************************************************************************************/
  // Render

  render() {

    // Because we're using inline-block, the height should all be the same for the elements on the toolbar, otherwise things
    // screw up because CSS. This is a reasonable restriction anyway, so I'm not debugging it further (Ritesh 12/17)

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

    let shiftRight = <div style={{display: 'inline-block', marginLeft: 30, position: 'relative'}} />;

    return (
      <div style={toolbarStyle} >

        {shiftRight}
       
        <ToolbarButton iconName="print" tooltip="Print (Ctrl+P)" usePushState={false}
          onClick={() => {}} />
        <ToolbarButton iconName="undo" tooltip="Undo (Ctrl+Z)" usePushState={false}
          onClick={(state) => {API.undo()}} />
        <ToolbarButton iconName="redo" tooltip="Redo (Ctrl+Y)" usePushState={false}
          onClick={(state) => {API.redo()}} />
        <ToolbarButton iconName="format_paint" tooltip="Paint format" usePushState={false}
          onClick={() => {}} />


        <div style={separatorStyle} />

        <VaryPropButton
          propTag="Money"
          tooltip="Format as currency"
          iconName="attach_money"  />
        <VaryPropButton
          propTag="Percentage"
          tooltip="Format as percent"
          iconName="create" />
        <ToolbarButton iconName="zoom_in" tooltip="Decrease decimal places" usePushState={false}
          onClick={() => {}}/>
        <ToolbarButton iconName="zoom_out" tooltip="Increase decimal places" usePushState={false}
          onClick={() => {}}/>
        <MoreFormatDropdown />
        

        <div style={separatorStyle} />

        <FontPicker />

        <div style={separatorStyle} />

        <FontSizePicker />

        <div style={separatorStyle} />

        <VaryPropButton
          propTag="Bold"
          tooltip="Bold (Ctrl+B)"
          iconName="format_bold" />
        <VaryPropButton
          propTag="Italic"
          tooltip="Italic (Ctrl+I)"
          iconName="format_italic" />
        <VaryPropButton
          propTag="Strikethrough"
          tooltip="Strikethrough (Alt+Shift+5)"
          iconName="strikethrough_s" />

        
        <div style={separatorStyle} />

        <ToolbarButton iconName="link" tooltip="Insert link (Ctrl+K)" usePushState={false}
          onClick={() => {}}/>
        <ToolbarButton iconName="comment" tooltip="Insert comment (Ctrl+Alt+M)" usePushState={false}
          onClick={() => {}}/>
        <ToolbarButton iconName="poll" tooltip="Insert chart..." usePushState={false}
          onClick={() => {}}/>

       
      </div>
    );
  }

  
});
