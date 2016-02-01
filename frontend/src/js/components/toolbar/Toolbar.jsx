/* @flow */

import React from 'react';
// $FlowFixMe: need to create declarations for all of these, but it's complicated.
import {Toolbar, Styles} from 'material-ui';
// $FlowFixMe: need to create declarations for all of these, but it's complicated.
import {IconMenu, MenuItem, IconButton} from 'material-ui';

import API from '../../actions/ASApiActionCreators';
import Constants from '../../Constants';

import SelectionStore from '../../stores/ASSelectionStore';

import ToolbarButton from './ToolbarButton.jsx';
import VaryPropButton from './VaryPropButton.jsx';
import MoreFormatDropdown from './MoreFormatDropdown.jsx';
import FontSizePicker from './FontSizePicker.jsx';
import FontPicker from './FontPicker.jsx';
import LanguagePicker from './LanguagePicker.jsx';
import ColorPicker from './ColorPicker.jsx';
import HAlignPicker from './HAlignPicker.jsx';
import VAlignPicker from './VAlignPicker.jsx';

import type {
  ASLanguage,
  ASExpression,
  ASValue,
  ASSheet,
  ASCellProp,
  VAlignType,
  HAlignType
} from '../../types/Eval';


type ToolbarProps = {
  toolbarHeight: number;
};

type ToolbarDefaultProps = {
  toolbarHeight: number;
};

type ToolbarState = {};

export default class ASToolbar
  extends React.Component<ToolbarDefaultProps, ToolbarProps, ToolbarState>
{

  constructor(props: ToolbarProps) {
    super(props);
  }

  /*************************************************************************************************************************/
  // React methods

  // #needsrefactor should go into global styles folder...
  getStyles(): any {
    return {
      // Because we're using inline-block, the height should all be the same for the elements on the toolbar, otherwise things
      // screw up because CSS. This is a reasonable restriction anyway, so I'm not debugging it further (Ritesh 12/17)
      toolbarStyle: {
        backgroundColor: '#333333',
        position: 'relative',
        height: this.props.toolbarHeight // height of toolbar
      },
      // Used to create a separating element between parts of toolbar
      // There is a ToolbarSeparator in material-ui but it didn't quite fit the bill; a simple div we control is better
      separatorStyle: {
        display: 'inline-block',
        height: this.props.toolbarHeight,
        marginLeft: 10, // equal separation distance on both sides
        marginRight: 10,
        backgroundColor: '#202020',
        boxShadow: '0px 0px 1px 0px rgba(255, 255, 255, 0.35)',
        verticalAlign: 'top', // we want the separator to span the height of the whole pane
        width: 2
      }
    }
  }

  /*************************************************************************************************************************/
  // Render

  render(): React.Element {

    let {toolbarStyle, separatorStyle} = this.getStyles();
    let shiftRight = <div style={{display: 'inline-block', marginLeft: 50, position: 'relative'}} />;
    let handleDecimalChange = (i) => {
      SelectionStore.withActiveSelection( ({range: rng}) => {
        API.handleChangeDecimalPrecision(i, rng);
      });
    };

    return (
      <div style={toolbarStyle} >

        {shiftRight}

        <ToolbarButton iconName="print" tooltip="Print (Ctrl+P)" usePushState={false}
          onClick={() => {}} />
        <ToolbarButton iconName="undo" tooltip="Undo (Ctrl+Z)" usePushState={false}
          onClick={(e) => {API.undo();}} />
        <ToolbarButton iconName="redo" tooltip="Redo (Ctrl+Y)" usePushState={false}
          onClick={(e) => {API.redo();}} />
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
        <ToolbarButton iconName="zoom_out" tooltip="Decrease decimal places" usePushState={false}
          onClick={(e) => {handleDecimalChange(-1);}} />
        <ToolbarButton iconName="zoom_in" tooltip="Increase decimal places" usePushState={false}
          onClick={(e) => {handleDecimalChange(1);}} />
        <MoreFormatDropdown />

        <div style={separatorStyle} />
        <LanguagePicker />
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
        <ColorPicker propTag="TextColor" iconName="text_format" tooltip="Text color" />

        <div style={separatorStyle} />

        <ColorPicker propTag="FillColor" iconName="format_color_fill" tooltip="Fill color" />
        <ColorPicker propTag="BorderColor" iconName="border_color" tooltip="Border color" />
        <HAlignPicker />
        <VAlignPicker />

        <div style={separatorStyle} />

        <ToolbarButton iconName="link" tooltip="Insert link (Ctrl+K)" usePushState={false}
          onClick={() => {}}/>
        <ToolbarButton iconName="comment" tooltip="Insert comment (Ctrl+Alt+M)" usePushState={false}
          onClick={() => {}}/>
        <ToolbarButton iconName="poll" tooltip="Insert chart..." usePushState={false}
          onClick={() => {}}/>
        <ToolbarButton iconName="functions" tooltip="Functions" usePushState={false}
          onClick={(e, state) => {window.open('http://alphasheets.com');}}/>

      </div>
    );
  }
}



ASToolbar.defaultProps = {
  toolbarHeight: 50
};

ASToolbar.propTypes = {
  toolbarHeight: React.PropTypes.number
};
