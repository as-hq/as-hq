/* @flow */

import type {StoreLink} from '../../types/React';
import type {StoreToken} from 'flux';

import React from 'react';

import ToolbarActionCreators from '../../actions/ASToolbarActionCreators';
import Util from '../../AS/Util';

import ASCell from '../../classes/ASCell';

import CellStore from '../../stores/ASCellStore';
import ToolbarStore from '../../stores/ASToolbarStore';
import ExpressionStore from '../../stores/ASExpressionStore';
import SheetStateStore from '../../stores/ASSheetStateStore';

import ToolbarButton from './ToolbarButton.jsx';
import MoreFormatDropdown from './MoreFormatDropdown.jsx';
import FontPicker from './FontPicker.jsx';
import FontSizePicker from './FontSizePicker.jsx';
// $FlowFixMe ::ALEX::
import LanguagePicker from './LanguagePicker.jsx';
import Separator from './Separator.jsx';
import ColorPicker from './ColorPicker.jsx';
import HAlignPicker from './HAlignPicker.jsx';
import VAlignPicker from './VAlignPicker.jsx';

type Props = {
  toolbarHeight: number;
};

type DefaultProps = {
  toolbarHeight: number;
};


function isActiveProp(tag: string, cell: ?ASCell) {
  if (cell == null) {
    return false;
  }

  return cell.hasPropWithTag(tag);
}

export default class ASToolbar extends React.Component {
  static defaultProps: DefaultProps = {
    toolbarHeight: 50
  };

  props: Props;
  state: {};

  $storeLinks: Array<StoreLink>;
  _cellStoreListener: StoreToken;
  _expressionListener: StoreToken;

  componentDidMount() {
    Util.React.addStoreLinks(this, [
      { store: ToolbarStore },
    ]);
    this._expressionListener = ExpressionStore.addListener(() => this.forceUpdate());
    this._cellStoreListener = CellStore.addListener(() => this.forceUpdate());
  }

  componentWillUnmount() {
    Util.React.removeStoreLinks(this);
    this._cellStoreListener.remove();
    this._expressionListener.remove();
  }

  render(): React.Element {
    const shiftRight = (
      <div
        style={{
          display: 'inline-block',
          marginLeft: 70,
          position: 'relative',
        }} />
    );
    const activeButton = ToolbarStore.getActiveMenuItem();
    const cell = CellStore.getActiveCell();
    const inPauseMode = SheetStateStore.inPauseMode();

    // Currently not supporting horizontal or vertical alignment (Ritesh 3/4)
    return (
      <div
        style={style}
        className="toolbar"
      >

        {shiftRight}

        <LanguagePicker
          visible={activeButton === 'LanguagePicker'}
          language={ExpressionStore.getLanguage()} />

        <Separator />

        <ToolbarButton
          iconName="print"
          tooltip="Print (Ctrl+P)"
          onClick={() => ToolbarActionCreators.print()} />
        <ToolbarButton
          iconName="undo"
          tooltip="Undo (Ctrl+Z)"
          onClick={() => {ToolbarActionCreators.undo();}} />
        <ToolbarButton
          iconName="redo"
          tooltip="Redo (Ctrl+Y)"
          onClick={() => {ToolbarActionCreators.redo();}} />
        <ToolbarButton
          iconName="format_paint"
          tooltip="Paint format"
          onClick={() => {ToolbarActionCreators.paintFormat();}} />

        <Separator />

        <ToolbarButton
          propTag="Money"
          tooltip="Format as currency"
          iconName="attach_money"
          onClick={() => ToolbarActionCreators.formatAs('Money')} />
        <ToolbarButton
          propTag="Percentage"
          tooltip="Format as percent"
          iconName="create"
          onClick={() => ToolbarActionCreators.formatAs('Percentage')} />
        <ToolbarButton
          iconName="zoom_out"
          tooltip="Decrease decimal places"
          onClick={() => {ToolbarActionCreators.handleDecimalChange(-1);}} />
        <ToolbarButton
          iconName="zoom_in"
          tooltip="Increase decimal places"
          onClick={() => {ToolbarActionCreators.handleDecimalChange(1);}} />
        <MoreFormatDropdown
          visible={activeButton === 'MoreFormat'} />

        <Separator />

        <FontPicker
          visible={activeButton === 'FontPicker'}
          value={'Arial'} />

        <Separator />

        <FontSizePicker
          visible={activeButton === 'FontSizePicker'}
          value={'10'} />

        <Separator />

        <ToolbarButton
          propTag="Bold"
          // TODO(joel) - use metaKeyName to give better tooltip messages on mac
          tooltip="Bold (Ctrl+B)"
          iconName="format_bold"
          active={isActiveProp('Bold', cell)}
          onClick={() => ToolbarActionCreators.toggleBooleanCellTag('Bold')} />
        <ToolbarButton
          propTag="Italic"
          tooltip="Italic (Ctrl+I)"
          iconName="format_italic"
          active={isActiveProp('Italic', cell)}
          onClick={() => ToolbarActionCreators.toggleBooleanCellTag('Italic')} />
        <ToolbarButton
          propTag="Strikethrough"
          tooltip="Strikethrough (Alt+Shift+5)"
          iconName="strikethrough_s"
          active={isActiveProp('Strikethrough', cell)}
          // TODO(joel) Make strikethrough cell prop!
          onClick={() => ToolbarActionCreators.toggleBooleanCellTag('Strikethrough')} />

        <ColorPicker
          propTag="TextColor"
          iconName="text_format"
          tooltip="Text color"
          active={activeButton === 'TextColor'}
          value={'#212121'}
          onSelect={color => ToolbarActionCreators.setColor('TextColor', '#' + color.hex)}
          onOpen={() => ToolbarActionCreators.openItem('TextColor')}
          onClose={() => ToolbarActionCreators.closeItem('TextColor')} />

        <Separator />

        <ColorPicker
          propTag="FillColor"
          iconName="format_color_fill"
          tooltip="Fill color"
          active={activeButton === 'FillColor'}
          value={'#ffffff'}
          onSelect={color => ToolbarActionCreators.setColor('FillColor', '#' + color.hex)}
          onOpen={() => ToolbarActionCreators.openItem('FillColor')}
          onClose={() => ToolbarActionCreators.closeItem('FillColor')} />
        <ColorPicker
          propTag="BorderColor"
          iconName="border_color"
          tooltip="Border color"
          active={activeButton === 'BorderColor'}
          value={'#0062b1'}
          onSelect={color => ToolbarActionCreators.setColor('BorderColor', '#' + color.hex)}
          onOpen={() => ToolbarActionCreators.openItem('BorderColor')}
          onClose={() => ToolbarActionCreators.closeItem('BorderColor')} />

        <Separator />

        <ToolbarButton
          iconName="link"
          tooltip="Insert link (Ctrl+K)"
          onClick={() => {}} />
        <ToolbarButton
          iconName="comment"
          tooltip="Insert comment (Ctrl+Alt+M)"
          onClick={() => {}} />
        <ToolbarButton
          iconName="poll"
          tooltip="Insert chart..."
          onClick={() => {}} />
        <ToolbarButton
          iconName="functions"
          tooltip="Functions"
          onClick={(e, state) => {window.open('http://alphasheets.com');}} />

      </div>
    );
  }
}

ASToolbar.propTypes = {
  toolbarHeight: React.PropTypes.number,
};


  // Because we're using inline-block, the height should all be the same for the elements on the toolbar, otherwise things
  // screw up because CSS. This is a reasonable restriction anyway, so I'm not debugging it further (Ritesh 12/17)
const style = {
  backgroundColor: '#333333',
  width: '100%',
  position: 'relative',
  height: 50 // TODO(joel) this.props.toolbarHeight // height of toolbar
};
