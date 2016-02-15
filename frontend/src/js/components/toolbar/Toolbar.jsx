/* @flow */

import type {StoreLink} from '../../types/React';
import type {StoreToken} from 'flux';

import React from 'react';

import ToolbarActionCreators from '../../actions/ASToolbarActionCreators';
import Util from '../../AS/Util';

import CellStore from '../../stores/ASCellStore';
import ToolbarStore from '../../stores/ASToolbarStore';
import ExpStore from '../../stores/ASExpStore';

import ToolbarButton from './ToolbarButton.jsx';
import MoreFormatDropdown from './MoreFormatDropdown.jsx';
import FontPicker from './FontPicker.jsx';
import FontSizePicker from './FontSizePicker.jsx';
import LanguagePicker from './LanguagePicker.jsx';
import ColorPicker from './ColorPicker.jsx';
import HAlignPicker from './HAlignPicker.jsx';
import VAlignPicker from './VAlignPicker.jsx';

type Props = {
  toolbarHeight: number;
};

type DefaultProps = {
  toolbarHeight: number;
};


function isActiveProp(tag: string, cell: ?ASCellObject) {
  if (cell == null) {
    return false;
  }

  for (const prop of cell.props) {
    if (prop.tag === tag) {
      return true;
    }
  }

  return false;
}


function Separator() {
  return <div style={styles.separatorStyle} />;
}


export default class ASToolbar
  extends React.Component<DefaultProps, Props, {}> {
  $storeLinks: Array<StoreLink>;
  _cellStoreListener: StoreToken;

  componentDidMount() {
    Util.React.addStoreLinks(this, [
      { store: ToolbarStore },
      // listen to this store for the language picker
      { store: ExpStore },
    ]);
    this._cellStoreListener = CellStore.addListener(() => this.forceUpdate());
  }

  componentWillUnmount() {
    Util.React.removeStoreLinks(this);
    this._cellStoreListener.remove();
  }

  render(): React.Element {
    const shiftRight = (
      <div
        style={{
          display: 'inline-block',
          marginLeft: 50,
          position: 'relative',
        }}
      />
    );
    const activeButton = ToolbarStore.getActiveMenuItem();
    const cell = CellStore.getActiveCell();

    return (
      <div
        style={styles.toolbarStyle}
        className="toolbar"
      >

        {shiftRight}

        <ToolbarButton
          iconName="print"
          tooltip="Print (Ctrl+P)"
          onClick={() => ToolbarActionCreators.print()}
        />
        <ToolbarButton
          iconName="undo"
          tooltip="Undo (Ctrl+Z)"
          onClick={() => {ToolbarActionCreators.undo();}}
        />
        <ToolbarButton
          iconName="redo"
          tooltip="Redo (Ctrl+Y)"
          onClick={() => {ToolbarActionCreators.redo();}}
        />
        <ToolbarButton
          iconName="format_paint"
          tooltip="Paint format"
          onClick={() => {ToolbarActionCreators.paintFormat();}}
        />

        <Separator />

        <ToolbarButton
          propTag="Money"
          tooltip="Format as currency"
          iconName="attach_money"
          onClick={() => ToolbarActionCreators.formatAs('Money')}
        />
        <ToolbarButton
          propTag="Percentage"
          tooltip="Format as percent"
          iconName="create"
          onClick={() => ToolbarActionCreators.formatAs('Percentage')}
        />
        <ToolbarButton
          iconName="zoom_out"
          tooltip="Decrease decimal places"
          onClick={() => {ToolbarActionCreators.handleDecimalChange(-1);}}
        />
        <ToolbarButton
          iconName="zoom_in"
          tooltip="Increase decimal places"
          onClick={() => {ToolbarActionCreators.handleDecimalChange(1);}}
        />

        <MoreFormatDropdown
          visible={activeButton === 'MoreFormat'}
        />

        <Separator />
        <LanguagePicker
          visible={activeButton === 'LanguagePicker'}
          language={ExpStore.getLanguage()}
        />

        <Separator />
        <FontPicker
          visible={activeButton === 'FontPicker'}
          value={ExpStore.getFont()}
        />

        <Separator />
        <FontSizePicker
          visible={activeButton === 'FontSizePicker'}
          value={ExpStore.getFontSize()}
        />
        <Separator />

        <ToolbarButton
          propTag="Bold"
          // TODO(joel) - use metaKeyName to give better tooltip messages on mac
          tooltip="Bold (Ctrl+B)"
          iconName="format_bold"
          active={isActiveProp('Bold', cell)}
          onClick={() => ToolbarActionCreators.toggleBooleanCellTag('Bold')}
        />
        <ToolbarButton
          propTag="Italic"
          tooltip="Italic (Ctrl+I)"
          iconName="format_italic"
          active={isActiveProp('Italic', cell)}
          onClick={() => ToolbarActionCreators.toggleBooleanCellTag('Italic')}
        />
        <ToolbarButton
          propTag="Strikethrough"
          tooltip="Strikethrough (Alt+Shift+5)"
          iconName="strikethrough_s"
          active={isActiveProp('Strikethrough', cell)}
          // TODO(joel) Make strikethrough cell prop!
          onClick={() => ToolbarActionCreators.toggleBooleanCellTag('Strikethrough')}
        />
        <ColorPicker
          propTag="TextColor"
          iconName="text_format"
          tooltip="Text color"
          active={activeButton === 'TextColor'}
          value={ExpStore.getTextColor()}
          onSelect={color => ToolbarActionCreators.setColor('TextColor', color)}
          onOpen={() => ToolbarActionCreators.openItem('TextColor')}
          onClose={() => ToolbarActionCreators.closeItem('TextColor')}
        />

        <Separator />

        <ColorPicker
          propTag="FillColor"
          iconName="format_color_fill"
          tooltip="Fill color"
          active={activeButton === 'FillColor'}
          value={ExpStore.getFillColor()}
          onSelect={color => ToolbarActionCreators.setColor('FillColor', color)}
          onOpen={() => ToolbarActionCreators.openItem('FillColor')}
          onClose={() => ToolbarActionCreators.closeItem('FillColor')}
        />
        <ColorPicker
          propTag="BorderColor"
          iconName="border_color"
          tooltip="Border color"
          active={activeButton === 'BorderColor'}
          value={ExpStore.getBorderColor()}
          onSelect={color => ToolbarActionCreators.setColor('BorderColor', color)}
          onOpen={() => ToolbarActionCreators.openItem('BorderColor')}
          onClose={() => ToolbarActionCreators.closeItem('BorderColor')}
        />

        <HAlignPicker
          visible={activeButton === 'HAlignPicker'}
          value={ExpStore.getHAlign()}
        />
        <VAlignPicker
          visible={activeButton === 'VAlignPicker'}
          value={ExpStore.getVAlign()}
        />

        <Separator />

        <ToolbarButton
          iconName="link"
          tooltip="Insert link (Ctrl+K)"
          onClick={() => {}}
        />
        <ToolbarButton
          iconName="comment"
          tooltip="Insert comment (Ctrl+Alt+M)"
          onClick={() => {}}
        />
        <ToolbarButton
          iconName="poll"
          tooltip="Insert chart..."
          onClick={() => {}}
        />
        <ToolbarButton
          iconName="functions"
          tooltip="Functions"
          onClick={(e, state) => {window.open('http://alphasheets.com');}}
        />

      </div>
    );
  }
}


ASToolbar.defaultProps = {
  toolbarHeight: 50,
};

ASToolbar.propTypes = {
  toolbarHeight: React.PropTypes.number,
};


const styles = {
  // Because we're using inline-block, the height should all be the same for the elements on the toolbar, otherwise things
  // screw up because CSS. This is a reasonable restriction anyway, so I'm not debugging it further (Ritesh 12/17)
  toolbarStyle: {
    backgroundColor: '#333333',
    width: '100%',
    position: 'relative',
    height: 50, // TODO(joel) this.props.toolbarHeight // height of toolbar
  },
  // Used to create a separating element between parts of toolbar
  // There is a ToolbarSeparator in material-ui but it didn't quite fit the bill; a simple div we control is better
  separatorStyle: {
    display: 'inline-block',
    height: 50, // TODO(joel) this.props.toolbarHeight,
    marginLeft: 10, // equal separation distance on both sides
    marginRight: 10,
    backgroundColor: '#202020',
    boxShadow: '0px 0px 1px 0px rgba(255, 255, 255, 0.35)',
    verticalAlign: 'top', // we want the separator to span the height of the whole pane
    width: 2,
  },
};
