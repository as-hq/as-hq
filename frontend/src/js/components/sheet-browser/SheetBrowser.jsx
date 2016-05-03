/* @flow */

import type { Callback } from '../../types/Base';
import type { StoreToken } from 'flux';
import _ from 'lodash';

import React from 'react';
import ReactDOM from 'react-dom';
import { Styles } from 'material-ui';
// $FlowFixMe
import Popover from 'material-ui/lib/popover/popover';
// $FlowFixMe
import TextField from 'material-ui/lib/text-field';
// $FlowFixMe
import IconButton from 'material-ui/lib/icon-button';
// $FlowFixMe
import { Scrollbars } from 'react-custom-scrollbars';

import Tab from './Tab.jsx';

import APIActions from '../../actions/APIActionCreators';
import API from '../../actions/ASApiActionCreators';
// #needsrefactor the two above should really be merged

import SheetStore from '../../stores/ASSheetStateStore';

type Props = {};
type State = {
  addPopoverOpen: boolean;
  titleFieldError: ?string;
};

type MenuItemContent = {
  disabled: boolean;
  action: Callback;
};

class SheetBrowser extends React.Component {
  props: Props;
  state: State;
  _sheetStoreListener: Callback;
  _createButton: any;
  _createTextField: any;
  _scrollArea: any;

  constructor(props: Props) {
    super(props)
    this.state = {
      addPopoverOpen: false,
      titleFieldError: null,
    };
  }

  componendDidMount() {
    this._sheetStoreListener = () => this.forceUpdate();
    SheetStore.addChangeListener(this._sheetStoreListener);
  }

  componentWillUnmount() {
    SheetStore.removeChangeListener(this._sheetStoreListener);
  }

  render(): React.Element {
    const { addPopoverOpen, titleFieldError } = this.state;
    const mySheets = SheetStore.getMySheets();
    const sharedSheets = SheetStore.getSharedSheets();
    const currentSheet = SheetStore.getCurrentSheetId();

    const popOverAnchor = addPopoverOpen ?
      ReactDOM.findDOMNode(this._createButton)
      :
      <span />
      ;

    if (addPopoverOpen) {
      setTimeout(() => {
        this._createTextField.focus();
      }, 100);
    }

    return (
      <div style={styles.root}>
        <IconButton ref={elem => this._createButton = elem}
                    style={styles.createButton}
                    onClick={() => this.setState({addPopoverOpen: true})}
                    iconClassName="material-icons"
                    tooltip="New sheet"
                    tooltipPosition="top-right"
                    tooltipStyles={styles.addTooltip} >
          add_box
        </IconButton>

        <div style={styles.leftFader} />

        <Popover open={addPopoverOpen}
                 anchorEl={popOverAnchor}
                 anchorOrigin={{horizontal: 'left', vertical: 'top'}}
                 targetOrigin={{horizontal: 'left', vertical: 'bottom'}}>

          <div style={styles.popover}>
            <TextField ref={elem => this._createTextField = elem}
                       style={styles.titleEditor}
                       hintText="Sheet name"
                       errorText={titleFieldError}
                       onKeyDown={(e) => this._onTitleEdit(e)}
                       onBlur={() => this.setState({addPopoverOpen: false})}
                       />
          </div>

        </Popover>


        <Scrollbars style={styles.tabs}
                    ref={elem => this._scrollArea=elem}
                    hideTracksWhenNotNeeded={true}
                    renderTrackVertical={props => this._renderVScroll(props)}
                    renderTrackHorizontal={props => this._renderHScroll(props)}
                    renderView={props => this._renderScrollView(props)}
                    >
          <div style={styles.spacer} />

          {mySheets.map(s =>
            <Tab title={s.sheetName}
                 active={s.sheetId === currentSheet}
                 mutable={true}
                 onSelect={() => APIActions.openSheet(s.sheetId)}
                 onLabelChange={name => API.renameSheet(s.sheetId, name)}
                 onDelete={() => API.deleteSheet(s.sheetId)}
                 key={s.sheetId}
                 />
            )
          }

          {sharedSheets.map(s =>
            <Tab title={s.sheetName + ' [shared]'}
                 active={s.sheetId === currentSheet}
                 mutable={false}
                 onSelect={() => APIActions.openSheet(s.sheetId)}
                 key={s.sheetId}
                 />
            )
          }

          <div style={styles.spacer} />
        </Scrollbars>

        <div style={styles.rightFader} />
      </div>
    );
  }

  _onTitleEdit(e: SyntheticKeyboardEvent) {
    const name = this._createTextField.getValue();
    if (name.split(' ').length > 1) {
      this.setState({titleFieldError: 'Sheet names cannot have spaces.'});
    } else if (e.which === 13) { // enter
      this.setState({addPopoverOpen: false});
      API.newSheet(name);
    } else if (e.which === 27) { // escape
      this._createTextField.setValue('');
      this.setState({addPopoverOpen: false, titleFieldError: null});
    } else {
      this.setState({titleFieldError: null});
    }
  }

  _renderHScroll(props: any): React.Element {
    return <span />;
  }

  _renderVScroll(props: any): React.Element {
    return <span />;
  }

  _renderScrollView({style, ...props}: any): React.Element {
    const finalStyle={
      ...style,
      marginRight: 0,
      marginBottom: 0,
      bottom: -23, // delete extra space due to horiz scrollbar
    };
    return <div style={finalStyle} {...props} />;
  }
}

const height = 30;
const width = 750;

const styles = {
  root: {
    display: 'inline-flex',
    flexDirection: 'row',
    width: '100%',
    height,
  },
  createButton: {
    flexGrow: 0,
    flexBasis: 'content',
    position: 'relative',
    top: '-11%',
    left: -2,
    padding: 0,
  },
  tabs: {
    flexGrow: 1,
    flexBasis: width,
    position: 'relative',
    height,
    overflowY: 'hidden',
    whiteSpace: 'nowrap',
    marginLeft: -10,
  },
  popover: {
  },
  addTooltip: {
    top: 0,
    zIndex: 1000 // to be visible on top of spreadsheet when closed
  },
  spacer: {
    display: 'inline-block',
    width: 5,
  },
  titleEditor: {
    marginLeft: 10,
    marginRight: 10,
  },
  leftFader: {
    position: 'relative',
    left: 'calc(1.5em - 10px)',
    width: '1.5em',
    background: '-webkit-linear-gradient(right, rgba(66,66,66,0) 0%, rgba(66,66,66,1) 100%)',
    zIndex: 10000000
  },
  rightFader: {
    position: 'relative',
    right: '2.5em',
    width: '2.5em',
    background: '-webkit-linear-gradient(left, rgba(66,66,66,0) 0%, rgba(66,66,66,1) 100%)',
    zIndex: 10
  }
};

export default SheetBrowser;
