/* @flow */

import React from 'react';
import ReactDOM from 'react-dom';
import {Paper} from 'material-ui';

import U from '../AS/Util';

// $FlowFixMe
import IconButton from 'material-ui/lib/icon-button';

import SheetBrowser from './sheet-browser/SheetBrowser.jsx';

import Focusable from './transforms/Focusable.jsx';
import {bottomBar as bottomBarZIndex} from '../styles/zIndex';

import _ from 'lodash';

import FocusActions from '../actions/ASFocusActionCreators';


window.dom = ReactDOM;

type Props = {
  errorIconStyle: any;
  outputIconStyle: any;
  sheetName: string;
  onErrorIconClick: () => void;
  onHeaderIconClick: () => void;
  onOutputIconClick: () => void;
};

function LabeledIconButton(props: {
  label: string;
  onClick: () => void;
  contentStyle: any;
  tooltip: string;
  iconClassName: string;
}): React.Element {
  const {label, onClick, contentStyle, tooltip, iconClassName} = props;

  return <div style={styles.button}>
    <IconButton
      style={styles.iconButton}
      iconStyle={contentStyle}
      onClick={onClick}
      iconClassName="material-icons"
      tooltip={tooltip}
      tooltipPosition="top-left"
      tooltipStyles={styles.tooltip} >
      {iconClassName}
    </IconButton>
  </div>;
}

class BottomBar extends React.Component {
  props: Props;
  state: {};
  _sheetBrowser: any;
  _onFocus: () => void;

  constructor(props: Props) {
    super(props);
    // Overridden by the Focusable HOC
    this._onFocus = () => {};
  }

  shouldComponentUpdate(nextProps: Props, nextState: {}): boolean {
    // have to check this manually because we can't compare functions (which get passed as props)
    return !(_.isEqual(nextProps.errorIconStyle, this.props.errorIconStyle) &&
             _.isEqual(nextProps.outputIconStyle, this.props.outputIconStyle) &&
             nextProps.sheetName === this.props.sheetName);
  }

  render(): React.Element {
    const {errorIconStyle, outputIconStyle, sheetName,
           onErrorIconClick, onOutputIconClick, onHeaderIconClick} = this.props;

    return (
      <Paper style={styles.root}
             onMouseEnter={() => this._onEnter()}
             onMouseLeave={() => this._onLeave()}
             onClick={() => this._onFocus()}
             >

        <div style={styles.sheetBrowser}>
          <SheetBrowser ref={elem => this._sheetBrowser = elem}
                        />
        </div>

        <LabeledIconButton
          contentStyle={errorIconStyle}
          onClick={onErrorIconClick}
          tooltip={`Errors (${U.Browser.metaKeyName()}+Alt+E)`}
          iconClassName="error_outline"
          label="Errors"
        />

        <LabeledIconButton
          contentStyle={outputIconStyle}
          onClick={onOutputIconClick}
          tooltip={`Cell output (${U.Browser.metaKeyName()}+Alt+O)`}
          iconClassName="label_outline"
          label="Cell output"
        />

        <LabeledIconButton
          onClick={onHeaderIconClick}
          tooltip={`Header output (${U.Browser.metaKeyName()}+Alt+H)`}
          iconClassName="input"
          label="Header output"
        />

      </Paper>
    );
  }

  _takeFocus() {
    ReactDOM.findDOMNode(this._sheetBrowser).focus();
  }

  _onEnter() {
    FocusActions.hover(name);
    FocusActions.focus(name);
  }

  _onLeave() {
    FocusActions.unhover(name);
    FocusActions.returnFocus();
  }
}

const styles = {
  root: {
    position: 'relative',
    display: 'flex',
    flexDirection: 'row',
    height: 30,
    background: '#424242',
    zIndex: bottomBarZIndex,
  },

  sheetBrowser: {
    position: 'relative',
    flexGrow: 1
  },

  button: {
    position: 'relative',
    float: 'right',
    marginTop: 5,
  },

  iconButton: {
    display: 'inline-block',
    width: 40,
    paddingTop: 0
  },

  buttonLabel: {
    position: 'relative',
    display: 'inline-block',
    color: '#ffffff',
    fontWeight: 500,
  },

  tooltip: {
    width: 'fit-content',
    top: -10,
    right: -2,
  },
};

const name = 'bottombar';

export default Focusable(BottomBar, {
  name,
  addFocusListener: (component, listener) => {
    component._onFocus = () => listener();
  },
  takeFocus: (component) => component._takeFocus()
});
