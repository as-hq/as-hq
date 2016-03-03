/* @flow */

import type {
  Callback
} from '../types/Base';

import type {
  StoreLink
} from '../types/React';

import type {
  RoutedComponentProps
} from '../types/Router';

import type { BottomPane } from '../types/State';
import type { StoreToken } from 'flux';

import {logDebug} from '../AS/Logger';

import React, {PropTypes} from 'react';
import ASEvaluationPane from './ASEvaluationPane.jsx';
import ASTopBar from './ASTopBar.jsx';
import ASConnectionBar from './ASConnectionBar.jsx';
import ASBottomBar from './ASBottomBar.jsx';

import ASCondFormattingDialog from './cond-formatting/ASCondFormattingDialog.jsx';
import ASChartDialog from './chart/ASChartDialog.jsx';

import ResizablePanel from './ResizablePanel.jsx';
import Toolbar from './toolbar/Toolbar.jsx';
import ShortcutHelper from './shortcut-helper/ShortcutHelper.jsx';

import ASErrorPaneController from './bottom-panes/ASErrorPaneController.jsx';
import ASCellPaneController from './bottom-panes/ASCellPaneController.jsx';
import ASHeaderPaneController from './bottom-panes/ASHeaderPaneController.jsx';

import U from '../AS/Util';
import * as flex from '../styles/flex';

import { actions as Shortcuts } from '../AS/Shortcuts';
import API from '../actions/ASApiActionCreators';
import DialogActions from '../actions/DialogActionCreators';
import OverlayActions from '../actions/ASOverlayActionCreators';
import ConfigActions from '../actions/ASConfigActionCreators';
import ClipboardActions from '../actions/ASClipboardActionCreators';

import ConfigStore from '../stores/ASConfigurationStore';
import HeaderOutputStore from '../stores/ASHeaderOutputStore';
import LogStore from '../stores/ASLogStore';
import ModalStore from '../stores/ASModalStore';

// $FlowFixMe: missing annotations
import ThemeManager from 'material-ui/lib/styles/theme-manager';
// $FlowFixMe: missing annotations
import DarkTheme from 'material-ui/lib/styles/raw-themes/dark-raw-theme';

import ASCodeField from './basic-controls/ASCodeField.jsx';

// $FlowFixMe
require('react-tap-event-plugin')();

// install shorctuts
Shortcuts.installAll();

type Props = RoutedComponentProps;

class App extends React.Component<{}, Props, {}> {
  $storeLinks: Array<StoreLink>;
  _configListener: StoreToken;
  _copyHandler: Callback<SyntheticClipboardEvent>;
  _cutHandler: Callback<SyntheticClipboardEvent>;
  _pasteHandler: Callback<SyntheticClipboardEvent>;

  constructor(props: Props) {
    super(props);
    this.$storeLinks = [];
    this._logListener = () => this.forceUpdate();
    this._copyHandler = evt => ClipboardActions.copy(evt);
    this._cutHandler = evt => ClipboardActions.cut(evt);
    this._pasteHandler = evt => ClipboardActions.paste(evt);
  }

  componentDidMount() {
    U.React.addStoreLinks(this, [
      { store: ModalStore }
    ]);
    LogStore.addListener(this._logListener);
    this._configListener = ConfigStore.addListener(() => this.forceUpdate());

    // TODO (anand) what does this do?
    window.addEventListener('contextmenu', (evt) => {
      evt.preventDefault();
    });

    // add clipboard event listeners
    window.addEventListener('copy', this._copyHandler);
    window.addEventListener('cut', this._cutHandler);
    window.addEventListener('paste', this._pasteHandler);
  }

  componentWillUnmount() {
    U.React.removeStoreLinks(this);
    LogStore.removeListener(this._logListener);
    this._configListener.remove();
    window.removeEventListener('copy', this._copyHandler);
    window.removeEventListener('cut', this._cutHandler);
    window.removeEventListener('paste', this._pasteHandler);
  }

  getChildContext(): any {
    return {
      muiTheme: ThemeManager.getMuiTheme(DarkTheme)
    };
  }

  render(): React.Element {
    const logOpen = LogStore.getIsOpen();
    const isConnected = ConfigStore.isConnected();
    const bottomPane = ConfigStore.getCurrentBottomPane();

    const centerContent = (
      <div style={styles.full}>
        <ASEvaluationPane />
      </div>
    );

    const bottomContent = (
      <div style={styles.full}>
        { this._getBottomPane(bottomPane) }
      </div>
    );

    const main = (
      <div style={{...flex.column, height: logOpen ? '50%' : '100%'}} >
        {isConnected ?
          <noscript/> :
          <div style={styles.connectionBar}>
            <ASConnectionBar />
          </div>
        }

        <ASCondFormattingDialog
          open={ModalStore.getCondFormattingOpen()}
          onRequestClose={() => DialogActions.closeCondFormattingDialog()} />

        <ASChartDialog
          open={ModalStore.getChartingOpen()}
          onRequestClose={() => DialogActions.closeChartingDialog()}
          onCreate={(chart) => OverlayActions.add(chart)} />

        <ASTopBar toggleEvalHeader={() => ConfigActions.toggleHeader()} />

        <Toolbar />

        <div style={styles.resizable}>
          <ResizablePanel content={centerContent}
                          sidebar={bottomContent}
                          sidebarVisible={!! bottomPane}
                          side="bottom" />
        </div>

        <div style={styles.bottomBar} >
          <ASBottomBar />
        </div>
        <ShortcutHelper />
      </div>
    );

    // The log viewer can be open, in which case we get a split view, or closed, in which case the sheet
    // is the whole page
    return (
      <div style={{width: '100%', height: '100%'}} >
        {main}
        {logOpen ? <LogViewer /> : null}
      </div>
    );
  }


  _getBottomPane(pane: BottomPane): ReactElement {
    switch(pane) {
      case 'errors': {
        // TODO (michael/anand) this component is pretty fucked.
        return <ASErrorPaneController />;
      }
      case 'header_output': {
        return <ASHeaderPaneController />;
      }
      case 'cell_output': {
        return <ASCellPaneController />;
      }
      default: {
        return <noscript />;
      }
    }
  }
}

const topBarHeight = 60;
const toolbarHeight = 50;
const connectionBarHeight = 24;

const styles = {
  app: {
    ...flex.column,
    height: '100%',
  },
  full: {
    height: '100%',
  },
  connectionBar: {
    height: connectionBarHeight,
  },
  resizable: {
    height: `calc(100% - ${
      toolbarHeight + topBarHeight
    }px)`,
  },
  bottomBar: {
    height: 24,
  },
};

App.childContextTypes = {
  muiTheme: React.PropTypes.object
};

export default App;
