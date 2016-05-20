/* @flow */

import type {
  ASValue,
  ASLanguage,
} from '../types/Eval';

import type { StoreToken } from 'flux';
import ASSelection from '../classes/ASSelection';
import ASPoint from '../classes/ASPoint';

import {logError, isTesting} from '../AS/Logger';

import React from 'react';
// $FlowFixMe
import CircularProgress from 'material-ui/lib/circular-progress';

import GridStore from '../stores/ASGridStore';
import FindStore from '../stores/ASFindStore';
import FocusStore from '../stores/ASFocusStore';
import ConfigStore from '../stores/ASConfigurationStore';

import API from '../actions/ASApiActionCreators';
import GridActions from '../actions/ASGridActionCreators';
import ConfigActions from '../actions/ASConfigActionCreators';
import FindActions from '../actions/ASFindActionCreators';

import * as BrowserTests from '../browser-test/index';

import ASCodeEditor from './ASCodeEditor.jsx';
import ASControlledSpreadsheet from './ASControlledSpreadsheet.jsx';
import EvalHeaderController from './eval-header/EvalHeaderController.jsx'
import ResizablePanel from './ResizablePanel.jsx'
import ASFindBar from './ASFindBar.jsx';
import ASFindModal from './ASFindModal.jsx';

type State = {
  shouldShowSheetLoader: boolean;
};
type Props = {};

export default class ASEvalPane extends React.Component {
  static defaultProps: Props = {};
  props: Props;
  state: State;

  _gridStoreToken: StoreToken;
  _configStoreToken: StoreToken;
  _spreadsheet: ASControlledSpreadsheet;

  constructor(props: Props) {
    super(props);
    this.state = {
      shouldShowSheetLoader: false,
    };
  }

  componentDidMount() {
    BrowserTests.install(window, this);
    this._gridStoreToken = GridStore.addListener(() => this.forceUpdate());
    this._configStoreToken = ConfigStore.addListener(() => {
      this.forceUpdate();

      // Perform two checks if we should show the sheet loader.
      // (1) immediately
      // (2) after 200ms.
      // This has the effect of showing the loader for sheets taking >200ms to load.
      if (ConfigStore.isSheetLoading()) {
        setTimeout(() => {
          if (ConfigStore.isSheetLoading()) {
            this.setState({shouldShowSheetLoader: true});
          }
        }, 200);
      } else {
        this.setState({shouldShowSheetLoader: false});
      }
    });
  }

  componentWillUnmount() {
    API.close();
    this._gridStoreToken.remove();
    this._configStoreToken.remove();
  }

  render() {
    const headerOpen = ConfigStore.isHeaderOpen();
    const findBarOpen = ConfigStore.isFindBarOpen();
    const findModalOpen = ConfigStore.isFindModalOpen();
    const { shouldShowSheetLoader } = this.state;

    const leftEvalPane = (
      <div style={styles.root}>

        {shouldShowSheetLoader &&
          <div style={styles.loaderContainer}>
            <h1 style={styles.loaderTitle}>Loading sheet</h1>
            <CircularProgress size={3} color='white' />
          </div>
        }

        {findBarOpen &&
          <ASFindBar
            onEnter={() => API.find(FindStore.getFindText())}
            onNext={() => FindActions.incrementSelection()}
            onPrev={() => FindActions.decrementSelection()}
            onClose={() => ConfigActions.closeFindBar()}
            onModal={() => ConfigActions.closeFindModal}
          />
        }

        {findModalOpen &&
          <ASFindModal
            initialSelection={0}
            onClose={() => ConfigActions.closeFindModal()}
          />
        }


        <ASCodeEditor />

        <ASControlledSpreadsheet
          ref={elem => this._spreadsheet = elem}
          selection={{
            value: GridStore.getActiveSelection(),
            requestChange(selection: ASSelection) {
              GridActions.select(selection);
            }
          }}
          scroll={{
            value: GridStore.getScroll(),
            requestChange(scroll: ASPoint) {
              GridActions.scrollTo(scroll);
            }
          }}
          height="100%"  />

      </div>
    );

    return (
        <ResizablePanel content={leftEvalPane}
                        sidebar={( <EvalHeaderController open={headerOpen} /> )}
                        sidebarVisible={headerOpen} />
    );
  }
}

const styles = {
  root: {
    display: 'flex',
    flexDirection: 'column',
    height: '100%'
  },
  loaderContainer: {
    position: 'absolute',
    height: '100%',
    width: '100%',
    zIndex: 1000,
    backgroundColor: 'rgba(0, 0, 0, 0.5)',
    textAlign: 'center',
  },
  loaderTitle: {
    color: 'white',
    paddingTop: '10%'
  },
};
