/* @flow */

import type {
  ASValue,
  ASLanguage,
  ASSheet
} from '../types/Eval';

import type { StoreToken } from 'flux';
import ASSelection from '../classes/ASSelection';
import ASPoint from '../classes/ASPoint';

import {logError, isTesting} from '../AS/Logger';

import React from 'react';

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

export default class ASEvalPane extends React.Component {
  static defaultProps: {} = {}; 
  props: {};
  state: {};

  _gridStoreToken: StoreToken;
  _configStoreToken: StoreToken;
  _spreadsheet: ASControlledSpreadsheet;

  componentDidMount() {
    this._gridStoreToken = GridStore.addListener(() => this.forceUpdate());
    this._configStoreToken = ConfigStore.addListener(() => this.forceUpdate());
    BrowserTests.install(window, this);
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

    const leftEvalPane = (
      <div style={{display: 'flex', flexDirection: 'column', height: '100%'}}>

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
