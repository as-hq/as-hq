/* @flow */

/*
  This controller component displays the detailed output of the active header.
*/

import React from 'react';

import HeaderStore from '../../stores/ASHeaderStore';
import HeaderOutputStore from '../../stores/ASHeaderOutputStore';

import ASOutputPane from './ASOutputPane.jsx';

class ASHeaderPaneController extends React.Component<{}, {}, {}> {
  _storeListener: any;

  constructor(props: {}) {
    super(props);
  }

  componentDidMount() {
    this._headerStoreListener = HeaderStore.addListener(() => this.forceUpdate());
    this._headerOutputStoreListener = HeaderOutputStore.addListener(() => this.forceUpdate());
  }

  componentWillUnmount() {
    this._headerStoreListener.remove();
    this._headerOutputStoreListener.remove();
  }

  render(): React.Element {
    // Get ANSI-formatted string to display
    const language = HeaderStore.getCurrentLanguage();
    const ansiContent = HeaderOutputStore.getOutputInLanguage(language);
    return (
      <ASOutputPane ansiContent={ansiContent}
                    title={`Header: ${language}`} />
    );
  }
}

export default ASHeaderPaneController;
