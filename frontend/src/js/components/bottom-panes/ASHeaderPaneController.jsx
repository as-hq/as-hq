/* @flow */

/*
  This controller component displays the detailed output of the active header.
*/

import React from 'react';

import HeaderStore from '../../stores/ASHeaderStore';

import ASOutputPane from './ASOutputPane.jsx';

class ASHeaderPaneController extends React.Component<{}, {}, {}> {
  _storeListener: any;

  constructor(props: {}) {
    super(props);
  }

  componentDidMount() {
    this._storeListener = HeaderStore.addListener(() => this.forceUpdate());
  }

  componentWillUnmount() {
    this._storeListener.remove();
  }

  render(): React.Element {
    // Get ANSI-formatted string to display
    const ansiContent = HeaderStore.getCurrentOutput();
    const language = HeaderStore.getCurrentLanguage();

    debugger;
    return (
      <ASOutputPane ansiContent={ansiContent}
                    title={`Header: ${language}`} />
    );
  }
}

export default ASHeaderPaneController;
