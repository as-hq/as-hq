/* @flow */

import type {
  Callback,
} from '../../types/Base';

import React from 'react';

import {Paper} from 'material-ui';

import _Styles from '../../styles/ASOutputPane';

// $FlowFixMe declaring this is not urgent right now
import Ansi from 'ansi_up';

import U from '../../AS/Util';

import ASIndex from '../../classes/ASIndex';

import CellStore from '../../stores/ASCellStore';
import SelectionStore from '../../stores/ASSelectionStore';

import _ from 'lodash';

type ASOutputPaneProps = {
};

type ASOutputPaneState = {
  content: ?string;
  selection: ?string;
};

export default class ASErrorPane
  extends React.Component<{}, ASOutputPaneProps, ASOutputPaneState>
{
  constructor(props: ASOutputPaneProps) {
    super(props);

    this.state = {
      content: null,
      selection: null
    };
  }

  componentDidMount() {
    CellStore.addChangeListener(this._handleCellChange.bind(this));
    SelectionStore.addChangeListener(this._handleSelectionChange.bind(this));
  }

  componentWillUnmount() {
    CellStore.removeChangeListener(this._handleCellChange.bind(this));
    SelectionStore.addChangeListener(this._handleSelectionChange.bind(this));
  }

  render(): React.Element {
    let {content, selection} = this.state;

    return (
      <div style={_Styles.root} >

        <Paper style={_Styles.topBar}>
          <span style={_Styles.topBarTitle}>
            {`Active cell: ${selection || ''}`}
          </span>
        </Paper>

        <Paper style={_Styles.contentPane}>
          <div style={_Styles.contentContainer} >
            {content ?
              this._getFormattedContentHTML(content)
              : <h3 style={_Styles.altMessage}>Nothing to display.</h3>}
          </div>
        </Paper>

      </div>
    );
  }

  _handleCellChange() {
    this.setState({ content: CellStore.getActiveCellDisplay() });
  }

  _handleSelectionChange() {
    this._handleCellChange(); // also get the new active cell's display
    let sel = SelectionStore.getActiveSelection();
    if (!!sel) {
      let {origin} = sel;
      let rng = origin.toRange();
      this.setState({
        selection: rng.toExcel().toString()
      });
    }
  }

  _getFormattedContentHTML(content: string): Array<HTMLElement> {
    return content
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;')
            .split('\n')
            .map((line) =>
      (
        <div
          style={_Styles.outputLine}
          dangerouslySetInnerHTML={
            {__html: Ansi.ansi_to_html(line, {use_classes: true})}
          } />
      )
    );
  }
}
