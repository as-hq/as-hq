/* @flow */

import type {
  Callback,
} from '../../types/Base';

import type {
  NakedIndex,
} from '../../types/Eval';

import React from 'react';

import {Paper} from 'material-ui';

import _Styles from '../../styles/ASOutputPane';

// $FlowFixMe declaring this is not urgent right now
import AnsiConverter from 'ansi-to-html';

var ansiConverter = new AnsiConverter(); // only need one instance of the converter, so I put it here

import U from '../../AS/Util';
const {
  Conversion: TC,
  Location: L
} = U;

import CellStore from '../../stores/ASCellStore';
import SelectionStore from '../../stores/ASSelectionStore';

import _ from 'lodash';

type ASOutputPaneProps = {
};

type ASOutputPaneState = {
  content: ?string;
};

export default class ASErrorPane
  extends React.Component<{}, ASOutputPaneProps, ASOutputPaneState>
{
  constructor(props: ASOutputPaneProps) {
    super(props);

    this.state = {
      content: null
    };
  }

  componentDidMount() {
    CellStore.addChangeListener(this._handleActiveCellChange.bind(this));
    SelectionStore.addChangeListener(this._handleActiveCellChange.bind(this));
  }

  componentWillUnmount() {
    CellStore.removeChangeListener(this._handleActiveCellChange.bind(this));
    SelectionStore.addChangeListener(this._handleActiveCellChange.bind(this));
  }

  render(): React.Element {
    let {content} = this.state;

    return (
      <Paper style={_Styles.root}>
        {content ?
          this._getFormattedContentHTML(content)
          : <h3>Nothing to display.</h3>}
      </Paper>
    );
  }

  _handleActiveCellChange() {
    this.setState({ content: CellStore.getActiveCellDisplay() });
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
        <div dangerouslySetInnerHTML={{__html: ansiConverter.toHtml(line)}} />
      )
    );
  }
}
