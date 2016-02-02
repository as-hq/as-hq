/* @flow */

/*
  This controller component displays the detailed output of the active cell.
*/

import React from 'react';

import CellStore from '../../stores/ASCellStore';
import SelectionStore from '../../stores/ASSelectionStore';

import ASOutputPane from './ASOutputPane.jsx';

class ASCellPaneController extends React.Component<{}, {}, {}> {
  _storeListener: () => void;
  _cellStoreToken: { remove: () => void };

  constructor(props: {}) {
    super(props);
    this._storeListener = () => this.forceUpdate();
  }

  componentDidMount() {
    this._cellStoreToken = CellStore.addListener(this._storeListener);
    SelectionStore.addChangeListener(this._storeListener);
  }

  componentWillUnmount() {
    this._cellStoreToken.remove();
    SelectionStore.removeChangeListener(this._storeListener);
  }

  render(): React.Element {
    // Get ANSI-formatted string to display
    const ansiContent = CellStore.getActiveCellDisplay();
    const originStr = this._getOriginString();

    return (
      <ASOutputPane ansiContent={ansiContent}
                    title={`Active cell: ${originStr}`} />
    );
  }

  // returns the stringified 'origin' of the selection.
  // e.g. if I have A1:B5 selected, the origin 'A1' is returned.
  _getOriginString(): string {
    const sel = SelectionStore.getActiveSelection();
    if (!! sel) {
      return sel.origin.toExcel().toString();
    } else {
      return 'none';
    }
  }
}

export default ASCellPaneController;
