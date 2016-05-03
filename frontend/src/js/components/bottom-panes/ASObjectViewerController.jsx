/* @flow */

import type {
  Callback
} from '../../types/Base';

import type {
  ASClientError
} from '../../types/Errors';

import type {
  StoreLink, 
  StoreToken
} from '../../types/React';

import CellStore from '../../stores/ASCellStore';
import GridStore from '../../stores/ASGridStore';
import ViewStore from '../../stores/ASObjectViewerStore';
import Util from '../../AS/Util';
import React from 'react';
import ASIndex from '../../classes/ASIndex';
import ASOutputPane from './ASOutputPane.jsx';

import API from '../../actions/ASApiActionCreators';

type ASObjectViewerControllerState = {
};

export default class ASObjectViewerController extends React.Component {
  static defaultProps: {} = {}; 
  props: {};
  state: ASObjectViewerControllerState;

  $storeLinks: Array<StoreLink>;
  _cellStoreToken: StoreToken;
  _gridStoreToken: StoreToken;
  _viewStoreToken: StoreToken;

  constructor(props: {}) {
    super(props);
  }

  componentDidMount() {
    this._cellStoreToken = CellStore.addListener(() => this.forceUpdate());
    this._gridStoreToken = GridStore.addListener(() => this.forceUpdate());
    this._viewStoreToken = ViewStore.addListener(() => this.forceUpdate());
  }

  componentWillUnmount() {
    this._cellStoreToken.remove();
    this._gridStoreToken.remove();
    this._viewStoreToken.remove();
  }

  render(): React.Element {
    const ansiContent = this._getContentString();
    const originStr   = this._getOriginString();

    return (
      <ASOutputPane ansiContent={ansiContent}
                    title={`Active cell: ${originStr}`} />
    );
  }

  // returns the stringified 'origin' of the selection.
  // e.g. if I have A1:B5 selected, the origin 'A1' is returned.
  _getOriginString(): string {
    const sel = GridStore.getActiveSelection();
    if (!! sel) {
      return sel.origin.toExcel().toString();
    } else {
      return 'none';
    }
  }

  _getContentString(): string {
    const loc = GridStore.getActiveSelection().origin;
    const view = ViewStore.getObjectViewAt(loc);
    if (view === null) {
      return "No object view";
    } else return view;
  }

}
