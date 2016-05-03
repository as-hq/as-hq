// @flow

import type {
  Callback
} from '../types/Base';

import type {
  StoreLink
} from '../types/React';

import React from 'react';
import U from '../AS/Util';

import type { StoreToken } from 'flux';

import SheetStateStore from '../stores/ASSheetStateStore';
import CellStore from '../stores/ASCellStore';
import GridStore from '../stores/ASGridStore';
import ConfigActions from '../actions/ASConfigActionCreators';
import ASBottomBar from './ASBottomBar.jsx';

type Props = {
  toggleBottomPane: Callback<string>;
};

export default class ASBottomBarController extends React.Component {
  static defaultProps = {}; 
  props: {};
  state: {};
  _gridStoreListener: StoreToken;
  _cellStoreListener: StoreToken;
  _sheetsListener: () => void;

  $storeLinks: Array<StoreLink>;

  constructor(props: Props) {
    super(props);
    this._sheetsListener = () => this.forceUpdate();
  }

  componentDidMount() {
    U.React.addStoreLinks(this, [
      { store: SheetStateStore },
    ]);
    SheetStateStore.addListener('GOT_MY_SHEETS', this._sheetsListener);
    this._gridStoreListener = GridStore.addListener(() => this.forceUpdate());
    this._cellStoreListener = CellStore.addListener(() => this.forceUpdate());
  }

  componentWillUnmount() {
    U.React.removeStoreLinks(this);
    SheetStateStore.removeListener('GOT_MY_SHEETS', this._sheetsListener);
    this._gridStoreListener.remove();
    this._cellStoreListener.remove();
  }

  render(): React.Element {
    const errorIconStyle  = CellStore.activeCellHasError() ? {color: "orange"} : {};
    const outputIconStyle = CellStore.activeCellHasOutput() ? {color: "orange"} : {};
    const objectViewerIconStyle = CellStore.activeCellIsObject() ? {color: "orange"} : {};
    const onErrorIconClick = () => ConfigActions.toggleBottomPane('errors');
    const onOutputIconClick = () => ConfigActions.toggleBottomPane('cell_output');
    const onHeaderIconClick = () => ConfigActions.toggleBottomPane('header_output');
    const onObjectViewerClick = ()=> ConfigActions.toggleBottomPane('object_viewer');
    return <ASBottomBar
              errorIconStyle={errorIconStyle}
              outputIconStyle={outputIconStyle}
              objectViewerIconStyle={objectViewerIconStyle}
              sheetName={SheetStateStore.getCurrentSheetTitle()}
              onErrorIconClick={onErrorIconClick}
              onOutputIconClick={onOutputIconClick}
              onHeaderIconClick={onHeaderIconClick}
              onObjectViewerClick={onObjectViewerClick} />;
  }
}
