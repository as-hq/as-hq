// @flow

import type {
  Callback
} from '../types/Base';

import type {
  StoreLink
} from '../types/React';

import React from 'react';
import U from '../AS/Util';

import SheetStateStore from '../stores/ASSheetStateStore';
import CellStore from '../stores/ASCellStore';
import GridStore from '../stores/ASGridStore';
import ConfigActions from '../actions/ASConfigActionCreators';
import ASBottomBar from './ASBottomBar.jsx';

type Props = {
  toggleBottomPane: Callback<string>;
};

export default class ASBottomBarController extends React.Component<{}, Props, {}> {
  $storeLinks: Array<StoreLink>;
  _gridStoreListener: StoreToken;
  _cellStoreListener: StoreToken;

  componentDidMount() {
    U.React.addStoreLinks(this, [
      { store: SheetStateStore },
    ]);

    this._gridStoreListener = GridStore.addListener(() => this.forceUpdate());
    this._cellStoreListener = CellStore.addListener(() => this.forceUpdate());
  }

  componentWillUnmount() {
    U.React.removeStoreLinks(this);
    this._gridStoreListener.remove();
    this._cellStoreListener.remove();
  }

  render(): React.Element {
    const errorIconStyle  = CellStore.activeCellHasError() ? {color: "orange"} : {},
          outputIconStyle = CellStore.activeCellHasOutput() ? {color: "orange"} : {}, 
          onErrorIconClick = () => ConfigActions.toggleBottomPane('errors'), 
          onOutputIconClick = () => ConfigActions.toggleBottomPane('cell_output'),
          onHeaderIconClick = () => ConfigActions.toggleBottomPane('header_output');

    return <ASBottomBar 
              errorIconStyle={errorIconStyle}
              outputIconStyle={outputIconStyle}
              onErrorIconClick={onErrorIconClick}
              onOutputIconClick={onOutputIconClick}
              onHeaderIconClick={onHeaderIconClick} />;
  }
}
