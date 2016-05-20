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

import WorkbookStore from '../stores/ASWorkbookStore';
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
  _workbookStoreListener: StoreToken;

  componentDidMount() {
    this._workbookStoreListener = WorkbookStore.addListener(() => this.forceUpdate());
    this._gridStoreListener = GridStore.addListener(() => this.forceUpdate());
    this._cellStoreListener = CellStore.addListener(() => this.forceUpdate());
  }

  componentWillUnmount() {
    this._workbookStoreListener.remove();
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
              sheetName={WorkbookStore.getCurrentSheetTitle()}
              onErrorIconClick={onErrorIconClick}
              onOutputIconClick={onOutputIconClick}
              onHeaderIconClick={onHeaderIconClick}
              onObjectViewerClick={onObjectViewerClick} />;
  }
}
