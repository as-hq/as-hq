/* @flow */

import type {
  Callback
} from '../../types/Base';

import type {
  ASClientError
} from '../../types/Errors';

import type {
  StoreLink
} from '../../types/React';

import CellStore from '../../stores/ASCellStore';
import GridStore from '../../stores/ASGridStore';
import Util from '../../AS/Util';
import React from 'react';
import ASIndex from '../../classes/ASIndex';
import ASErrorPane from './ASErrorPane.jsx';

type ASErrorPaneControllerProps = {
  selectCellAtLocation: Callback<ASIndex>;
};

type ASErrorPaneControllerState = {
  onlyShowCurSelErrs: boolean;
};

export default class ASErrorPaneController
  extends React.Component<{}, ASErrorPaneControllerProps, ASErrorPaneControllerState>
{
  $storeLinks: Array<StoreLink>;
  _cellStoreToken: StoreToken;
  _gridStoreToken: StoreToken;

  constructor(props: ASErrorPaneControllerProps) {
    super(props);

    this.state = {
      onlyShowCurSelErrs: true,
    };
  }

  componentDidMount() {
    this._cellStoreToken = CellStore.addListener(() => this.forceUpdate());
    this._gridStoreToken = GridStore.addListener(() => this.forceUpdate());
  }

  componentWillUnmount() {
    this._cellStoreToken.remove();
    this._gridStoreToken.remove();
  }

  render(): React.Element {
    const errors = this._getCurrentErrorList();
    const {onlyShowCurSelErrs} = this.state;

    return (
      <ASErrorPane
        errors={errors}
        onlyShowCurSelErrs={onlyShowCurSelErrs}
        showAllValueLink={{
          value: onlyShowCurSelErrs,
          requestChange: (onlyShowCurSelErrs) => {
            this.setState({onlyShowCurSelErrs});
          }}}
      />
    );
  }

  _getCurrentErrorList(): Array<ASClientError> {
    const errors = CellStore.getAllErrors();
    const {onlyShowCurSelErrs} = this.state;
    const currentSelection = GridStore.getActiveSelection();

    if (onlyShowCurSelErrs && currentSelection) {
      return errors.filter(({location}) => location.isInRange(currentSelection.range));
    } else {
      return errors;
    }
  }
}
