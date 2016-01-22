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
import SelectionStore from '../../stores/ASSelectionStore';
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

  constructor(props: ASErrorPaneControllerProps) {
    super(props);

    this.state = {
      onlyShowCurSelErrs: true,
    };
  }

  componentDidMount() {
    Util.React.addStoreLinks(this, [
      { store: SelectionStore }, 
      { store: CellStore }
    ]);
  }

  componentWillUnmount() {
    Util.React.removeStoreLinks(this);
  }

  render(): React.Element {
    let errors = this._getCurrentErrorList();
    let {onlyShowCurSelErrs} = this.state;

    return <ASErrorPane 
      errors={errors}
      onlyShowCurSelErrs={onlyShowCurSelErrs}
      onErrorSelect={(errRow) => this._onErrorSelect(errRow)}
      showAllValueLink={{
        value: onlyShowCurSelErrs,
        requestChange: (onlyShowCurSelErrs) => { this.setState({onlyShowCurSelErrs}); }
      }} />;
  }

  _getCurrentErrorList(): Array<ASClientError> {
    const errors = CellStore.getAllErrors(),
          {onlyShowCurSelErrs} = this.state,
          currentSelection = SelectionStore.getActiveSelection();

    if (onlyShowCurSelErrs && currentSelection) {
      return errors.filter(({location}) => location.isInRange(currentSelection.range));
    } else {
      return errors;
    }
  }

  // What to do after you click on the nth error in the list of errors in the pane. 
  _onErrorSelect(errRow: number) {
    let errors = this._getCurrentErrorList();
    let {location} = errors[errRow];
 
    this.props.selectCellAtLocation(location);
  }
}