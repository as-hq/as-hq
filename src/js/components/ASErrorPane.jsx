/* @flow */

import type {
  ASSelection
} from '../types/Eval';

import type {
  ASClientError
} from '../types/Errors';

import React from 'react';

import CellStore from '../stores/ASCellStore';
import SelectionStore from '../stores/ASSelectionStore';

import _ from 'lodash';

type ASErrorPaneProps = {
  style: {[key: string]: any};
};

type ASErrorPaneState = {
  currentSelection: ?ASSelection;
  errors: Array<ASClientError>;
  onlyCurrentCell: boolean;
};

export default class ASErrorPane
  extends React.Component<{}, ASErrorPaneProps, ASErrorPaneState>
{
  constructor(props: ASErrorPaneProps) {
    super(props);

    this.state = {
      currentSelection: null,
      errors: [],
      onlyCurrentCell: true
    };
  }

  componentDidMount() {
    CellStore.addChangeListener(this._handleCellStoreChange.bind(this));
    SelectionStore.addChangeListener(this._handleSelectionChange.bind(this));
  }

  componentWillUnmount() {
    CellStore.removeChangeListener(this._handleCellStoreChange.bind(this));
    SelectionStore.removeChangeListener(this._handleSelectionChange.bind(this));
  }

  render(): React.Element {
    let {style, ...etc} = this.props;

    return ( // TODO
      <div>
      </div>
    );
  }

  _getCurrentErrorList(): Array<ASClientError> {
    if (this.state.onlyCurrentCell) {
      return this.state.errors.filter(
        ({ location }) => _.isEqual(location, this.state.currentSelection)
      );
    } else {
      return this.state.errors;
    }
  }

  _handleCellStoreChange() {
    this.setState({ errors: CellStore.getAllErrors() });
  }

  _handleSelectionChange() {
    this.setState({ currentSelection: SelectionStore.getActiveSelection() });
  }
}
