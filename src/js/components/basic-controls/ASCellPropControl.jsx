/* @flow */

import type {
  NakedRange,
  ASIndex,
  ASCell,
  ASCellProp
} from '../../types/Eval';

import React, {PropTypes} from 'react';
import ASButton from './ASButton.jsx';

import {using} from '../../AS/Maybe';

import Util from '../../AS/Util';
import TC from '../../AS/TypeConversions';
import API from '../../actions/ASApiActionCreators';
import CellStore from '../../stores/ASCellStore';
import SheetStateStore from '../../stores/ASSheetStateStore';
import SelectionStore from '../../stores/ASSelectionStore';

// For display only. When Javascript actually makes it not a pain in the ass
// to export React Classes and type the props this may be of use.
type ASCellPropControlProps<T> = {
  control: ReactElement;
  setControlStateFromCellProp: (prop: ?ASCellProp) => void;
  propTag: string;
  setBackendCellProp: (nextState: T, rng: NakedRange) => void;
};

export default React.createClass({
  componentDidMount() {
    SelectionStore.addChangeListener(this._onActiveCellChange);
    CellStore.addChangeListener(this._onActiveCellChange);
  },

  componentWillUnmount() {
    SelectionStore.removeChangeListener(this._onActiveCellChange);
    CellStore.removeChangeListener(this._onActiveCellChange);
  },

  // fired both when the active selection changes and when the active
  // cell updates.
  _onActiveCellChange() {
    let ac = CellStore.getActiveCell(),
        prop = (ac != null) ? Util.getPropByTag(this.props.propTag, ac) : null;
    this.props.setControlStateFromCellProp(prop);
  },

  // any should be T, but Javascript doesn't let us do that right now.
  onControlStateChange(nextState: any) {
    SelectionStore.withActiveSelection(({range: activeRange}) => {
      this.props.setBackendCellProp(nextState, activeRange);
    });
  },

  render(): React.Element {
    return this.props.control;
  }
});
