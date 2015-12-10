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

//
type ASCellPropControlProps<T> = {
  control: ReactElement;
  setControlStateFromCellProp: (prop: ?ASCellProp) => void;
  propTag: string;
  setBackendCellProp: (rng: NakedRange) => void;
};

class ASCellPropControl<T>
  extends React.Component<{}, ASCellPropControlProps<T>, {}>
{
  componentDidMount() {
    SelectionStore.addChangeListener(this._onActiveCellChange);
    CellStore.addChangeListener(this._onActiveCellChange);
  }

  componentWillUnmount() {
    SelectionStore.removeChangeListener(this._onActiveCellChange);
    CellStore.removeChangeListener(this._onActiveCellChange);
  }

  // fired both when the active selection changes and when the active
  // cell updates.
  _onActiveCellChange() {
    let ac = SheetStateStore.getActiveCell();
    if (!!ac) {
      let prop = Util.getPropByTag(this.props.propTag, ac);
      this.props.setControlStateFromCellProp(prop);
    }
  }

  onControlStateChange() {
    SelectionStore.withActiveSelection(({range: activeRange}) => {
      this.props.setBackendCellProp(activeRange);
    })
  }

  render(): React.Element {
    return this.props.control;
  }
}
