/* @flow */

import type {
  NakedRange,
  ASIndex,
  ASCell
} from '../../types/Eval';

import React, {PropTypes} from 'react';
import ASButton from './ASButton.jsx';

import Util from '../../AS/Util';
import TC from '../../AS/TypeConversions';
import API from '../../actions/ASApiActionCreators';
import CellStore from '../../stores/ASCellStore';
import SheetStateStore from '../../stores/ASSheetStateStore';
import SelectionStore from '../../stores/ASSelectionStore';

export default React.createClass({
  componentDidMount() {
    SelectionStore.addChangeListener(this._onSelectionStoreChange);
  },

  componentWillUnmount() {
    SelectionStore.removeChangeListener(this._onSelectionStoreChange);
  },

  getInitialState() {
    return ({
      active: false
    });
  },

  render(): ReactElement {
    let {cellProp, iconClassName, ...etc} = this.props;
    let {active} = this.state;

    return (
      <ASButton
        height="24px"
        primary={active}
        iconClassName={iconClassName}
        {...etc}
      />
    );
  },

  _onToggleClick() {
    let {cellProp} = this.props;
    let activeSelection = SelectionStore.getActiveSelection();
    if (! activeSelection) {
      throw new Error('Tried to toggle on nonexistent selection');
    }

    API.toggleProp(cellProp, activeSelection.range);
  },

  _onSelectionStoreChange() {
    // this is like this so that future refactors are easier

    this._onCellChange();
  },

  /* Following gets called when new cell or range selected
   * or when selected cell is changed. */
  _onCellChange() {
    // just computes whether all of them have the same prop
    // if not, set active to false

    let activeSelection = SelectionStore.getActiveSelection();
    if (! activeSelection) {
      throw new Error('Cell changed without active selection');
    }
    console.log('selection', activeSelection)

    let newIndices = TC.rangeToIndices(activeSelection.range);
    console.log('indices', newIndices);
    let newCells = Util.removeEmpty(newIndices.map(
      ({col, row}) => CellStore.getCell(col, row)
    ));
    console.log('new cells', newCells);
    let active = newCells.map((cell) =>
      {
        let ret = Util.cellPropIsActive(this.props.cellProp, cell);
        console.log('cell prop is active', ret);
        return ret;
      }
    ).every((val) => val) && (newCells.length > 0);
    this.setState({
      active: active
    });
  }
});
