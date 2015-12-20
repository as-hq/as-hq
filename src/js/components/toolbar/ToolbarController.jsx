/* @flow */

import type {
  NakedRange,
  ASIndex,
  ASCell,
  ASCellProp
} from '../../types/Eval';

import React, {PropTypes} from 'react';
import _ from 'lodash';

import U from '../../AS/Util';
let {
  Conversion: TC
} = U;


import API from '../../actions/ASApiActionCreators';
import CellStore from '../../stores/ASCellStore';
import SheetStateStore from '../../stores/ASSheetStateStore';
import SelectionStore from '../../stores/ASSelectionStore';

/*
This component is a higher-order-component built for easy communication between stores and controls that monitor props
*/

// For display only. When Javascript actually makes it not a pain in the ass
// to export React Classes and type the props this may be of use.
type ASCellPropControlProps<T> = {
  control: ReactElement;
  setControlStateFromCellProp: (prop: ?ASCellProp) => void;
  propTag: string;
  setBackendCellProp: (nextState: T, rng: NakedRange) => void;
};

export default React.createClass({

  /* We need to know the control, tag of the prop (Bold), how to update control state if stores change, and how to update backend if 
  control changes */
  propTypes: {
    propTag: React.PropTypes.string.isRequired,
    setControlStateFromCellProp: React.PropTypes.func.isRequired,
    setBackendCellProp: React.PropTypes.func.isRequired,
    control: React.PropTypes.object.isRequired
  },

  componentDidMount() {
    SelectionStore.addChangeListener(this._onActiveCellChange);
    CellStore.addChangeListener(this._onActiveCellChange);
  },

  componentWillUnmount() {
    SelectionStore.removeChangeListener(this._onActiveCellChange);
    CellStore.removeChangeListener(this._onActiveCellChange);
  },

  // Keep track of previous state so that we don't send redundant messages
  getInitialState() {
    return {
      activeCell: null, 
      activeCellProp: null
    }
  },

  /* 
    When the active selection or cell change, get the active cell's prop via the propTag and tell the
    underlying control to change its state based on that prop. For example, change loc to A5; if A5 is bold, 
    the bold button needs to be pushed in. Only send update if something relevant changed.
  */
  _onActiveCellChange() {
    let ac = CellStore.getActiveCell(),
        prop = (ac != null) ? U.Cell.getPropByTag(this.props.propTag, ac) : null;
    // Only send updates to control if something changed
    if (!_.isEqual(prop, this.state.activeCellProp) || !_.isEqual(ac, this.state.activeCell)) {
      this.props.setControlStateFromCellProp(prop);
    }
  },

  /*
    When the control's state changes, bubble up here and use that state and the current active selection to 
    change props on backend. For example, user presses bold button while A5 is selected -> API message to change prop, which
    in turn will cause the cell to become bold. We shouldn't use any, but JS doesn't like T. 
  */
  onControlStateChange(nextState: any) {
    console.log("control about to set backend");
    SelectionStore.withActiveSelection(({range: activeRange}) => {
      this.props.setBackendCellProp(nextState, activeRange);
    });
  },

  // Simply render the control
  render(): React.Element {
    return this.props.control;
  }
});
