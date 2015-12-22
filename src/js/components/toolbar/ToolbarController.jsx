/* @flow */

import type {
  NakedRange,
  ASIndex,
  ASCell
} from '../../types/Eval';

import React, {PropTypes} from 'react';
import _ from 'lodash';

import API from '../../actions/ASApiActionCreators';
import CellStore from '../../stores/ASCellStore';
import SheetStateStore from '../../stores/ASSheetStateStore';
import SelectionStore from '../../stores/ASSelectionStore';

/*
This component is a higher-order-component built for 
easy communication between stores and controls that monitor cell state (props, lang, etc)
*/

export default React.createClass({

  /* We need to know the control, how to update control state if stores change, and how to update backend if 
  control changes */
  propTypes: {
    setControlStateFromCell: React.PropTypes.func.isRequired,
    propagateControlStateChange: React.PropTypes.func.isRequired,
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
      activeCell: null
    }
  },

  /* 
    When the active selection or cell change, tell the underlying control to change its state based on that cell. 
    For example, change loc to A5; if A5 is bold, the bold button needs to be pushed in. Only send update if something relevant changed.
  */
  _onActiveCellChange() {
    let ac = CellStore.getActiveCell();
    // Only send updates to control if the active cell changed
    if (!_.isEqual(ac, this.state.activeCell)) {
      this.setState({activeCell: ac});
      this.props.setControlStateFromCell(ac);
    }
  },

  /*
    When the control's state changes, bubble up here and use that state and the current active selection to 
    change state on backend. For example, user presses bold button while A5 is selected -> API message to change prop, which
    in turn will cause the cell to become bold. 
  */
  onControlStateChange(nextState) {
    console.log("control about to set backend");
    SelectionStore.withActiveSelection(({range: activeRange}) => {
      this.props.propagateControlStateChange(nextState, activeRange);
    });
  },

  // Simply render the control
  render() {
    return this.props.control;
  }
});
