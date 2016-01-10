/* @flow */

import React, {PropTypes} from 'react';

import API from '../../actions/ASApiActionCreators';
import CellStore from '../../stores/ASCellStore';
import SheetStateStore from '../../stores/ASSheetStateStore';
import SelectionStore from '../../stores/ASSelectionStore';

import ASCell from '../../classes/ASCell';
import ASRange from '../../classes/ASRange';

/*
This component is a higher-order-component built for
easy communication between stores and controls that monitor cell state (props, lang, etc)
*/

type ToolbarControllerProps = {
  setControlStateFromCell: (activeCell: ?ASCell) => void;
  propagateControlStateChange: (nextState: any, activerange: ASRange) => void;
  control: React.Element;
};

type ToolbarControllerDefaultProps = {
};

type ToolbarControllerState = {
  activeCell: ?ASCell;
};


export default class ToolbarController
  extends React.Component<ToolbarControllerDefaultProps, ToolbarControllerProps, ToolbarControllerState>
{

  constructor(props: ToolbarController) {
    super(props);
  }

  componentDidMount() {
    SelectionStore.addChangeListener(this._onActiveCellChange.bind(this));
    CellStore.addChangeListener(this._onActiveCellChange.bind(this));
  }

  componentWillUnmount() {
    SelectionStore.removeChangeListener(this._onActiveCellChange.bind(this));
    CellStore.removeChangeListener(this._onActiveCellChange.bind(this));
  }

  /*
    When the active selection or cell change, tell the underlying control to change its state based on that cell.
    For example, change loc to A5; if A5 is bold, the bold button needs to be pushed in.
  */
  _onActiveCellChange() {
    let ac = CellStore.getActiveCell();
    this.setState({activeCell: ac});
    this.props.setControlStateFromCell(ac);
  }

  /*
    When the control's state changes, bubble up here and use that state and the current active selection to
    change state on backend. For example, user presses bold button while A5 is selected -> API message to change prop, which
    in turn will cause the cell to become bold.
  */
  onControlStateChange(nextState: any) {
    SelectionStore.withActiveSelection(({range: activeRange}) => {
      this.props.propagateControlStateChange(nextState, activeRange);
    });
  }

  // Simply render the control
  render(): React.Element {
    return this.props.control;
  }
}

/* We need to know the control, how to update control state if stores change, and how to update backend if
control changes */
ToolbarController.propTypes = {
  setControlStateFromCell: React.PropTypes.func.isRequired,
  propagateControlStateChange: React.PropTypes.func.isRequired,
  control: React.PropTypes.object.isRequired
};
