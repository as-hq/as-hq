/* @flow */

import type {
  Callback,
  Lens
} from '../types/Base';

import type {
  ASClientError
} from '../types/Errors';

import React from 'react';

import {Paper} from 'material-ui';
// $FlowFixMe: need to create declarations for all of these, but it's complicated.
import Table from 'material-ui/lib/table/table';
// $FlowFixMe: need to create declarations for all of these, but it's complicated.
import TableBody from 'material-ui/lib/table/table-body';
// $FlowFixMe: need to create declarations for all of these, but it's complicated.
import TableHeader from 'material-ui/lib/table/table-header';
// $FlowFixMe: need to create declarations for all of these, but it's complicated.
import TableHeaderColumn from 'material-ui/lib/table/table-header-column';
// $FlowFixMe: need to create declarations for all of these, but it's complicated.
import TableRow from 'material-ui/lib/table/table-row';
// $FlowFixMe: need to create declarations for all of these, but it's complicated.
import TableRowColumn from 'material-ui/lib/table/table-row-column';

import U from '../AS/Util';
const {
  Conversion: TC,
  Location: L
} = U;

import ASIndex from '../classes/ASIndex';
import ASSelection from '../classes/ASSelection';

import CellStore from '../stores/ASCellStore';
import SelectionStore from '../stores/ASSelectionStore';

import _Styles from '../styles/ASErrorPane';

import _ from 'lodash';

type ASErrorPaneProps = {
  style?: {[key: string]: any};
  onRequestSelect: Callback<ASIndex>;
  open: boolean;
};

type ASErrorPaneState = {
  currentSelection: ?ASSelection;
  errors: Array<ASClientError>;
  onlyCurrentCell: boolean;
  selectedRow: number;
};

export default class ASErrorPane
  extends React.Component<{}, ASErrorPaneProps, ASErrorPaneState>
{
  constructor(props: ASErrorPaneProps) {
    super(props);

    this.state = {
      currentSelection: null,
      errors: [],
      onlyCurrentCell: true,
      selectedRow: -1
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

  linkStateLens<T>(lens: Lens<ASErrorPaneState, T>): ReactLink<T> {
    let self = this;
    return ({
      value: lens.get(self.state),
      requestChange(newValue: T) {
        lens.set(self.state, newValue);
      }
    });
  }

  linkState(str: $Keys<ASErrorPaneState>): ReactLink {
    let self = this;

    return this.linkStateLens({
      get(state: ASErrorPaneState) { return state[str]; },
      set(state: ASErrorPaneState, val: any) {
        self.setState({ [str]: val });
      }
    });
  }

  render(): React.Element {
    let errors = this._getCurrentErrorList();
    let {open} = this.props;
    let {selectedRow} = this.state;

    return (
      <Paper style={_Styles.root}>
        <div style={{
          ..._Styles.showAllContainer,
          ...(open ? { } : { display: 'none' })
        }}>
          <div style={_Styles.showAllLabel}>
            Show only errors from current cell
          </div>
          <input
            type="checkbox"
            style={_Styles.showAllCheckbox}
            checkedLink={this.linkState('onlyCurrentCell')} />
        </div>
        <Table
          height="100%"
          fixedHeader={true}
          selectable={false}
          style={_Styles.table}
          headerStyle={_Styles.th}
          onCellClick={this._handleCellClick.bind(this)}>
          <TableHeader
            adjustForCheckbox={false}
            displaySelectAll={false}
            height={_Styles.thHeight}
            style={_Styles.th}>
            <TableRow displayRowCheckbox={false} style={_Styles.th}>
              {[
                'Cell location', 'Language', 'Error description'
              ].map((str, idx) =>
                <TableHeaderColumn style={{..._Styles.thd, width: _Styles.widths[idx]}}>
                  {str}
                </TableHeaderColumn>
              )}
            </TableRow>
          </TableHeader>
          <TableBody
            displayRowCheckbox={false}
            stripedRows={true}
            selectable={true}
            style={_Styles.tbody}>
            {errors.map(({location, language, msg}, rowIdx) =>
              <TableRow
                style={_Styles.tr(rowIdx)}
                displayBorder={false}
                selected={selectedRow === rowIdx} >
                {[
                  location.toExcel().toString(),
                  language,
                  msg
                ].map((str, idx) =>
                  <TableRowColumn style={{..._Styles.tr(rowIdx), width: _Styles.widths[idx]}}>
                    {str}
                  </TableRowColumn>
                )}
              </TableRow>
            )}
          </TableBody>
        </Table>
      </Paper>
    );
  }

  _getCurrentErrorList(): Array<ASClientError> {
    const {onlyCurrentCell, currentSelection, errors} = this.state;

    if (onlyCurrentCell && currentSelection) {
      return errors.filter(
        ({ location }) => location.isInRange(currentSelection.range)
      );
    } else {
      return errors;
    }
  }

  _handleCellStoreChange() {
    this.setState({ errors: CellStore.getAllErrors() });
  }

  _handleSelectionChange() {
    this.setState({ currentSelection: SelectionStore.getActiveSelection() });
  }

  _handleCellClick(row: number, col: number) {
    let errors = this._getCurrentErrorList();
    let {location} = errors[row];

    console.log('request select', location);

    this.props.onRequestSelect(location);
    this.setState({
      selectedRow: row
    });
  }
}
