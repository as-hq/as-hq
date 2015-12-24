/* @flow */

import type {
  Callback
} from '../types/Base';

import type {
  ASSelection
} from '../types/Eval';

import type {
  ASClientError
} from '../types/Errors';

import React from 'react';

import {Paper} from 'material-ui';
import Table from 'material-ui/lib/table/table';
import TableBody from 'material-ui/lib/table/table-body';
import TableHeader from 'material-ui/lib/table/table-header';
import TableHeaderColumn from 'material-ui/lib/table/table-header-column';
import TableRow from 'material-ui/lib/table/table-row';
import TableRowColumn from 'material-ui/lib/table/table-row-column';

import U from '../AS/Util';
const {
  Conversion: TC,
  Location: L
} = U;

import CellStore from '../stores/ASCellStore';
import SelectionStore from '../stores/ASSelectionStore';

import _Styles from '../styles/ASErrorPane';

import _ from 'lodash';

type ASErrorPaneProps = {
  style: {[key: string]: any};
  onRequestSelect: Callback<NakedIndex>;
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
    let errors = this._getCurrentErrorList();

    return (
      <Paper style={_Styles.root}>
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
            selectable={false}
            style={_Styles.tbody}>
            {errors.map(({location, language, msg}, rowIdx) =>
              <TableRow
                style={_Styles.tr(rowIdx)}
                displayBorder={false}>
                {[
                  TC.rangeToExcel(TC.indexToRange(location)),
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
        ({ location }) => L.indexIsInRange(location, currentSelection.range)
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
  }
}
