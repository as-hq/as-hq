/* @flow */

import type {
  Callback,
  Lens
} from '../../types/Base';

import type {
  ASClientError
} from '../../types/Errors';

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

import U from '../../AS/Util';
const {
  Conversion: TC,
  Location: L
} = U;

import ASIndex from '../../classes/ASIndex';
import ASSelection from '../../classes/ASSelection';

import _Styles from '../../styles/ASErrorPane';

import _ from 'lodash';

type Props = {
  showAll: boolean;
  errors: Array<ASClientError>;
};

export default class ASErrorPane
  extends React.Component<{}, Props, {}>
{

  shouldComponentUpdate(nextProps: Props, nextState: {}): boolean {
    return !_.isEqual(nextProps.errors, this.props.errors);
  }

  render(): React.Element {
    const {showAll, errors} = this.props;
    return (
      <Paper style={_Styles.root}>
        <div style={_Styles.showAllContainer}>
          <div style={_Styles.showAllLabel}>
            Show only errors from current selection
          </div>
          <input
            type="checkbox"
            style={_Styles.showAllCheckbox}
            checked={showAll}
          />
        </div>

        <Table
          height="100%"
          fixedHeader={true}
          selectable={false}
          style={_Styles.table}
          headerStyle={_Styles.th}
          onCellClick={(row, _) => this._onErrorSelect(row)}>
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
                displayBorder={false} >
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

  _onErrorSelect(idx: number) {
    // TODO
  }
}
