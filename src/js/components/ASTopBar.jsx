/* @flow */

import type {
  Callback
} from '../types/Base';

import type {
  NestedMenuSpec,
  SimpleItemSpec,
  MenuItemSpec
} from './menu-bar/types';

import React from 'react';

import API from '../actions/ASApiActionCreators';
import SheetStateStore from '../stores/ASSheetStateStore';

import ASCondFormattingDialog from './dialogs/ASCondFormattingDialog.jsx';
import ASMenuBar from './menu-bar/ASMenuBar.jsx';
import ASSpreadsheet from './ASSpreadsheet.jsx';

type ASTopBarProps = {
  toggleEvalHeader: Callback;
};

type ASTopBarState = {
  condFormattingOpen: boolean;
};

function nested(etc): NestedMenuSpec {
  return ({
    tag: 'NestedMenuSpec',
    ...etc
  });
}

function simple({callback, title}): SimpleItemSpec {
  return ({
    tag: 'SimpleItemSpec',
    title: title,
    callback: callback
  });
}

export default class ASTopBar extends React.Component<{}, ASTopBarProps, ASTopBarState> {
  constructor(props: ASTopBarProps) {
    super(props);

    this.state = { condFormattingOpen: false };
  }

  render(): React.Element {
    let self = this;

    return (
      <span>
        <ASCondFormattingDialog
          open={this.state.condFormattingOpen}
          onRequestClose={this._onCondFormatClose.bind(this)} />
        <ASChartDialog
          open={this.state.chartOpen}
          onRequestClose={this._onChartClose}
          onCreate={ASSpreadsheet.addOverlay} />
        <ASMenuBar menus={[
          {title: 'File', menuItems: [
            simple({
              title: 'Open',
              callback() {
                alert("To open a saved AlphaSheets sheet, drag it onto the spreadsheet on this page. (Cut us some slack, this is an MVP.)");
              }
            }),

            simple({
              title: 'Save',
              callback() {
                API.export(SheetStateStore.getCurrentSheet());
              }
            })
          ]},

          {title: 'Edit', menuItems: [
            simple({
              title: 'Conditional formatting',
              callback() {
                self.setState({
                  condFormattingOpen: true
                });
              }
            })
          ]},

          {title: 'Code', menuItems: [
            simple({
              title: 'Toggle header',
              callback() {
                self.props.toggleEvalHeader();
              }
            })
          ]},

          {title: 'Help', menuItems: [
            simple({
              title: 'Submit bug report',
              callback() {
                let bugReport = window.prompt("Please describe the bug you encountered.","");
                API.bugReport(bugReport);
              }
            })
          ]}
        ]} />
      </span>
    );
  }

  _onCondFormatClose() {
    this.setState({condFormattingOpen: false});
  },

  _onChartClose() {
    this.setState({chartOpen: false});
  }
}
