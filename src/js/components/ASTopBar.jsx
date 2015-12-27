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
import OverlayActions from '../actions/ASOverlayActionCreators';
import Constants from '../Constants';

import ASCondFormattingDialog from './dialogs/ASCondFormattingDialog.jsx';
import ASMenuBar from './menu-bar/ASMenuBar.jsx';

import FileImportDialog from '../AS/FileImportDialog';
import ASChartDialog from './chart/ASChartDialog.jsx';

type ASTopBarProps = {
  toggleEvalHeader: Callback;
};

type ASTopBarState = {
  chartOpen: boolean;
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

    this.state = { condFormattingOpen: false, chartOpen: false };
  }

  render(): React.Element {

    let self = this;

    let testAlphaSheets =
      Constants.isProduction
        ? []
        : [(
          simple({
            title: 'Test AlphaSheets',
            callback() {
              window.test();
            }
          })
        )];

    return (
      <span>
        <ASCondFormattingDialog
          open={this.state.condFormattingOpen}
          onRequestClose={this._onCondFormatClose.bind(this)} />
        <ASChartDialog
          open={this.state.chartOpen}
          onRequestClose={this._onChartClose.bind(this)}
          onCreate={OverlayActions.add} />
        <div style={{
          position: 'absolute',
          display: 'block',
          width: '60px',
          height: '60px',
          color: 'rgba(255, 255, 255, 0.72)',
          backgroundColor: '#242424',
          boxShadow: '0px 0px 15px 2px #000000',
          borderRadius: '50%',
          fontWeight: '500',
          textAlign: 'center',
          fontSize: '28pt',
          fontStyle: 'italic',
          fontFamily: 'Georgia, serif',
          top: 0,
          left: 0,
          lineHeight: '50px',
          zIndex: '100000'
        }}>
          α
        </div>
        <ASMenuBar style={{paddingLeft: '50px'}} menus={[
          {title: 'File', menuItems: [
            simple({
              title: 'Open',
              callback() {
                alert("To open a saved AlphaSheets sheet, drag it onto the spreadsheet on this page.");
              }
            }),

            simple({
              title: 'Import CSV',
              callback() {
                FileImportDialog.openFileDialog(false, FileImportDialog.importCSVCallback);
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

          {title: 'Insert', menuItems: [
            simple({
              title: 'Chart',
              callback() {
                self.setState({
                  chartOpen: true
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
            }),

            ...testAlphaSheets
          ]}
        ]} />
      </span>
    );
  }

  _onCondFormatClose() {
    this.setState({condFormattingOpen: false});
  }

  _onChartClose() {
    this.setState({chartOpen: false});
  }
}
