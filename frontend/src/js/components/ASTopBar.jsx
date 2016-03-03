/* @flow */

import type {
  Callback
} from '../types/Base';

import type {
  NestedMenuSpec,
  SimpleItemSpec,
  MenuItemSpec,
  FileItemSpec,
} from './menu-bar/types';

import React from 'react';

import API from '../actions/ASApiActionCreators';
import SheetStateStore from '../stores/ASSheetStateStore';
import LogStore from '../stores/ASLoginStore';
import * as LogViewerActionCreator from '../actions/ASLogViewerActionCreators';
import SheetActions from '../actions/ASSheetActionCreators';
import DialogActions from '../actions/DialogActionCreators';
import OverlayActions from '../actions/ASOverlayActionCreators';
import Constants from '../Constants';

import ASCondFormattingDialog from './cond-formatting/ASCondFormattingDialog.jsx';
import ASMenuBar from './menu-bar/ASMenuBar.jsx';

import FileImportDialog from '../AS/FileImportDialog';
import ASChartDialog from './chart/ASChartDialog.jsx';
import {topBar as topBarZIndex} from '../styles/zIndex';

type ASTopBarProps = {
  toggleEvalHeader: Callback;
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

function file({callback, title}): FileItemSpec {
  return ({
    tag: 'FileItemSpec',
    title: title,
    callback: callback,
  });
}

export default class ASTopBar extends React.Component<{}, ASTopBarProps, {}> {
  _sheetsListener: () => void;

  constructor(props: ASTopBarProps) {
    super(props);
    this._sheetsListener = () => this.forceUpdate();
  }

  componentDidMount() {
    API.getMySheets();
    SheetStateStore.addListener('GOT_MY_SHEETS', this._sheetsListener);
  }

  componentWillUnmount() {
    SheetStateStore.removeListener('GOT_MY_SHEETS', this._sheetsListener);
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

    // If the user is a dev, then display an option to open the log viewer
    const maybeLogButton = LogStore.userIsDev() 
      ? [(
         simple({
            title: 'Log Viewer',
            callback() {
              LogViewerActionCreator.openLogViewer();
            }
          })
      )]
      : [];


    return (
      <span>
        <a href="http://alphasheets.com" style={{
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
          textDecoration: 'none',
          fontSize: '28pt',
          fontStyle: 'italic',
          fontFamily: 'Georgia, serif',
          top: 0,
          left: 0,
          lineHeight: '50px',
          zIndex: topBarZIndex,
        }}>
          α
        </a>
        <ASMenuBar style={{paddingLeft: '50px'}} menus={[
          {title: 'File', menuItems: [

            nested({
              title: 'Open',
              menuItems:
                SheetStateStore.getMySheets().map(sheet =>
                  simple({
                    title: sheet.sheetName,
                    callback() {
                      API.openSheet(sheet.sheetId);
                    }
                  })
                )
            }),

            simple({
              title: 'New',
              callback() {
                // Set timeout so that the callback finishes quickly and menu closes
                // before the popup appears
                setTimeout(function() {
                  let sheetName = window.prompt("Enter the sheet name.");
                  if (sheetName != null) {
                    API.newSheet(sheetName);
                  }
                }, 20);
              }
            }),

            file({
              title: 'Import CSV',
              callback(files) {
                FileImportDialog.importCSV(files)
              },
            }),

            file({
              title: 'Import Excel',
              callback(files) {
                FileImportDialog.importExcel(files)
              },
            }),

            simple({
              title: 'Save',
              callback() {
                API.export(SheetStateStore.getCurrentSheetId());
              }
            }),

          ]},

          {title: 'Edit', menuItems: [
            simple({
              title: 'Conditional formatting',
              callback() {
                DialogActions.openCondFormattingDialog();
              }
            }),

            simple({
              title: 'Clear sheet',
              callback() {
                let shouldClear = confirm("Are you sure you want to clear the sheet?");
                if (shouldClear) {
                  API.clearSheet();
                }
              }
            }),
          ]},

          // Disabling charts for now (Ritesh leap day)
          // {title: 'Insert', menuItems: [
          //   simple({
          //     title: 'Chart',
          //     callback() {
          //       DialogActions.openChartingDialog();
          //     }
          //   })
          // ]},

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

            ...testAlphaSheets,
            ...maybeLogButton
          ]}
        ]} />
      </span>
    );
  }
}
