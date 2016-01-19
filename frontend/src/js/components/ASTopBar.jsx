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
import request from 'superagent';

import API from '../actions/ASApiActionCreators';
import SheetStateStore from '../stores/ASSheetStateStore';
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
  constructor(props: ASTopBarProps) {
    super(props);
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
          zIndex: topBarZIndex,
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

            file({
              title: 'Import CSV',
              callback(files) {
                // TODO(joel): this should be part of the api, not done inline
                const req = request.post(FileImportDialog.url);
                for (let i = 0; i < files.length; i++) {
                  const file = files[i];
                  req.attach(file.name, file);
                }

                req.end((err, res) => {
                  if (err || !res.ok) {
                    console.error(err);
                    alert('Could not import files');
                  } else {
                    for (let i = 0; i < files.length; i++) {
                      FileImportDialog.importCSVCallback(files[i]);
                    }
                  }
                });
              },
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

          {title: 'Insert', menuItems: [
            simple({
              title: 'Chart',
              callback() {
                DialogActions.openChartingDialog();
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
}
