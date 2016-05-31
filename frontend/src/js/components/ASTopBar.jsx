/* @flow */

import type { StoreToken } from 'flux';
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
import DOM from 'react-dom';
import {Paper} from 'material-ui';

import API from '../actions/ASApiActionCreators';
import APIActions from '../actions/APIActionCreators';
import GridStore from '../stores/ASGridStore';
import LoginStore from '../stores/ASLoginStore';
import WorkbookStore from '../stores/ASWorkbookStore';
import LogStore from '../stores/ASLoginStore';
import * as LogViewerActionCreator from '../actions/ASLogViewerActionCreators';
import WorkbookActions from '../actions/ASWorkbookActionCreators';
import DialogActions from '../actions/DialogActionCreators';
import OverlayActions from '../actions/ASOverlayActionCreators';
import ShortcutHelperActions from '../actions/ShortcutHelperActionCreators';
import Constants from '../Constants';

import ASCondFormattingDialog from './cond-formatting/ASCondFormattingDialog.jsx';
import ASMenuBar from './menu-bar/ASMenuBar.jsx';

import FileImportDialog from '../AS/FileImportDialog';
import {topBar as topBarZIndex} from '../styles/zIndex';

import U from '../AS/Util';

type Props = {
  toggleEvalHeader: Callback;
};

type State = {
  workbookPopoverOpen: boolean;
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

function file({callback, title}): SimpleItemSpec {
  return ({
    tag: 'SimpleItemSpec',
    title: title,
    callback: () => {
      U.File.promptOpen(fs => callback(fs))
    },
  });
}

export default class ASTopBar extends React.Component {
  static defaultProps = {};
  props: Props;
  state: State;
  _workbookStoreListener: StoreToken;
  _workbookTitle: any;

  constructor(props: Props) {
    super(props);
    this.state = {
      workbookPopoverOpen: false,
    };
  }

  componentDidMount() {
    this._workbookStoreListener = WorkbookStore.addListener(() => this.forceUpdate())
  }

  componentWillUnmount() {
    this._workbookStoreListener.remove();
  }

  render(): React.Element {
    let self = this;
    const { workbookPopoverOpen } = this.state;
    const paused = WorkbookStore.inPauseMode();

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
      <div style={styles.root}>
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
        <ASMenuBar style={styles.menuBar} menus={[
          {title: 'File', menuItems: [

            simple({
              title: 'New workbook',
              callback() {
                const name = window.prompt('Enter workbook name.');
                API.newWorkbook(name);
              }
            }),

            nested({
              title: 'Open workbook',
              menuItems:
                WorkbookStore.getWorkbooks().map(wb =>
                  simple({
                    title: WorkbookStore.getWorkbookTitle(wb.id),
                    callback() {
                      API.openWorkbook(wb.id);
                    }
                  })
                )
            }),

            simple({
              title: 'Clone',
              callback() {
                API.cloneSheet(WorkbookStore.getCurrentSheetId());
              }
            }),

            nested({
              title: 'Import...',
              menuItems: [
                file({
                  title: 'Workbook',
                  callback(files) {
                    FileImportDialog.importAlphaSheets(files)
                  },
                }),

                file({
                  title: 'CSV',
                  callback(files) {
                    FileImportDialog.importCSV(files)
                  },
                }),

                file({
                  title: 'Excel',
                  callback(files) {
                    FileImportDialog.importExcel(files)
                  },
                }),
              ]
            }),

            nested({
              title: 'Export...',
              menuItems: [
                simple({
                  title: 'Workbook',
                  callback() {
                    API.exportWorkbook(WorkbookStore.getCurrentWorkbookId());
                  }
                }),
                simple({
                  title: 'Current cell value',
                  callback() {
                    const { origin } = GridStore.getActiveSelection();
                    API.exportCell(origin);
                  }
                }),
              ]
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
              title: 'Global code editor',
              callback() {
                self.props.toggleEvalHeader();
              }
            }),
            simple({
              title: 'Re-evaluate',
              callback() {
                API.reEval();
              }
            }),
            simple({
              title: paused ? 'Paused evaluations (click to toggle)' : 'Normal evaluations (click to toggle)',
              callback() {
                WorkbookActions.togglePauseMode();
              }
            })
          ]},

          {title: 'Share', menuItems: [
            simple({
              title: 'Via link...',
              callback() {
                DialogActions.openShareDialog();
              }
            }),
          ]},

          {title: 'Help', menuItems: [
            simple({
              title: 'Documentation',
              callback() {
                window.open('https://gist.github.com/zgao/3280177206da3cf75ca2f8c616c19c7f')
              }
            }),
            simple({
              title: 'Keyboard shortcuts',
              callback() {
                ShortcutHelperActions.toggleShortcutHelper();
              }
            }),
            simple({
              title: 'Submit bug report',
              callback() {
                let bugReport = window.prompt("Please describe the bug you encountered.","");
                API.bugReport(bugReport);
              }
            }),

            ...testAlphaSheets,
            ...maybeLogButton
          ]},
        ]} />

        <Paper style={styles.workbookTitle}>
          {WorkbookStore.getWorkbookTitle(WorkbookStore.getCurrentWorkbookId())}
        </Paper>

      </div>
    );
  }
}

const styles = {
  root: {
    width: '100%',
    display: 'inline-flex',
    flexDirection: 'row',
    backgroundColor: '#212121',
  },
   menuBar: {
    paddingLeft: 50,
    flexGrow: 1,
    flexBasis: 'content',
    position: 'relative',
   },
   workbookTitle: {
     flexGrow: 1,
     flexBasis: 'content',
     position: 'relative',
     backgroundColor: '#424242',
     color: 'white',
     marginRight: 10,
     marginRight: 10,
     marginTop: 8,
     marginBottom: 8,
     paddingLeft: 5,
     paddingRight: 5,
     fontWeight: 'bold',
  },
};
