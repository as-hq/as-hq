/* @flow */

import React from 'react';

import {logDebug} from '../AS/Logger';
import Constants from '../Constants';

import ExpressionStore from '../stores/ASExpressionStore';
import WorkbookStore from '../stores/ASWorkbookStore';
import GridStore from '../stores/ASGridStore';
import API from '../actions/ASApiActionCreators';
import NotificationActions from '../actions/ASNotificationActionCreators';

import request from 'superagent';

/*
A similar React class used to be called FileImportButton, but there will no longer be file import buttons.
The main use of this file is now a function that opens up a file dialog upon a callback, with config parameters
It will then send these files to our file handling Python server via HTTP on backend.
You have the option of also specifying a callback after the HTTP succeeds.
*/

const FileImportDialog = {

  url: Constants.getBackendUrl('http', Constants.BACKEND_IMPORT_PORT),

  // Given a file, get the index and language, and send a message to backend
  importCSVCallback(file: File) {
    const {origin} = GridStore.getActiveSelection();
    const language = ExpressionStore.getLanguage();
    API.importCSV(origin, language, file.name);
  },

  importExcelCallback(file: File) {
    API.importExcel(WorkbookStore.getCurrentSheetId(), file.name);
  },

  postFilesToBackend(files: Array<File>, callback:  (file: File ) => void ) {
    const req = request.post(FileImportDialog.url);
    for (let i = 0; i < files.length; i++) {
      req.attach(files[i].name, files[i]);
    }
    req.end((err, res) => {
      if (err || !res.ok) {
        console.error(err);
        NotificationActions.addNotification({
          title: 'Could not import files.',
          level: 'error',
          autoDismiss: 3,
        });
      } else {
        for (let i = 0; i < files.length; i++) {
          callback(files[i]);
        }
      }
    });
  },

  importCSV(files: Array<File>) {
    this.postFilesToBackend(files, this.importCSVCallback);
  },

  importExcel(files: Array<File>) {
    this.postFilesToBackend(files, this.importExcelCallback);
  }
};

export default FileImportDialog;
