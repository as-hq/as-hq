/* @flow */

import React from 'react';

import {logDebug} from '../AS/Logger';
import Constants from '../Constants';

import ExpressionStore from '../stores/ASExpressionStore';
import SelectionStore from '../stores/ASSelectionStore';
import SheetStateStore from '../stores/ASSheetStateStore';
import API from '../actions/ASApiActionCreators';

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
    let sel = SelectionStore.getActiveSelection();
    if (sel == null) {
      return;
    } else {
      const simpleIndex = sel.origin;
      const language = ExpressionStore.getLanguage();
      API.importCSV(simpleIndex, language, file.name);
    }
  },

  importExcelCallback(file: File) {
    API.importExcel(SheetStateStore.getCurrentSheetId(), file.name);
  },

  postFilesToBackend(files: Array<File>, callback:  (file: File ) => void ) {
    const req = request.post(FileImportDialog.url);
    for (let i = 0; i < files.length; i++) {
      req.attach(files[i].name, files[i]);
    }
    req.end((err, res) => {
      if (err || !res.ok) {
        console.error(err);
        alert('Could not import files');
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
