/* @flow */

import React from 'react';
let request = require('superagent');

import {logDebug} from '../AS/Logger';
import Constants from '../Constants';

import ExpStore from '../stores/ASExpStore.js';
import SelectionStore from '../stores/ASSelectionStore';
import API from '../actions/ASApiActionCreators';


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
      let simpleIndex = sel.origin,
          lang = ExpStore.getLanguage();
      if (lang != null) {
        API.importCSV(simpleIndex, lang, file.name);
      } else {
        // make Excel the default language. Would do ExpStore.getLanguage() || 'Excel', but Flow.
        API.importCSV(simpleIndex, 'Excel', file.name);
      }
    }
  },

};

export default FileImportDialog;
