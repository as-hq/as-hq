/* @flow */

import React from 'react';
let request = require('superagent');

import {logDebug} from '../AS/Logger';
import Constants from '../Constants';
import {HOST_IP, HOST_BASE_URL} from '../Constants';

import ExpStore from '../stores/ASExpStore.js';
import SelectionStore from '../stores/ASSelectionStore';
import API from '../actions/ASApiActionCreators';


/*
A similar React class used to be called FileImportButton, but there will no longer be file import buttons.
The main use of this file is now a function that opens up a file dialog upon a callback, with config parameters
It will then send these files to our file handling Python server via HTTP on backend.
You have the option of also specifying a callback after the HTTP succeeds.
*/

export default {

  // Get the URL to POST a file to, casing on remote
  getStaticUrl(): string {
    if (Constants.isRemote) {
      return 'http://' + HOST_IP + ':9000';
    } else {
      return 'http://' + HOST_BASE_URL + ':9000';
    }
  },

  // Given a file, get the index and language, and send a message to backend
  importCSVCallback(file: File) {
    let sel = SelectionStore.getActiveSelection();
    if (sel == null){
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

  /*
  Given a boolean to allow multiple files, and a callback function to call if the POST file is successful (on each file),
  create a fileSelector HTML element that has an input file, and simulate a click on that element.
  We want an onchange to fire, which is why we create and dispatch a change event
  In the onchange after the click and the files are selected, send them via HTTP to a Python file server.
  Note that having the callback here guarantees that it is called only after a successful POST
  */
  openFileDialog(allowMultiple: boolean, callbackAfterSuccess: ((file: File) => void)): ?FileList { 
    let fileSelector = document.createElement('input');
    fileSelector.setAttribute('type', 'file');
    if (allowMultiple) {
      fileSelector.setAttribute('multiple', 'multiple');
    }
    fileSelector.addEventListener("change", (evt) => {
      evt.preventDefault();
      let files = evt.target.files;
      let req = request.post(this.getStaticUrl());
      for (var i = 0; i < files.length; i++) {
        let file = files[i];
        req.attach(file.name, file);
      }
      req.end((err, res) => {
        // Upon failure, do nothing, upon success, execute callback
        if (err || !res.ok) {
          alert("Could not import files");
        } else {
          // Upon success, call the callback function for each file
          for (var i = 0; i < files.length; i++) {
            callbackAfterSuccess(files[i]);
          }
        }
      });
    });
    fileSelector.click();
    let event = new Event('change');
    fileSelector.dispatchEvent(event);
  }

};
