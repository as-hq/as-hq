/* @flow */

import type {
  ASSheet,
  ASLanguage
} from '../types/Eval';

import type {
  ASCellGrid,
  ASFocusType
} from '../types/State';

import type {
  ASUserId
} from '../types/User';

import {logDebug} from '../AS/Logger';

import React from 'react';
import Dispatcher from '../Dispatcher';
import BaseStore from './BaseStore';
import Constants from '../Constants';

import U from '../AS/Util';

import ASIndex from '../classes/ASIndex';
import ASRange from '../classes/ASRange';
import ASSelection from '../classes/ASSelection';

import Render from '../AS/Renderers';
import ReplStore from  './ASReplStore';
import API from '../actions/ASApiActionCreators';
import CellStore from './ASCellStore';

type SheetStateStoreData = {
  userId: ASUserId;
  decoupleAttempt: boolean;
  xscroll: number;
  yscroll: number;
  openSheets: Array<ASSheet>;
  currentSheet: ASSheet;
  activeFocus: ASFocusType;
  lastActiveFocus: ASFocusType;
  clipboard: {
    area: ?ASSelection;
    isCut: boolean;
  };
  externalError: ?string;
  viewingWindow: ASRange;
};

let _data: SheetStateStoreData = {
  userId: "TEST_USER_ID",
  decoupleAttempt: false,
  xscroll: 0,
  yscroll: 0,
  openSheets: [],
  currentSheet: {
    tag: 'Sheet',
    sheetId: "INIT_SHEET_ID",
    sheetName: "Sheet1",
    sheetPermissions: {
      tag: 'Blacklist',
      contents: []
    }
  },
  activeFocus: 'grid',
  lastActiveFocus: 'textbox',
  clipboard: {
    area: null,
    isCut: false
  },
  externalError: null,
  viewingWindow:
    ASRange.fromNaked({
      tl: { col: 0, row: 0},
      br: { col: 100, row: 100}
    })
};

const ASSheetStateStore = Object.assign({}, BaseStore, {
  dispatcherIndex: Dispatcher.register((action) => {
    logDebug('Sheet state store received action', action);
    switch (action._type) {
      case 'FIND_INCREMENTED':
        break;
      case 'FIND_DECREMENTED':
        break;
      case 'GOT_FIND':
        // do nothing here on find; that's in the find store
        break;
      case 'EVAL_TRIED_TO_DECOUPLE':
        _data.decoupleAttempt = true;
        ASSheetStateStore.emitChange();
        break;
      /*
        This action is sent to Dispatcher by the ASSpreadsheet action creator on a scroll event
        It gets previous scroll state from the store and then uses the API to send a "get cells" message to server
      */
      case 'SCROLLED':
        const {vWindow} = action;
        _data.viewingWindow = vWindow.extendByCache();
        API.updateViewingWindow(vWindow);
        break;
      case 'GOT_FAILURE':
        ASSheetStateStore.setExternalError(action.errorMsg);
        if (action.action === "EvaluateRepl") {
          ReplStore.advanceLine();
        }
        ASSheetStateStore.emitChange();
        break;
    }
  }),

  /**************************************************************************************************************************/
  /* getter and setter methods */

  getDecoupleAttempt() {
    return _data.decoupleAttempt;
  },

  setDecoupleAttempt(b) {
    _data.decoupleAttempt = b;
  },

  getUserId() {
    return _data.userId;
  },

  setUserId(id) {
    _data.userId = id;
  },

  getCurrentSheet() {
    return _data.currentSheet;
  },

  getCurrentSheetId(): string {
    return ASSheetStateStore.getCurrentSheet().sheetId;
  },

  setCurrentSheet(sht) {
    _data.currentSheet = sht;
  },

  setCurrentSheetById(sheetId) {
    _data.currentSheet = {
      tag: 'Sheet',
      sheetId: sheetId,
      sheetName: "",
      sheetPermissions: {
        tag: 'Blacklist',
        contents: []
      }
    };
  },

  setClipboard(rng, isCut) {
    _data.clipboard.area = rng;
    _data.clipboard.isCut = isCut;
    Render.setMode(rng === null ? null : (isCut ? 'cut' : 'copy'));
  },

  getClipboard() {
    return _data.clipboard;
  },

  setScroll(x, y) {
    _data.xscroll = x;
    _data.yscroll = y;
  },

  getScroll() {
    return {x: _data.xscroll, y: _data.yscroll};
  },

  setExternalError(err: ?string) {
    _data.externalError = err;
  },

  getExternalError() {
    return _data.externalError;
  },

  /**************************************************************************************************************************/
  /* Focus */

  setFocus(elem) {
    logDebug("FOCUS", elem);
    _data.lastActiveFocus = _data.activeFocus;
    _data.activeFocus = elem;
  },

  getFocus() { return _data.activeFocus; },

  toggleFocusF2() {
    logDebug("last focus: ", _data.activeFocus);
    let temp = _data.activeFocus;
    if (_data.activeFocus === 'grid' && _data.lastActiveFocus === 'grid')
      _data.activeFocus = 'textbox';
    else if (_data.activeFocus === 'grid' && _data.lastActiveFocus === 'textbox')
      _data.activeFocus = 'textbox';
    else if (_data.activeFocus === 'grid' && _data.lastActiveFocus === 'editor')
      _data.activeFocus = 'editor';
    else if (_data.activeFocus === 'textbox')
      _data.activeFocus = 'grid';
    else if (_data.activeFocus === 'editor')
      _data.activeFocus = 'grid';
    _data.lastActiveFocus = temp;
    logDebug("new focus: ", _data.activeFocus);
  },

  /**************************************************************************************************************************/
  /* Data boundaries */
  getDataBoundary(start: ASIndex, direction): ASIndex {
    let dr = 0, dc = 0;

    switch (direction) {
      case "Right": dc = 1; break;
      case "Left": dc = -1; break;
      case "Down": dr = 1; break;
      case "Up": dr = -1; break;
    }

    let c = start.col, r = start.row;
    while (c >= 1 && r >= 1 && c <= Constants.numCols && r <= Constants.numRows) {
      c += dc;
      r += dr;
      if (CellStore.isNonBlankCell(c, r)
       && !(CellStore.isNonBlankCell(c + dc, r + dr) && CellStore.isNonBlankCell(c - dc, r - dr))) {
        break;
      }
    }

    if (c < 1) c = 1;
    if (r < 1) r = 1;
    if (c > Constants.numCols) c = Constants.numCols;
    if (r > Constants.numRows) r = Constants.numRows;

    return ASIndex.fromNaked({col: c, row: r});
  },

  //This function returns what the new selection would be if you pressed ctrl+shift+right/up/left/down.
  //If shift is not held down,
  getDataBoundSelection(selection: ASSelection, direction): ASSelection {
    let rng = selection.range,
        {tl, br} = rng,
        origin = selection.origin;

    let startLoc = { row: origin.row, col: origin.col };
    switch (direction) {
      case "Right": startLoc.col = (origin.col == tl.col) ? br.col : tl.col; break;
      case "Left": startLoc.col = (origin.col == br.col) ? tl.col : br.col; break;
      case "Up": startLoc.row = (origin.row == br.row) ? tl.row : br.row; break;
      case "Down": startLoc.row = (origin.row == tl.row) ? br.row : tl.row; break;
      default: throw "Invalid direction passed in";
    }

    let bound = this.getDataBoundary(startLoc, direction);

    // slight misnomers; these are the corners, but not necessarily top left or bottom right
    let newTl = {row: tl.row, col: tl.col};
    let newBr = {row: br.row, col: br.col};
    if (direction == "Up" || direction == "Down") {
      if (origin.row > tl.row)
        newTl.row = bound.row;
      else
        newBr.row = bound.row;
    } else if (direction == "Left" || direction == "Right") {
      if (origin.col > tl.col)
        newTl.col = bound.col;
      else
        newBr.col = bound.col;
    }
    // I haven't actually figured out why the above code works, it seems like it sort of just does.

    return new ASSelection({ range: {tl: newTl, br: newBr}, origin: origin });
  },

  // TODO actually get the data boundaries by iterating, or something
  // (but as long as we're using LARGE_SEARCH_BOUND, this area is an upper bound)
  getDataBounds() {
    return { tl: {col: 1, row: 1},
             br: {col: Constants.LARGE_SEARCH_BOUND,
                  row: Constants.LARGE_SEARCH_BOUND} };
  },

  getViewingWindow() {
    return _data.viewingWindow;
  }
});

export default ASSheetStateStore;
