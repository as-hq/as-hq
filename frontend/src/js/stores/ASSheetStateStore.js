/* @flow */

import type {
  ASSheet,
  ASLanguage
} from '../types/Eval';

import type {
  ASCellGrid,
} from '../types/State';

import type {
  ASUserId
} from '../types/User';

import invariant from 'invariant';
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
import LoginStore from './ASLoginStore';

type SheetStateStoreData = {
  decoupleAttempt: boolean;
  xscroll: number;
  yscroll: number;
  mySheets: Array<ASSheet>;
  sharedSheets: Array<ASSheet>;
  currentSheetId: ?string;
  clipboard: {
    area: ?ASSelection;
    isCut: boolean;
  };
  externalError: ?string;
  viewingWindow: ?ASRange;
};

let _data: SheetStateStoreData = {
  decoupleAttempt: false,
  xscroll: 0,
  yscroll: 0,
  mySheets: [],
  sharedSheets: [],
  currentSheetId: null,
  clipboard: {
    area: null,
    isCut: false
  },
  externalError: null,
  viewingWindow: null
};

const ASSheetStateStore = Object.assign({}, BaseStore, {
  dispatcherIndex: Dispatcher.register((action) => {
    switch (action._type) {

      case 'LOGIN_SUCCESS':
        _data.currentSheetId = action.sheetId;
        console.warn('login success, got sheetId: ', action.sheetId);
        ASSheetStateStore.emitChange();
        break;

      case 'CHANGED_SHEET':
        _data.currentSheetId = action.sheetId;
        console.warn('sheet changed to ', action.sheetId);
        ASSheetStateStore.emitChange();
        break;

      case 'GOT_MY_SHEETS':
        _data.mySheets = action.mySheets;
        _data.sharedSheets = action.sharedSheets;
        ASSheetStateStore.emit('GOT_MY_SHEETS');
        break;

      case 'TOGGLED_PAUSE_MODE':
        ASSheetStateStore.togglePauseMode();
        ASSheetStateStore.emit('TOGGLED_PAUSE_MODE');
        break;

      default:
        break;

    }
  }),

  /**************************************************************************************************************************/
  // Helpers

  findSheet(pred: ((sheet: ASSheet) => boolean)): ?ASSheet {
    const sheet = _data.mySheets
          .concat(_data.sharedSheets)
          .find(pred);
    // sheet is not available when app is first mounting.
    if (sheet === undefined || sheet === null) {
      return null;
    } else return sheet;
  },

  getCurrentSheet(): ?ASSheet {
    return this.findSheet(sheet => sheet.sheetId === _data.currentSheetId);
  },

  /**************************************************************************************************************************/
  /* getter and setter methods */

  getDecoupleAttempt() {
    return _data.decoupleAttempt;
  },

  setDecoupleAttempt(b) {
    _data.decoupleAttempt = b;
  },

  getCurrentSheetId(): string {
    const sid = _data.currentSheetId;
    invariant(sid, 'Authenticated user does not have a sheet ID!');
    return sid;
  },

  setClipboard(rng, isCut) {
    _data.clipboard.area = rng;
    _data.clipboard.isCut = isCut;
    Render.setMode(rng === null ? null : (isCut ? 'cut' : 'copy'));
  },

  togglePauseMode() {
    const sheet = this.getCurrentSheet();
    if (sheet != null) {
      sheet.inPauseMode = !sheet.inPauseMode;
    }
  },

  inPauseMode(): boolean {
    const sheet = this.getCurrentSheet();
    return sheet !== null && sheet.inPauseMode;
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

  getMySheets(): Array<ASSheet> {
    return _data.mySheets;
  },

  getSharedSheets(): Array<ASSheet> {
    return _data.sharedSheets;
  },

  getCurrentSheetTitle(): string {
    const sheet = this.getCurrentSheet();
  
    // sheet is not available when app is first mounting.
    if (sheet === null) {
      return '';
    }

    let qualifier = '';
    if (sheet.sheetOwner !== LoginStore.getUserId()) {
      qualifier = ` (owned by ${sheet.sheetOwner})`;
    }
    return sheet.sheetName + qualifier;
  },

  getSheetLink(accountRequired: boolean): string {
    invariant(_data.currentSheetId, "Cannot produce a sheet link when there is no sheet id!");
    return (
      'http://' +
      Constants.getFrontendHost() +
      '/#/sheets/' +
      (accountRequired ? '' : 'public/') +
      _data.currentSheetId
    );
  },

  /**************************************************************************************************************************/
  /* Data boundaries */
  getDataBoundary(start: ASIndex, direction: string): ASIndex {
    let dY = 0, dX = 0;

    switch (direction) {
      case "Right": dX = 1; break;
      case "Left": dX = -1; break;
      case "Down": dY = 1; break;
      case "Up": dY = -1; break;
      default: throw "Invalid direction passed in";
    }

    const shiftAmount = { dY: dY, dX: dX };
    let prev = start;
    let curIdx = start.shift(shiftAmount);
    let next = curIdx.shift(shiftAmount);

    // go in this direction (shiftAmount)
      // find the first one that's a transition or an edge

    let checkWhetherCurrentIsBoundary = (p, c, n) => {
      return c && !(p && n);
    };

    while (!curIdx.equals(next)) { // while you still have a next, and you haven't reached boundary
      let [p, c, n] = [prev, curIdx, next].map(i => CellStore.isNonBlankCell(i));
      if (checkWhetherCurrentIsBoundary(p, c, n)) {
        break;
      }
      [prev, curIdx, next] = // move to the next window of 3 cells
        [prev, curIdx, next].map((x) => x.shift(shiftAmount));
    }

    return curIdx;
  },

  //This function returns what the new selection would be if you pressed ctrl+shift+right/up/left/down.
  //If shift is not held down,
  getDataBoundSelection(selection: ASSelection, direction: string): ASSelection {
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

    let bound = ASSheetStateStore.getDataBoundary(ASIndex.fromNaked(startLoc), direction);

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

  getViewingWindow(): ?ASRange {
    return _data.viewingWindow;
  }
});

export default ASSheetStateStore;
