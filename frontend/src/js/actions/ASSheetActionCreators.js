/* @flow */

import Dispatcher from '../Dispatcher';

import type {
  SheetUpdate,
  CondFormatRuleUpdate,
  Update,
  UpdateTemplate
} from '../types/Updates';

import type {
  ASSheet
} from '../types/Eval';

import ASCell from '../classes/ASCell';
import U from '../AS/Util';
import ASCondFormatRule from '../classes/ASCondFormatRule';
import SheetStateStore from '../stores/ASSheetStateStore';

export default {
  updateSheet(update: SheetUpdate) {
    setSheetData(update);
  },

  resetData(update: SheetUpdate) {
    Dispatcher.dispatch({
      _type: 'CLEARED_SHEET',
      sheetId: SheetStateStore.getCurrentSheetId()
    });
    setSheetData(update);
  },

  clearSheet(sheetId: string) {
    Dispatcher.dispatch({
      _type: 'CLEARED_SHEET',
      sheetId
    });
  },

  changeSheet(sheetId: string) {
    Dispatcher.dispatch({
      _type: 'CHANGED_SHEET',
      sheetId
    });
  },

  setMySheets(sheets: Array<ASSheet>) {
    Dispatcher.dispatch({
      _type: 'GOT_MY_SHEETS',
      sheets
    });
  },

  decouple() {
    Dispatcher.dispatch({
      _type: 'EVAL_TRIED_TO_DECOUPLE'
    });
  }
}

// *****************************************************************
// helpers

function updateIsEmpty(update: UpdateTemplate) { // same problems as makeServerMessage
  return update.newVals.length == 0 && update.oldKeys.length == 0;
}

function setSheetData(update: SheetUpdate) {
  if (!updateIsEmpty(update.descriptorUpdates)) {
    Dispatcher.dispatch({
      _type: 'GOT_UPDATED_RANGE_DESCRIPTORS',
      newRangeDescriptors: update.descriptorUpdates.newVals,
      oldRangeKeys: update.descriptorUpdates.oldKeys
    });
  }

  if (!updateIsEmpty(update.cellUpdates)) {
    Dispatcher.dispatch({
      _type: 'GOT_UPDATED_CELLS',
      newCells: ASCell.makeCells(update.cellUpdates.newVals),
      oldLocs: U.Location.makeLocations(update.cellUpdates.oldKeys)
    });
  }

  if (!updateIsEmpty(update.barUpdates)) {
    Dispatcher.dispatch({
      _type: 'GOT_UPDATED_BARS',
      newBars: update.barUpdates.newVals,
      oldBarLocs: update.barUpdates.oldKeys
    });
  }

  if (!updateIsEmpty(update.condFormatRuleUpdate)) {
    Dispatcher.dispatch({
      _type: 'GOT_UPDATED_RULES',
      newRules:
        update.condFormatRuleUpdate.newVals.map(
          (r) => new ASCondFormatRule(r)
        ),
      oldRuleIds: update.condFormatRuleUpdate.oldKeys,
    });
  }
}
