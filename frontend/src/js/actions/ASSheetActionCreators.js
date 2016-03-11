/* @flow */

import Dispatcher from '../Dispatcher';

import type {
  SheetUpdate,
  CondFormatRuleUpdate,
  Update,
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
    Dispatcher.dispatch({
      _type: 'SHEET_UPDATED',
      sheetId: SheetStateStore.getCurrentSheetId(),
      update
    });
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

}
