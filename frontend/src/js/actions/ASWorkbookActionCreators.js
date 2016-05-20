/* @flow */

import Dispatcher from '../Dispatcher';
import API from '../actions/ASApiActionCreators';

import type {
  SheetUpdate,
  CondFormatRuleUpdate,
  Update,
} from '../types/Updates';

import type {
  Workbook,
  WorkbookRef,
  Sheet,
} from '../types/Eval';

import ASCell from '../classes/ASCell';
import U from '../AS/Util';
import ASCondFormatRule from '../classes/ASCondFormatRule';
import WorkbookStore from '../stores/ASWorkbookStore';

export default {

  togglePauseMode() {
    API.togglePauseMode();
    Dispatcher.dispatch({
      _type: 'TOGGLED_PAUSE_MODE'
    });
  },

  updateSheet(update: SheetUpdate) {
    Dispatcher.dispatch({
      _type: 'SHEET_UPDATED',
      sheetId: WorkbookStore.getCurrentSheetId(),
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

  setOpenedWorkbook(workbook: Workbook) {
    Dispatcher.dispatch({
      _type: 'SET_OPENED_WORKBOOK',
      workbook,
    });
  },

  setMyWorkbooks(workbooks: Array<WorkbookRef>) {
    Dispatcher.dispatch({
      _type: 'SET_MY_WORKBOOKS',
      workbooks,
    });
  },

}
