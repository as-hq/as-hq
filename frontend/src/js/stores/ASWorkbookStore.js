/* @flow */

import type {
  Workbook,
  WorkbookRef,
  Sheet,
} from '../types/Eval';

import type { ASAction } from '../types/Actions';

import Constants from '../Constants';

// $FlowFixMe
import { ReduceStore } from 'flux/utils';
import Immutable from 'immutable';
import invariant from 'invariant';
import Dispatcher from '../Dispatcher';

import LoginStore from './ASLoginStore';

import ASRange from '../classes/ASRange';
import ASIndex from '../classes/ASIndex';

type State = any;
const StateRecord = Immutable.Record({
  openedWorkbook: null,     // currently open workbook.         :: OpenedWorkbook
  workbookRefs: [],         // list of workbooks owned by user  :: [WorkbookRef]
});

class WorkbookStore extends ReduceStore<State> {
  getInitialState(): State {
    return new StateRecord();
  }

  reduce(state: State, action: ASAction): State {
    switch(action._type) {

      case 'LOGIN_SUCCESS': {
        const { openedWorkbook, workbookRefs } = action;
        return state.set('openedWorkbook', openedWorkbook)
                    .set('workbookRefs', workbookRefs);
      }

      case 'CHANGED_SHEET': {
        // required because immutable doesn't do a deep diff with "update"
        const sid = action.sheetId;
        this.__emitChange();
        return state.update('openedWorkbook', wb => {
          wb.openedSheet = sid;
          return wb;
        });
      }

      case 'SET_OPENED_WORKBOOK': {
        return state.set('openedWorkbook', action.workbook);
      }

      case 'SET_MY_WORKBOOKS': {
        return state.set('workbookRefs', action.workbooks);
      }

      case 'TOGGLED_PAUSE_MODE': {
        // TODO
        debugger;
      }

      default: {
        return state;
      }
    }
  }

  /**************************************************************************************************************************/
  // Public accessors

  getCurrentWorkbookId(): string {
    const wid = this._withOpenedWorkbook(wb => wb.id);
    invariant(wid, 'Authenticated user does not have a workbook ID!');
    return wid;
  }

  getCurrentSheetId(): string {
    const sid = this._withOpenedWorkbook(wb => wb.openedSheet);
    invariant(sid, 'Authenticated user does not have a sheet ID!');
    return sid;
  }

  inPauseMode(): boolean {
    return this._withOpenedSheet(sh => sh.inPauseMode) || false;
  }

  getCurrentSheetTitle(): string {
    const title = this._withOpenedSheet(sheet => {
      let qualifier = '';
      if (sheet.owner !== LoginStore.getUserId()) {
        qualifier = ` (owned by ${sheet.owner})`;
      }
      return sheet.name + qualifier;
    });
    // sheet is not available when app is first mounting.
    return title || '';
  }

  getCurrentWorkbookName(): string {
    const name = this._withOpenedWorkbook(wb => wb.name);
    return name || '';
  }

  getSheetLink(accountRequired: boolean): string {
    const sheetId = this.getCurrentSheetId();
    invariant(sheetId, "Cannot produce a sheet link when there is no sheet id!");
    return (
      'http://' +
      Constants.getFrontendHost() +
      '/#/sheets/' +
      (accountRequired ? '' : 'public/') +
      sheetId
    );
  }

  getOpenedSheets(): Array<Sheet> {
    const ss = this._withOpenedWorkbook(wb => wb.sheets);
    return ss || [];
  }

  getWorkbooks(): Array<WorkbookRef> {
    return this.getState().workbookRefs;
  }

  getMySheets(): Array<Sheet> {
    const me = LoginStore.getUserId();
    return this.getOpenedSheets().filter(s => s.owner === me);
  }

  getSharedSheets(): Array<Sheet> {
    const me = LoginStore.getUserId();
    return this.getOpenedSheets().filter(s => s.owner != me);
  }

  /**************************************************************************************************************************/
  // Private  helpers

  _withOpenedSheet(f): any {
    const sheet = this._withOpenedWorkbook(wb => {
      return wb.sheets.find(sheet => sheet.id === wb.openedSheet);
    });
    if (!! sheet) {
      return f(sheet);
    } else {
      return null;
    }
  }

  _withOpenedWorkbook(f): any {
    const wb = this.getState().openedWorkbook;
    if (!! wb) {
      return f(wb);
    } else {
      return null;
    }
  }
}

export default new WorkbookStore(Dispatcher);
