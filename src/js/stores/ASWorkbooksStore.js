/* @flow */

import type {
  ASAction,
  WorkbookAction
} from '../types/Actions';

import type {
  ASSheet
} from '../types/Eval';

import type {
  ASBackendWorkbookSheet
} from '../types/Messages';

import type {
  Dict
} from '../types/Base';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import BaseStore from './BaseStore';
import Util from '../AS/Util';

let _data: {
  workbooks: Dict<ASBackendWorkbookSheet>
} = {
  workbooks: {}
};

function isNotWorkbookAction(action: ASAction): boolean {
  return !Util.arrContains(
    ['GOT_UPDATED_WORKBOOKS', 'GOT_NEW_WORKBOOKS', 'DELETED_WORKBOOKS'],
    action._type
  );
}

const ASWorkbookStore = Object.assign({}, BaseStore, {
  /* This function describes the actions of the ASWorkbookStore upon recieving a message from Dispatcher */
  dispatcherIndex:
    Dispatcher.register((action: ASAction) => {
      if (isNotWorkbookAction(action)) {
        return;
      }

      let _action = ((action: any): WorkbookAction);

      let workbookDict: Dict<ASBackendWorkbookSheet> = {};
      _action.workbooks.forEach((wb: ASBackendWorkbookSheet) => {
        workbookDict[wb.wsName] = wb;
      });

      switch (_action._type) {
        case 'GOT_UPDATED_WORKBOOKS':
          ASWorkbookStore.updateData(workbookDict);
          ASWorkbookStore.emitChange();
          break;
        case 'GOT_NEW_WORKBOOKS':
          ASWorkbookStore.mergeWorkbooks(workbookDict);
          ASWorkbookStore.emitChange();
          break;
        case 'DELETED_WORKBOOKS':
          ASWorkbookStore.deleteWorkbooks(workbookDict);
      }
    }),

/**************************************************************************************************************************/
// store modification methods

  updateData(wbs: Dict<ASBackendWorkbookSheet>) {
    _data.workbooks = wbs;
  },

  mergeWorkbooks(wbs: Dict<ASBackendWorkbookSheet>) {
    for (var key in wbs) {
      let wb = wbs[key];
      if (_data.workbooks[key]){
        // assumes the sheets are actually new
        _data.workbooks[key].wsSheets =
            Util.mergeSheets(_data.workbooks[key].wsSheets,
              wb.wsSheets);
      } else
        _data.workbooks[key] = wbs[key];
    }
  },

  deleteWorkbooks(wbs) {
    // TODO
    for (var key in wbs) {
      if (_data.workbooks[key]){
        _data.workbooks[key] =
            Util.removeSheets(_data.workbooks[key].wsSheets,
              wbs[key].wsSheets);
      }
    }
  },

  getWorkbooks(): Dict<ASBackendWorkbookSheet> {
    return _data.workbooks;
  },

  getSheets(workbookName: string): Array<ASSheet> {
    return _data.workbooks[workbookName].wsSheets;
  }
});


export default ASWorkbookStore;
