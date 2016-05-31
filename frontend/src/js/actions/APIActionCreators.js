/* @flow */

import type { Offset } from '../types/Eval';

import API from './ASApiActionCreators';
import NotificationActions from './ASNotificationActionCreators';
import Constants from '../Constants';
import Dispatcher from '../Dispatcher';

import GridStore from '../stores/ASGridStore';
import ExpressionStore from '../stores/ASExpressionStore';
import HeaderStore from '../stores/ASHeaderStore';
import WorkbookStore from '../stores/ASWorkbookStore';

let lastEval = 0;
let lastHeaderEval = 0;

export default {
  evaluate(offset: Offset, providedExpression?: string) {
    const curTime = new Date().getTime();
    if (curTime - lastEval >= Constants.EVAL_RATE_LIMIT) {
      lastEval = curTime;

      const origin = ExpressionStore.getTextboxPosition();
      const expression = providedExpression || ExpressionStore.getExpression();
      const language = ExpressionStore.getLanguage();
      Dispatcher.dispatch({
        _type: 'API_EVALUATE',
        offset
      });
      API.evaluate(origin, expression, language);
    }
  },

  evaluateActiveHeader() {
    const curTime = new Date().getTime();
    if (curTime - lastHeaderEval >= Constants.EVAL_RATE_LIMIT) {
      lastHeaderEval = curTime;

      NotificationActions.addNotification({
        title: 'Evaluated!',
        level: 'success',
        autoDismiss: 1
      });
      const expression = HeaderStore.getCurrentExpression();
      const language = HeaderStore.getCurrentLanguage();
      API.evaluateHeader(expression, language);
    }
  },

  // Used to open sheets that the user owns.
  openSheet(mySheetId?: string) {
    const sheetId = mySheetId || WorkbookStore.getCurrentSheetId();
    Dispatcher.dispatch({ _type: 'LOADING_DATA' });
    API.openSheet(sheetId);
  },

  openWorkbook(myWorkbookId?: string) {
    const workbookId = myWorkbookId || WorkbookStore.getCurrentWorkbookId();
    Dispatcher.dispatch({ _type: 'LOADING_DATA' });
    API.openWorkbook(workbookId);
  },

  // Used to open sheets that the user has received a link to.
  acquireSheet(sheetId: string) {
    Dispatcher.dispatch({ _type: 'LOADING_DATA' });
    API.acquireSheet(sheetId);
  },

  acquireWorkbook(workbookId: string) {
    Dispatcher.dispatch({ _type: 'LOADING_DATA' });
    API.acquireWorkbook(workbookId);
  },
}
