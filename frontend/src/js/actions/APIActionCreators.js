/* @flow */

import type { Offset } from '../types/Eval';

import API from './ASApiActionCreators';
import Constants from '../Constants';
import Dispatcher from '../Dispatcher';

import GridStore from '../stores/ASGridStore';
import ExpressionStore from '../stores/ASExpressionStore';

let lastEval = 0;

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
  }
}
