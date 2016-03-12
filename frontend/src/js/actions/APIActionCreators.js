/* @flow */

import type { Offset } from '../types/Eval';

import API from './ASApiActionCreators';
import Dispatcher from '../Dispatcher';

import GridStore from '../stores/ASGridStore';
import ExpressionStore from '../stores/ASExpressionStore';

export default {
  evaluate(offset: Offset, providedExpression?: string) {
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
