/* @flow */

import type { Offset } from '../types/Hypergrid';

import API from './ASApiActionCreators';
import Dispatcher from '../Dispatcher';

import SelectionStore from '../stores/ASSelectionStore';
import ExpressionStore from '../stores/ASExpressionStore';

export default {
  evaluate(moveDirection: Offset, providedExpression?: string) {
    const origin = ExpressionStore.getTextboxPosition();
    const expression = providedExpression || ExpressionStore.getExpression();
    const language = ExpressionStore.getLanguage();
    Dispatcher.dispatch({
      _type: 'API_EVALUATE',
      moveDirection
    });
    API.evaluate(origin, expression, language);
  }
}
