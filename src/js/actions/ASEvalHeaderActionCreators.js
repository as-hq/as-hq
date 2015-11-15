import Dispatcher from '../Dispatcher';
import Constants from '../Constants';

/* The action creator for the REPL just sends a relevant action to Dispatcher */

export default {
  storeEvalHeaderExpression(lang, value) {
    Dispatcher.dispatch({
      type: Constants.ActionTypes.EVAL_HEADER_CLOSED,
      lang: lang,
      value: value
    });
  }
};