/* @flow */

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';

import type {
  ASLanguage
} from '../types/Eval';

import type {
  EvalHeader
} from '../types/Messages';

export default {
  initialize(expression: string, language: ASLanguage) {
    Dispatcher.dispatch({
      _type: 'HEADER_INITIALIZED',
      expression,
      language
    });
  },

  update(expression: string, language: ASLanguage) {
    Dispatcher.dispatch({
      _type: 'HEADER_UPDATED',
      expression,
      language
    });
  },

  setOutput(value: string, display: ?string) {
    Dispatcher.dispatch({
      _type: 'HEADER_EVALUATED',
      value,
      display
    });
  },

  setLanguage(language: ASLanguage) {
    Dispatcher.dispatch({
      _type: 'HEADER_LANGUAGE_CHANGED',
      language
    });
  }
};
