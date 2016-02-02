// @flow

import Dispatcher from '../Dispatcher';

import type ASSelection from '../classes/ASSelection';
import type {ASLanguage} from '../types/Eval';

export function setActiveSelection(selection: ASSelection) {
  Dispatcher.dispatch({
    _type: 'SET_ACTIVE_SELECTION',
    selection,
  });
}
