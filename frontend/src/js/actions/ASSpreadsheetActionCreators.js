/* @flow */

import type { Offset } from '../types/Hypergrid';

import ASSelection from '../classes/ASSelection';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import KeyUtils from '../AS/utils/Key';
import ASIndex from '../classes/ASIndex';

import API from './ASApiActionCreators';
import SelectionStore from '../stores/ASSelectionStore';
import ExpressionStore from '../stores/ASExpressionStore';
import { actions as Shortcuts } from '../AS/Shortcuts';

const SpreadsheetActions = {

  shiftSelection(offset: Offset, extend: boolean) {
    const selection = SelectionStore.getActiveSelection();
    SpreadsheetActions.select(
      selection.shift(offset, extend)
    );
  },

  select(selection: ASSelection, shouldNotScroll?: boolean) {
    Dispatcher.dispatch({
      _type: 'SELECTION_CHANGED',
      selection,
      shouldNotScroll
    });
  },

  initialize() {
    // XXX (anand) this is a really dumb workaround to the following problem:
    // Hypergrid doesn't let you select A1 as the first selection. Yeah, just that particular cell.
    SpreadsheetActions.select(ASIndex.fromNaked({col: 2, row: 1}).toSelection());
    SpreadsheetActions.select(ASSelection.defaultSelection());
  },

  executeKey(e: SyntheticKeyboardEvent) {
    if (KeyUtils.offsetsSelection(e)) {
      SpreadsheetActions.shiftSelection(
        KeyUtils.keyToOffset(e),
        !! e.shiftKey
      );
    } else {
      Shortcuts.try(e, 'grid');
    }
  }
};

export default SpreadsheetActions;
