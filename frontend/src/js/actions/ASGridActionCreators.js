/* @flow */

import type { Offset } from '../types/Eval';
import type { Dimensions } from '../types/Hypergrid';

import ASSelection from '../classes/ASSelection';
import ASPoint from '../classes/ASPoint';
import ASRange from '../classes/ASRange';

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';
import KeyUtils from '../AS/utils/Key';
import ASIndex from '../classes/ASIndex';

import API from './ASApiActionCreators';
import APIActions from './APIActionCreators';
import GridStore from '../stores/ASGridStore';
import ExpressionStore from '../stores/ASExpressionStore';
import HeaderStore from '../stores/ASHeaderStore';
import ViewStore from '../stores/ASObjectViewerStore';
import CellStore from '../stores/ASCellStore';

import { actions as Shortcuts } from '../AS/Shortcuts';
import ExpressionActions from './ASExpressionActionCreators';

const GridActions = {

  repaint() {
    document.dispatchEvent(
      new CustomEvent('grid-repaint')
    );
  },

  shiftSelection(offset: Offset, extend?: boolean) {
    const selection = GridStore.getActiveSelection();
    GridActions.select(
      selection.shift(offset, extend)
    );
  },

  select(selection: ASSelection, shouldScroll?: boolean) {
    Dispatcher.dispatch({
      _type: 'SELECTION_CHANGED',
      selection,
      shouldScroll:
        (shouldScroll === undefined) ? true : shouldScroll
    });

    // Ideally, the logic for this should be placed in a clickaway for EvalHeaderController.
    // Unfortunately, no easy clickways work at the moment, so this is our 80/20. (Alex 4/10)
    // ADDITIONALLY, this should eventually save all the unevaluated headers rather
    // than just any active unevaluated headers... probably.
    // #needsrefactor
    if (HeaderStore.isDirty()) {
      APIActions.evaluateActiveHeader();
    }
    // Object viewing
    const loc = GridStore.getActiveSelection().origin;
    const dirty = ViewStore.shouldRecomputeObjectViewAt(loc);
    if (dirty && CellStore.activeCellIsObject()) {
      API.getObjectView(loc);
    }
  },

  scrollBy(offset: Offset) {
    Dispatcher.dispatch({
      _type: 'GRID_SCROLL_OFFSET',
      offset
    });
  },

  scrollTo(scroll: ASPoint) {
    Dispatcher.dispatch({
      _type: 'GRID_SCROLL_CHANGED',
      scroll
    });
  },

  disableScroll() {
    Dispatcher.dispatch({
      _type: 'GRID_SCROLL_DISABLED',
    });
  },

  initialize() {
    // XXX (anand) this is a really dumb workaround to the following problem:
    // Hypergrid doesn't let you select A1 as the first selection. Yeah, just that particular cell.
    GridActions.select(ASIndex.fromNaked({col: 2, row: 1}).toSelection());
    GridActions.select(ASSelection.defaultSelection());
  },


  executeKey(e: SyntheticKeyboardEvent) {
    // initiate 'buffered' editing (capture all keys until
    // textbox actually takes focus)
    if (KeyUtils.initiatesEditMode(e)) {
      KeyUtils.killEvent(e);
      ExpressionActions.startEditingBuffered(
        KeyUtils.keyToString(e)
      );
    }

    // if already editing, send the key to textbox, since
    // we must be in the middle of a focus transition.
    else if (ExpressionStore.isEditing()) {
      ExpressionActions.executeTextboxKey(e);
    }

    // let the window event handler take over.
    else if (KeyUtils.isCopyPasteType(e)) {
      return;
    }

    else {
      KeyUtils.killEvent(e);
      GridActions.executeGridOnlyKey(e);
    }
  },

  executeGridOnlyKey(e: SyntheticKeyboardEvent) {
    if (KeyUtils.offsetsSelection(e)) {
      GridActions.shiftSelection(
        KeyUtils.keyToOffset(e),
        !! e.shiftKey
      );
    } else {
      Shortcuts.try(e, 'grid');
    }
  }
};

export default GridActions;
