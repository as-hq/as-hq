/* @flow */

import type { ASAction } from '../types/Actions';
import type { NakedRange } from '../types/Eval';
import type { Dimensions } from '../types/Hypergrid';

// $FlowFixMe
import { ReduceStore } from 'flux/utils';
import Immutable from 'immutable';
import invariant from 'invariant';
import Dispatcher from '../Dispatcher';

import ASSelection from '../classes/ASSelection';
import ASPoint from '../classes/ASPoint';
import ASRange from '../classes/ASRange';
import ASIndex from '../classes/ASIndex';

import Render from '../AS/Renderers';

import SheetStateStore from './ASSheetStateStore';
import ExpressionStore from './ASExpressionStore';

// #flowlens
type State = any;
const StateRecord = Immutable.Record({
  activeSelection: null,
  lastActiveSelection: null,
  // the top-left visible index on the grid.
  scroll: null,
  width: null,
  height: null
});

class GridStore extends ReduceStore<State> {
  getInitialState(): State {
    return new StateRecord();
  }

  reduce(state: State, action: ASAction): State {
    switch(action._type) {
      case 'LOGIN_SUCCESS': {
        const {sheetId} = action;

        if (state.activeSelection === null) {
          return new StateRecord({
            activeSelection: ASSelection.defaultSelection(sheetId),
            lastActiveSelection: ASSelection.defaultSelection(sheetId),
            scroll: ASPoint.defaultPoint(),
            width: 30,
            height: 20
          });
        } else {
          return state.update('activeSelection', s => s.changeSheet(sheetId))
                      .update('lastActiveSelection', s => s.changeSheet(sheetId));
        }
      }

      case 'START_EDITING':
      case 'EXPRESSION_CHANGED':
      case 'REPAINT_SPREADSHEET': {
        document.dispatchEvent(
          new CustomEvent('grid-repaint')
        );
        return state;
      }

      case 'SELECTION_CHANGED': {
        const {selection, shouldScroll} = action;
        return setActiveSelection(state, selection, shouldScroll);
      }

      case 'CHANGED_SHEET': {
        const {sheetId} = action;
        return state.update('activeSelection', sel => sel.changeSheet(sheetId));
      }

      case 'GRID_DIMENSIONS_CHANGED': {
        const {width, height} = action;
        return state.merge({width, height});
      }

      case 'GRID_SCROLL_CHANGED': {
        return state.set('scroll', action.scroll);
      }

      case 'GRID_SCROLL_OFFSET': {
        const {offset} = action;
        return state.update('scroll', s => s.shift(offset));
      }

      case 'API_EVALUATE': {
        const origin = ExpressionStore.getTextboxPosition();
        return setActiveSelection(
          state,
          origin.shift(action.offset).toSelection(),
          true
        );
      }

      default:
        return state;
    }
  }

  getActiveSelection(): ASSelection {
    const sel = this.getState().activeSelection;
    invariant(sel, 'Active selection not available for authenticated user!');
    return sel;
  }

  getLastActiveSelection(): ASSelection {
    const sel = this.getState().lastActiveSelection;
    invariant(sel, 'Last active selection not available for authenticated user!');
    return sel;
  }

  getScroll(): ASPoint {
    return this.getState().scroll;
  }

  getViewingWindow(): ASRange {
    return ASRange.fromNaked(
      getViewingWindow(this.getState())
    );
  }

  getDimensions(): Dimensions {
    const {width, height} = this.getState();
    return {width, height};
  }

  isVisible(idx: ASIndex): boolean {
    return this.getViewingWindow().contains(idx);
  }

}

function setActiveSelection(
  state: State,
  selection: ASSelection,
  shouldScroll: boolean
) {
  // Render.setSelection() is for speed purposes only. Ideally we would be
  // getting the selection from this store during render, but getting the
  // variable from the store is empirically much slower than just setting
  // its value directly in the file. (Relayed from Anand -- Alex 12/9)
  Render.setSelection(selection);
  let state_ = state;
  if (shouldScroll) {
    state_ = scrollTo(state_, selection);
  }

  return state_.merge({
    activeSelection: selection,
    lastActiveSelection: state.activeSelection,
  });
}

function getViewingWindow(state: State): NakedRange {
  const {scroll: {x, y}, width, height} = state;
  return {
    tl: {col: x, row: y},
    br: {col: x + width, row: y + height}
  };
}

function scrollTo(state: State, {range, origin}: ASSelection): State {
  const {tl, br} = range;
  const {col, row} = origin;
  const {scroll, activeSelection: oldSelection} = state;
  const {x: oldScrollH, y: oldScrollV} = scroll;
  const win = getViewingWindow(state);

  const {origin: oldOrigin, range: oldRange} = oldSelection;
  const {tl: oldTl, br: oldBr} = oldRange;

  // I think this code is a little hacky; I haven't thought this through deeply to ensure that
  // it works in all cases. It does work for ctrl shift arrows and ctrl arrows though. (Alex 11/3/15)

  let scrollV = oldScrollV, scrollH = oldScrollH;

  if (oldOrigin.equals(origin)) {
    if (rowVisible(win, oldTl) && ! rowVisible(win, tl)) {
      scrollV = scrollVForTopEdge(tl.row);
    } else if (rowVisible(win, oldBr) && ! rowVisible(win, br)) {
      scrollV = scrollVForBottomEdge(win, oldScrollV, br.row);
    }

    if (columnVisible(win, oldTl) && ! columnVisible(win, tl)) {
      scrollH = scrollHForLeftEdge(tl.col);
    } else if (columnVisible(win, oldBr) && ! columnVisible(win, br)) {
      scrollH = scrollHForRightEdge(win, oldScrollH, br.col);
    }
  }

  else {
    if (col < win.tl.col) {
      scrollH = scrollHForLeftEdge(col)
    } else if (col > win.br.col) {
      scrollH = scrollHForRightEdge(win, oldScrollH, col);
    }

    if (row < win.tl.row) {
      scrollV = scrollVForTopEdge(row);
    } else if (row > win.br.row) {
      scrollV = scrollVForBottomEdge(win, oldScrollV, row);
    }
  }

  return state.set('scroll', new ASPoint({x: scrollH, y: scrollV}));
}

function rowVisible(viewingWindow: NakedRange, loc: ASIndex): boolean {
  const {tl, br} = viewingWindow;
  return (tl.row <= loc.row && loc.row <= br.row);
}

function columnVisible(viewingWindow: NakedRange, loc: ASIndex): boolean {
  const {tl, br} = viewingWindow;
  return (tl.col <= loc.col && loc.col <= br.col);
}

function scrollVForBottomEdge(viewingWindow: NakedRange, scrollV: number, row: number): number {
  return scrollV + row - viewingWindow.br.row;
}

function scrollVForTopEdge(row: number): number {
  return row;
}

function scrollHForRightEdge(viewingWindow: NakedRange, scrollH: number, col: number): number {
  return scrollH + col - viewingWindow.br.col;
}

function scrollHForLeftEdge(col: number): number {
  return col;
}

export default new GridStore(Dispatcher);
