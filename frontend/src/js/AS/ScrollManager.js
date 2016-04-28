/* @flow */

import React from 'react';

import FocusStore from '../stores/ASFocusStore';

import GridActions from '../actions/ASGridActionCreators';

import type { FocusedElement } from '../types/State';

// ::ALEX::
type SyntheticScrollEvent = SyntheticEvent & {wheelDeltaX: number, wheelDeltaY: number};

class ScrollManager {

  shouldManageScroll(hover: FocusedElement, focus: FocusedElement): boolean {
    return (
      couldBeStolen(focus) ||
      (focus !== hover && hover !== null)
    );
  }

  /*
  In general, we want to execute scroll events on the element that's
  hovered, even if it doesn't have focus. For now, the only four
  elements that are hover-able are:
  (1) grid
  (2) editor
  (3) textbox
  (4) header

  When hover === null, an uncontrolled element is currently hovered,
  and ScrollManager will do nothing.
   */
  handleEvent(e: SyntheticScrollEvent) {
    const focus = FocusStore.getFocus();
    const hover = FocusStore.getHoveredElement();

    if (this.shouldManageScroll(hover, focus)) {
      this.executeScroll(e, hover);
    }
  }

  executeScroll(e: SyntheticScrollEvent, hover: FocusedElement) {
    switch(hover) {
      case 'grid': {
        const dX =
          (e.wheelDeltaX > 0) ? -1 :
          ((e.wheelDeltaX < 0) ? 1 : 0);
        const dY=
          (e.wheelDeltaY > 0) ? -1 :
          ((e.wheelDeltaY < 0) ? 1 : 0);
        GridActions.scrollBy({dX, dY});
        break;
      }

      case 'bottombar':
      case 'bottompane': {
        // hypergrid steals the scroll event from this pane (somehow, despite
        // focus not being on it), so disable grid scrolling.
        // grid will auto re-enable scrolling when it detects hover/focus again.
        GridActions.disableScroll();
        break;
      }

      case 'editor':
      case 'textbox':
      case 'header':
      default: {
        // TODO
        break;
      }
    }
  }
}

function couldBeStolen(focus: FocusedElement): boolean {
  return [
    'bottompane',
    'bottombar',
  ].includes(focus);
}

export default new ScrollManager();
