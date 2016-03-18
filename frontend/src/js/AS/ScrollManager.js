/* @flow */

import React from 'react';

import FocusStore from '../stores/ASFocusStore';

import GridActions from '../actions/ASGridActionCreators';

import type { FocusedElement } from '../types/State';

// ::ALEX::
type SyntheticScrollEvent = SyntheticEvent & {wheelDeltaX: number, wheelDeltaY: number};

const ScrollManager = {

  handleEvent(e: SyntheticScrollEvent) {
    const focus = FocusStore.getFocus();
    const hover = FocusStore.getHoveredElement();

  /*
  In general, we want to execute scroll events on the element that's
  hovered, even if it doesn't have focus. For now, the only four
  elements that are hover-able are:
  (1) grid
  (2) editor
  (3) textbox
  (4) header

  When hover === null, an uncontrolled element is currently hovered,
  and scrollmanager will do nothing.
   */
    if (focus !== hover && hover !== null) {
      ScrollManager.executeScroll(e, hover);
    }
  },

  // ::ALEX::
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

export default ScrollManager;
