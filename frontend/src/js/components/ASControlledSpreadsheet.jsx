/* @flow */


import React from 'react';

import ASSelection from '../classes/ASSelection';
import ASPoint from '../classes/ASPoint';

import Focusable from './transforms/Focusable.jsx';
import HOPurify from './transforms/HOPurify.jsx';
import ASSpreadsheet from './ASSpreadsheet.jsx';

import GridStore from '../stores/ASGridStore';

const ASControlledSpreadsheet = HOPurify({
  component: ASSpreadsheet,
  purifiers: {
    selection: {
      addChangeListener({component, listener}) {
        component._grid.addFinEventListener(
          'fin-selection-changed',
          () => listener()
        );
      },

      getValue(component: any): ASSelection {
        return component._getSelection();
      },

      setValue({component, value: selection}) {
        component._select(selection);
      },
    },

    scroll: {
      addChangeListener({component, listener}) {
        component._grid.addFinEventListener(
          'fin-scroll-x',
          () => listener()
        );
        component._grid.addFinEventListener(
          'fin-scroll-y',
          () => listener()
        );
      },

      getValue(component: any): ASPoint {
        return component._getScroll();
      },

      setValue({component, value: scroll}) {
        console.warn('set scroll:', scroll);
        component._scrollTo(scroll);
      }
    }
  },

  onReady(cb: () => void) {
    // purify when hypergrid is ready
    document.addEventListener('fin-ready', cb);
  }
});

const name = 'grid';

export default Focusable(ASControlledSpreadsheet, {
  name,
  addFocusListener: (component, listener) => {
    component.getInstance()._onGridFocus = () => listener();
  },
  takeFocus: (component) => {
    component.getInstance()._grid.takeFocus();
  }
});
