/* @flow */


import React from 'react';

import ASSelection from '../classes/ASSelection';
import Focusable from './transforms/Focusable.jsx';
import HOPurify from './transforms/HOPurify.jsx';
import ASSpreadsheet from './ASSpreadsheet.jsx';

import SpreadsheetActions from '../actions/ASSpreadsheetActionCreators';
import SelectionStore from '../stores/ASSelectionStore';

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
        return component.getSelectionArea();
      },

      setValue({component, value: selection}) {
        component.select(
          selection,
          SelectionStore.shouldScroll()
        );
      },
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
