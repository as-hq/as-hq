/* @flow */

import type {
  Callback
} from '../types/Base';

import type {InitCallback} from './types';

import Render from '../AS/Renderers';
import ASSpreadsheet from '../components/ASSpreadsheet.jsx';

import {convert} from './helpers';

const callbacks: Array<InitCallback> = [
  // Overriding the renderer's getVisibleColumns() and getVisibleRows(), because they're off by 1...
  ({ renderer }) => {
    renderer.getVisibleColumns = () => {
      // normally getVisibleColumns() just returns renderer.renderedColumns
      let rc = renderer.renderedColumns.slice(0);
      if (rc.length > 0) {
        let last = rc[rc.length - 1];
        if (!isNaN(last)) {
          rc.push(last + 1);
          // rc.push(last + 2);
        }
      }
      return rc;
    };

    renderer.getVisibleRows = () => {
      let rr = renderer.renderedRows.slice(0);
      if (rr.length > 0) {
        let last = rr[rr.length - 1];
        if (!isNaN(last)) {
          rr.push(last + 1);
        }
      }
      return rr;
    };
  },

  ({ model, renderer, cellProvider }) => { // add renderers
    cellProvider.getCell = Render.getCellRenderer;
    renderer.addExtraRenderer(Render.selectionRenderer);
    renderer.addExtraRenderer(Render.dependencyRenderer);
    renderer.addExtraRenderer(Render.draggingRenderer);
    renderer.addExtraRenderer(Render.cornerBoxRenderer);
    renderer.startAnimator();
  }
];

export default convert(callbacks);
