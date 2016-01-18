/* @flow */

import type {Callback} from '../types/Base';
import type ASSpreadsheet from '../components/ASSpreadsheet';
import type {InitCallback} from './types';

export function convert(
  evtHandlers: Array<InitCallback>
): Array<Callback<ASSpreadsheet>> {
  return evtHandlers.map((cb: InitCallback) => {
    return (spreadsheet: ASSpreadsheet) => {
      const hg            = spreadsheet._getHypergrid(),
            model         = hg.getBehavior(),
            renderer      = hg.getRenderer(),
            cellProvider  = model.getCellProvider();
      cb({
        spreadsheet: spreadsheet,
        hg: hg,
        model: model,
        renderer: renderer,
        cellProvider: cellProvider
      });
    };
  });
}
