/* @flow */

import type { Callback } from '../types/Base';
import type ASSpreadsheet from '../components/ASSpreadsheet.jsx';

export type InitCallback = Callback<({
  spreadsheet: ASSpreadsheet;
  hg: HGElement;
  model: HGBehaviorElement;
  renderer: HGRendererElement;
  cellProvider: HGCellProviderElement;
})>;
