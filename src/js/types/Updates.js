/* @flow */

import type { 
  ASCell, 
  ASLocation, 
} from'./Eval';

import type { 
  Bar, 
  BarIndex, 
  BarType, 
  BarProp
} from './Bar';

import type { 
  CondFormatRule
} from './CondFormat';

export type SheetUpdate = {
  tag: 'SheetUpdate';
  cellUpdates: CellUpdate;
  barUpdates: BarUpdate;
  condFormatRulesUpdates: CondFormatRuleUpdate;
  //#incomplete updatedRangeDescriptors: DescriptorUpdate;
};

export type CellUpdate = {
  tag: 'Update';
  newVals: Array<ASCell>;
  oldKeys: Array<ASLocation>;
};

export type BarUpdate = {
  tag: 'Update';
  newVals: Array<Bar>;
  oldKeys: Array<BarIndex>;
};

export type CondFormatRuleUpdate = {
  tag: 'Update';
  newVals: Array<CondFormatRule>;
  oldKeys: Array<string>;
};