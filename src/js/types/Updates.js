/* @flow */

import type { 
  ASCell, 
  ASLocation, 
  RangeDescriptor, 
  RangeKey
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
  descriptorUpdates: RangeDescriptorUpdate;
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

export type RangeDescriptorUpdate = {
  tag: 'Update';
  newVals: Array<RangeDescriptor>;
  oldKeys: Array<RangeKey>;
};