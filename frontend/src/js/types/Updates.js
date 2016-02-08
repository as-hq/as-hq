/* @flow */

import type {
  ASLocationObject,
  ASCellObject,
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
  condFormatRuleUpdate: CondFormatRuleUpdate;
  descriptorUpdates: RangeDescriptorUpdate;
};

export type Update = CellUpdate | BarUpdate | CondFormatRuleUpdate | RangeDescriptorUpdate;

export type CellUpdate = UpdateTemplate<ASCellObject, ASLocationObject>

export type BarUpdate = UpdateTemplate<Bar, BarIndex>

export type CondFormatRuleUpdate = UpdateTemplate<CondFormatRule, string>

export type RangeDescriptorUpdate = UpdateTemplate<RangeDescriptor, RangeKey>

export type UpdateTemplate<T, U> = {
  tag: 'Update';
  newVals: Array<T>;
  oldKeys: Array<U>;
};
