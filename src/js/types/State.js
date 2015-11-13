/* @flow */

import type {
  NakedIndex,
  NakedRange,
  ASIndex,
  ASRange
} from './Eval';

export type ASViewingWindow = ASRange;

export type ASSelection = {
  range: NakedRange,
  origin: NakedIndex
};
