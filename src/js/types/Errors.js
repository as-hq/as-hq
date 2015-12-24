/* @flow */

import type {
  NakedIndex
} from './Eval';

export type ASExecError = {
  errorMsg: string;
// TODO: split into union
};

export type ASExcelExecError = {
  tag: string;
// TODO: split into union
};

export type ASClientError = {
  location: NakedIndex;
  msg: string;
};
