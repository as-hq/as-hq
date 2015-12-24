/* @flow */

import type {
  NakedIndex,
  ASLanguage
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
  language: ?ASLanguage;
  msg: string;
};
