/* @flow */

import type ASIndex from '../classes/ASIndex';

import type {
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
  location: ASIndex;
  language: ?ASLanguage;
  msg: string;
};
