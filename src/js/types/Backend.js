/* @flow */

import type {
  ASRange,
  ASIndex,
  ASCell
} from './Eval';

export type PayloadSelection = {
  tag: 'PayloadSelection';
  selectionRange: ASRange;
  selectionIndex: ASIndex;
};

export type ASBackendPayload =
  PayloadN
  | PayloadInit
  | PayloadDaemonInit
  | PayloadCL
  | PayloadLL
  | PayloadR
  | PayloadS
  | PayloadSelection
  | PayloadJump
  | PayloadSS
  | PayloadWB
  | PayloadWBS
  | PayloadWorkbookSheets
  | PayloadW
  | PayloadU
  | PayloadE
  | PayloadCommit
  | PayloadDelete
  | PayloadPaste
  | PayloadTag
  | PayloadXp
  | PayloadReplValue
  | PayloadList
  | PayloadText
  | PayloadMutate
  | PayloadDrag

export type ASBackendCommit = {
  tag: 'ASCommit';
  before: Array<ASCell>;
  after: Array<ASCell>;
  time: ASBackendTime;
};

export type ASBackendSheet = {
  tag: 'Sheet';
  sheetId: string;
  sheetName: string;
  sheetPermissions: ASPermissions;
};

export type ASBackendWorkbookSheet = {
  tag: 'WorkbookSheet';
  wsName: string;
  wsSheets: Array<ASBackendSheet>;
};
