/* @flow */

import type {
  ASRange,
  ASIndex,
  ASSheet,
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

export type ASBackendWorkbookSheet = {
  tag: 'WorkbookSheet';
  wsName: string;
  wsSheets: Array<ASSheet>;
};

export type Success = {
  tag: 'Success';
};

export type Failure = {
  tag: 'Failure';
  failDesc: string;
};

export type NoResult = {
  tag: 'NoResult';
};

export type ASBackendResult = Success | Failure | NoResult;

export type NoAction = {
  action: 'NoAction';
};

export type ASMessageAction =
  NoAction
  | Acknowledge
  | SetInitialSheet
  | New | Import
  | Open | Close
  | Evaluate | EvaluateRepl
  | Update
  | Get | Delete
  | Copy | Cut | CopyForced
  | Undo | Redo
  | Clear
  | UpdateWindow
  | SetTag | ToggleTag
  | Repeat
  | BugReport
  | JumpSelect
  | MutateSheet
  | Drag;

export type ASMessagePayload = {
  payload: ASBackendPayload;
};

export type ASMessageResult = {
  result: ASBackendResult;
};

export type ASServerMessage = ASMessageAction & ASMessagePayload & ASMessageResult;

export type ASClientMessage = ASMessageAction & ASMessagePayload;

export type ASAPICallbackPair = {
  fulfill: (msg: ?ASServerMessage) => void;
  reject: (msg: ?ASServerMessage) => void;
};
