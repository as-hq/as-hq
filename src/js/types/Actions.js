/* @flow */

import type {
  ASIndex,
  ASRange,
  ASCell,
  ASExpression,
  ASValue,
  ASCompositeValue,
  ASLanguage,
  ASReplValue
} from './Eval';

import type {
  ASViewingWindow
} from './State';

import type {
  CondFormatRule,
  ASBackendCommit,
  PayloadSelection,
  ASBackendWorkbookSheet,
  ASMessageAction,
  ASServerMessage,
  RowCol,
} from './Messages';

export type GotFailureAction = {
  _type: 'GOT_FAILURE';
  action: ASMessageAction;
  errorMsg: string;
};

export type ScrolledAction = {
  _type: 'SCROLLED';
  vWindow: ASViewingWindow;
};

export type GotUpdatedCellsAction = {
  _type: 'GOT_UPDATED_CELLS';
  updatedCells: Array<ASCell>;
};

export type GotOpenAction = {
  _type: 'GOT_OPEN';
  expressions: Array<ASExpression>;
  initRowCols: Array<RowCol>; 
};

export type GotUndoAction = {
  _type: 'GOT_UNDO';
  commit: ASBackendCommit;
};

export type GotRedoAction = {
  _type: 'GOT_REDO';
  commit: ASBackendCommit;
};

export type GotSelectionAction = {
  _type: 'GOT_SELECTION';
  newSelection: PayloadSelection;
};

export type FetchedCellsAction = {
  _type: 'FETCHED_CELLS';
  newCells: Array<ASCell>;
};

export type ImportAction = {
  _type: 'GOT_IMPORT';
  newCells: Array<ASCell>;
};

export type ClearedAction = {
  _type: 'CLEARED';
};

export type ClearedSheetAction = {
  _type: 'CLEARED_SHEET';
  sheetId: string;
};

export type GotUpdatedWorkbooksAction = {
  _type: 'GOT_UPDATED_WORKBOOKS';
  workbooks: Array<ASBackendWorkbookSheet>
};

export type GotUpdatedRulesAction = {
  _type: 'GOT_UPDATED_RULES';
  rules: Array<CondFormatRule>;
};

export type DeletedLocsAction = {
  _type: 'DELETED_LOCS';
  deletedRange: ASRange;
  updatedCells: Array<ASCell>;
};

export type ReplLeftAction = {
  _type: 'REPL_LEFT';
  lang: ASLanguage;
  value: string;
};

export type GotReplResponseAction = {
  _type: 'GOT_REPL_RESPONSE';
  response: ASReplValue;
};

export type GotEvalHeaderResponseAction = {
  _type: 'GOT_EVAL_HEADER_RESPONSE';
  response: ASCompositeValue;
};

export type GotFindAction = {
  _type: 'GOT_FIND';
  findLocs: Array<ASIndex>;
};

export type GotNewWorkbooksAction = {
  _type: 'GOT_NEW_WORKBOOKS';
  workbooks: Array<ASBackendWorkbookSheet>;
};

export type DeletedWorkbooksAction = {
  _type: 'DELETED_WORKBOOKS';
  workbooks: Array<ASBackendWorkbookSheet>;
};

export type FindIncrementedAction = {
  _type: 'FIND_INCREMENTED';
};

export type FindDecrementedAction = {
  _type: 'FIND_DECREMENTED';
};

export type GridKeyPressedAction = {
  _type: 'GRID_KEY_PRESSED';
  xpStr: string;
};

export type EditorChangedAction = {
  _type: 'EDITOR_CHANGED';
  xpStr: string;
};

export type TextboxChangedAction = {
  _type: 'TEXTBOX_CHANGED';
  xpStr: string;
};

export type NormalSelChangedAction = {
  _type: 'NORMAL_SEL_CHANGED';
  xpStr: string;
};

export type PartialRefChangeWithEditorAction = {
  _type: 'PARTIAL_REF_CHANGE_WITH_EDITOR';
  xpStr: string;
  excelStr: string; // TODO: can we impose any constraints here
};

export type PartialRefChangeWithGridAction = {
  _type: 'PARTIAL_REF_CHANGE_WITH_GRID';
  excelStr: string;
};

export type PartialRefChangeWithTextboxAction = {
  _type: 'PARTIAL_REF_CHANGE_WITH_TEXTBOX';
  xpStr: string;
  excelStr: string;
};

export type EscPressedAction = {
  _type: 'ESC_PRESSED';
};

export type BackendUpdatedAndCellsChangedAction = {
  _type: 'BACKEND_UPDATED_AND_CELLS_CHANGED';
};

export type EvalTriedToDecoupleAction = {
  _type: 'EVAL_TRIED_TO_DECOUPLE';
};

export type WorkbookAction =
  GotUpdatedWorkbooksAction
  | GotNewWorkbooksAction
  | DeletedWorkbooksAction;

export type ASAction =
  GotFailureAction
  | GotUpdatedWorkbooksAction
  | GotUpdatedRulesAction
  | GotNewWorkbooksAction
  | DeletedWorkbooksAction
  | ScrolledAction
  | GotUpdatedCellsAction
  | GotOpenAction
  | GotUndoAction
  | GotRedoAction
  | GotSelectionAction
  | FetchedCellsAction
  | ImportAction
  | ClearedAction
  | ClearedSheetAction
  | DeletedLocsAction
  | ReplLeftAction
  | GotReplResponseAction
  | GotEvalHeaderResponseAction
  | GotFindAction
  | FindIncrementedAction
  | FindDecrementedAction
  // three-way data integration actions
  | GridKeyPressedAction
  | EditorChangedAction
  | TextboxChangedAction
  | NormalSelChangedAction
  | PartialRefChangeWithEditorAction
  | PartialRefChangeWithGridAction
  | PartialRefChangeWithTextboxAction
  | EscPressedAction
  | BackendUpdatedAndCellsChangedAction
  | EvalTriedToDecoupleAction;
