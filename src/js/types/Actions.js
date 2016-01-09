/* @flow */

import type ASCell from '../classes/ASCell';

import type {
  ASIndexObject,
  ASLocation,
  ASRangeObject,
  ASExpression,
  ASValue,
  ASCompositeValue,
  ASSelectionObject,
  ASLanguage,
  RangeDescriptor,
  RangeKey
} from './Eval';

import type {
  ASViewingWindow
} from './State';

import type {
  CondFormatRule
} from './CondFormat';

import type {
  ASBackendWorkbookSheet,
  ClientMessage,
  EvalHeader
} from './Messages';

import type {
  Bar,
  BarIndex
} from './Bar';

export type GotFailureAction = {
  _type: 'GOT_FAILURE';
  errorMsg: string;
};

export type ScrolledAction = {
  _type: 'SCROLLED';
  vWindow: ASViewingWindow;
};

export type GotOpenAction = {
  _type: 'GOT_OPEN';
  evalHeaders: Array<EvalHeader>;
};

export type GotSelectionAction = {
  _type: 'GOT_SELECTION';
  newSelection: ASSelectionObject;
};

export type GotUpdatedCellsAction = {
  _type: 'GOT_UPDATED_CELLS';
  newCells: Array<ASCell>;
  oldLocs: Array<ASLocation>;
};

export type GotUpdatedRangeDescriptorsAction = {
  _type: 'GOT_UPDATED_RANGE_DESCRIPTORS';
  newRangeDescriptors: Array<RangeDescriptor>;
  oldRangeKeys: Array<RangeKey>;
};

export type GotUpdatedBarsAction = {
  _type: 'GOT_UPDATED_BARS';
  newBars: Array<Bar>;
  oldBarLocs: Array<BarIndex>;
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
  newRules: Array<CondFormatRule>;
  oldRuleIds: Array<string>;
};

// export type ReplLeftAction = {
//   _type: 'REPL_LEFT';
//   lang: ASLanguage;
//   value: string;
// };

export type GotEvalHeaderResponseAction = {
  _type: 'GOT_EVAL_HEADER_RESPONSE';
  response: ASCompositeValue;
};

export type GotFindAction = {
  _type: 'GOT_FIND';
  findLocs: Array<ASIndexObject>;
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
  cursorPos: number;
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
  | GotNewWorkbooksAction
  | DeletedWorkbooksAction
  | GotUpdatedRulesAction
  | ScrolledAction
  | GotOpenAction
  | GotSelectionAction
  | GotUpdatedCellsAction
  | GotUpdatedRangeDescriptorsAction
  | GotUpdatedBarsAction
  | ClearedAction
  | ClearedSheetAction
  // | ReplLeftAction
  // | GotReplResponseAction
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
