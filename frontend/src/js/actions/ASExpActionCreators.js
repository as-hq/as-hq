// @flow

import Dispatcher from '../Dispatcher';
import Constants from '../Constants';

import API from '../actions/ASApiActionCreators';

import ExpStore from '../stores/ASExpStore';
import SelectionStore from '../stores/ASSelectionStore';

import type {
  ASLanguage,
  HAlignType,
  VAlignType,
} from '../types/Eval';

/* The action creator for expression changes */

export default {

  // Handle a change originating from the ace editor
  // xpStr is the new string
  handleEditorChange(xpStr: string) {
    Dispatcher.dispatch({
     _type: Constants.ActionTypes.EDITOR_CHANGED,
      xpStr: xpStr
    });
  },

  // Textbox onkeyup fired, xpStr is the new string
  handleTextBoxChange(xpStr: string) {
    Dispatcher.dispatch({
     _type: Constants.ActionTypes.TEXTBOX_CHANGED,
      xpStr: xpStr
    });
  },

  // Grid key pressed; xpStr is the updated str
  // namely, the old string with a new character at the end
  handleGridChange(xpStr: string, cursorPos: number) {
    Dispatcher.dispatch({
     _type: Constants.ActionTypes.GRID_KEY_PRESSED,
      xpStr: xpStr,
      cursorPos: cursorPos
    });
  },

  handleSelChange(language: ASLanguage, expression: string) {
    Dispatcher.dispatch({
     _type: Constants.ActionTypes.NORMAL_SEL_CHANGED,
      language,
      expression
    });
  },

  handlePartialRefEditor(xpStr: string, excelStr: string) {
    Dispatcher.dispatch({
     _type: Constants.ActionTypes.PARTIAL_REF_CHANGE_WITH_EDITOR,
      xpStr: xpStr,
      excelStr: excelStr
    });
  },

  handlePartialRefTextBox(xpStr: string, excelStr: string) {
    Dispatcher.dispatch({
     _type: Constants.ActionTypes.PARTIAL_REF_CHANGE_WITH_TEXTBOX,
      xpStr: xpStr,
      excelStr: excelStr
    });
  },

  handlePartialRefGrid(excelStr: string) {
    Dispatcher.dispatch({
     _type: Constants.ActionTypes.PARTIAL_REF_CHANGE_WITH_GRID,
      excelStr: excelStr
    });
  },

  handleEscape() {
    Dispatcher.dispatch({
     _type: Constants.ActionTypes.ESC_PRESSED
    });
  },

  setLanguage(language: ASLanguage) {
    Dispatcher.dispatch({
     _type: 'LANGUAGE_CHANGED',
     language
    });

    if (!ExpStore.getUserIsTyping() && ExpStore.getExpression() !== '') {
      SelectionStore.withActiveSelection(({range}) => API.setLanguagesInRange(lang, range));
    }
  },

  // TODO(joel): move setFont, setVAlign, and setHAlign out of here. They're
  // more FormatStore-ish
  setFont(font: string) {
    Dispatcher.dispatch({
      _type: 'SET_FONT',
      font,
    });
  },

  setVAlign(alignment: VAlignType) {
    Dispatcher.dispatch({
      _type: 'SET_VALIGN',
      alignment,
    });
  },

  setHAlign(alignment: HAlignType) {
    Dispatcher.dispatch({
      _type: 'SET_HALIGN',
      alignment,
    });
  },
};
