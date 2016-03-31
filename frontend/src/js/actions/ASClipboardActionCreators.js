/* @flow */

import Dispatcher from '../Dispatcher';
import API from './ASApiActionCreators';
import ConfigActions from './ASConfigActionCreators';
import GridActions from './ASGridActionCreators.js';
import NotificationActions from './ASNotificationActionCreators';

import FocusStore from '../stores/ASFocusStore';
import GridStore from '../stores/ASGridStore';
import CellStore from '../stores/ASCellStore';
import SheetStateStore from '../stores/ASSheetStateStore';
import ExpressionStore from '../stores/ASExpressionStore';
import ModalStore from '../stores/ASModalStore';

import U from '../AS/Util';
import Render from '../AS/Renderers';

const ClipboardActions = {
  /*
  executeKey(e: SyntheticClipboardEvent) {
    switch(e.which) {
      case 67:
        ClipboardActions.copy(e);
        break;
      case 88:
        ClipboardActions.cut(e);
        break;
      case 86:
        ClipboardActions.paste(e);
        break;
      default:
        break;
    }
  }, */

  copy(e: SyntheticClipboardEvent) {
    if (! ModalStore.isAnyOpen()) {
      switch(FocusStore.getFocus()) {
        case 'grid':
          handleCopyTypeEventForGrid(e, false);
          break;

        default:
          break;
      }
    }
  },

  cut(e: SyntheticClipboardEvent) {
    if (! ModalStore.isAnyOpen()) {
      switch(FocusStore.getFocus()) {
        case 'grid':
          handleCopyTypeEventForGrid(e, true);
          break;

        default:
          break;
      }
    }
  },

  paste(e: SyntheticClipboardEvent) {
    switch(FocusStore.getFocus()) {
      case 'grid':
        handlePasteEventForGrid(e);
        break;

      default:
        break;
    }
  }
}

/**
 * Handles cut and copy events fired on the grid.
 * @param  {[type]}  e:      SyntheticClipboardEvent [description]
 * @param  {[type]} isCut?: boolean                 [description]
 */
function handleCopyTypeEventForGrid(e: SyntheticClipboardEvent, isCut?: boolean) {
  // For now, the killEvent doesn't kill fin-hypergrid's default copy handler, since
  // fin's hypergrid component is a child of ASEvaluationPane. If all this code
  // gets commented out, copy actually works mostly as expected, EXCEPT that
  // the table saved to the clipboard (from "let html = ...") doesn't have
  // id=alphasheets set, which is how we know we the clipboard content is
  // from AlphaSheets originally.
  //
  // Alex 10/29 -- nope, killEvent actually does something, and I don't understand what.
  // I DO know that if you leave it out, cut doesn't save anything to the clipboard
  // if there's already external data on the clipboard, but copy DOES work, and I don't
  // understand why.
  U.Key.killEvent(e);

  const sel = GridStore.getActiveSelection();
  const vals = CellStore.getRowMajorCellValues(sel.range);

  // XXX should not mutate stores!!!
  SheetStateStore.setClipboard(sel, isCut);
  const html = U.Clipboard.valsToHtml(vals, sel.range);
  const plain = U.Clipboard.valsToPlain(vals);
  e.clipboardData.setData("text/html",html);
  e.clipboardData.setData("text/plain",plain);
  GridActions.repaint(); // render immediately
}

function handlePasteEventForGrid(e: SyntheticClipboardEvent) {
  Render.setMode(null);

  const sel = GridStore.getActiveSelection();

  const containsHTML = e.clipboardData.types.includes("text/html");
  const containsPlain = e.clipboardData.types.includes("text/plain");
  const isAlphaSheets = API.isTesting ||
        (
          containsHTML
            ? U.Clipboard.htmlStringIsAlphaSheets(e.clipboardData.getData("text/html"))
            : false
        );

  // #incomplete should either be checking if you're from the same sheet, OR support
  // copy/pasting across sheets.
  if (isAlphaSheets) { // From AS
    const clipboard = SheetStateStore.getClipboard();
    const sheetId = SheetStateStore.getCurrentSheetId();
    const toASRange = sel.range;

    let fromRange = U.Clipboard.getAttrsFromHtmlString(e.clipboardData.getData("text/html"));
    let fromSheetId = sel.range.sheetId;

    // clipboard.area is basically obsolete, except for allowing copy/paste on Macs (which currently
    // can't copy contents onto the clipboard directly).
    if (API.isTesting || U.Browser.isMac()) {
      if (!! clipboard.area) {
        fromRange   = clipboard.area.range;
        fromSheetId = SheetStateStore.getCurrentSheetId();
      }
    }

    if (fromRange) {
      if (clipboard.isCut && sheetId == fromSheetId) { // only give cut behavior within sheets
        API.cut(fromRange, toASRange);
        // XXX should not mutate stores!!!
        SheetStateStore.setClipboard(null, false);
      } else {
        API.copy(fromRange, toASRange);
      }
    } else {
      NotificationActions.addNotification({
        title: 'Nothing in clipboard.',
        level: 'error',
        autoDismiss: 2
      });
    }

    GridActions.repaint(); // render immediately

  } else { // Not from AS

    if (containsPlain) {
      const lang = ExpressionStore.getLanguage();
      const plain = e.clipboardData.getData("text/plain");
      const vals = U.Clipboard.tabularStringToTable(plain);
      const evalInstructions2d = U.Clipboard.externalStringsToEvalInstructions(sel.origin, vals, lang);
      const evalInstructions = U.Array.concatAll(evalInstructions2d);
      API.pasteSimple(evalInstructions);
      // The normal eval handling will make the paste show up
    } else {
      // TODO: Not handling html conversion for now
      // Not sure if getData is smart enough to do that for you
    }
  }
}

export default ClipboardActions;
