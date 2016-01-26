/* @flow */

import type {
  ASAction
} from '../../types/Actions';

import type {
  PairObject
} from '../../types/Base';

import type {
  ASLocation,
  ASSheet,
  ASWorkbook,
} from '../../types/Eval';

import type {
  ASViewingWindow,
  ASClientExpression
} from '../../types/State';

import type {
  ASBackendWorkbookSheet,
  ASClientWindow,
  ServerMessage,
  EvalInstruction,
  ServerAction
} from '../../types/Messages';

import Location from './Location';

import ASIndex from '../../classes/ASIndex';
import ASRange from '../../classes/ASRange';
import ASSelection from '../../classes/ASSelection';

import Constants from '../../Constants';
import CellStore from '../../stores/ASCellStore';
import SheetStateStore from '../../stores/ASSheetStateStore';

let CU = {
  /**************************************************************************************************************************/
  /* Type constructors */

  makeEvalInstruction(asIndex: ASIndex, xpObj: ASClientExpression): EvalInstruction {
    return  {
      tag: "EvalInstruction",
      evalXp: xpObj,
      evalLoc: asIndex.obj()
    };
  },

  makeWorkbookSheet(): ASBackendWorkbookSheet {
    return {
      tag: 'WorkbookSheet',
      wsName: "",
      wsSheets: [{
        tag: 'Sheet',
        sheetId: "",
        sheetName: "",
        sheetPermissions:{
          tag: "Blacklist",
          contents: []
        }
      }]
    };
  },

  makeWorkbook(): ASWorkbook {
    return {
      tag: 'Workbook',
      workbookName: "",
      workbookSheets: []
    };
  },

  makeSheet(sheetName: string): ASSheet {
    return {
      tag: 'Sheet',
      sheetId: "",
      sheetName: sheetName,
      sheetPermissions: {
        tag: 'Blacklist',
        contents: []
      }
    };
  },

  getLocationFromServerAction(action: ServerAction): Array<ASLocation> {
    switch(action.tag) {
      case 'EvalInstruction': {
        return [new action.evalLoc.index];
      }

      case 'Evaluate': {
        return action.contents.map((instr) => {
          return new ASIndex(instr.evalLoc);
        });
      }

      case 'Get': {
        return action.contents.map((idx) => {
          return new ASIndex(idx);
        });
      }

      case 'Delete': {
        return [new ASRange(action.contents)];
      }

      case 'Copy': {
        return [new ASRange(action.copyTo)];
      }

      case 'Cut': {
        return [new ASRange(action.cutFrom),
                new ASRange(action.cutTo)]
      }

      case 'ToggleProp':
      case 'SetProp':
      case 'ChangeDecimalPrecision': {
        return [new ASRange(action.contents[1])];
      }

      case 'Repeat': {
        return [new ASSelection(action.contents).range]
      }

      case 'Drag': {
        return [new ASRange(action.dragRange)];
      }

      default: {
        return [];
      }
    }
  },

  /**************************************************************************************************************************/
  /* Type conversions */

  intToChar(i: number): string {
    return 'ABCDEFGHIJKLMNOPQRSTUVXYZ'.charAt(i);
  },

  charToInt(c: string): number {
    return c.charCodeAt(0) - 64;
  },

  colorToHtml(str: string): string {
    if (str.charAt(0) === "#" || str.substring(0,2) === "rgb") { // if color already correct format
      return str;
    } else {
      let result = CU.colorNameToHex(str);
      if (result === null || result === undefined) {
        throw new Error('Tried to convert an incorrect color name');
      }

      return result;
    }
  },

  asHAlignToHtml(align: string): string {
    switch (align) {
      case 'LeftAlign':
        return 'left';
      case 'HCenterAlign':
        return 'center';
      case 'RightAlign':
        return 'right';
      default:
        throw "Invalid HAlign passed in";
    }
  },

  colorNameToHex(color: string): ?string {
    if (typeof Constants.Colors[color.toLowerCase()] != 'undefined')
        return Constants.Colors[color.toLowerCase()];
    return undefined;
  },

  // indexStringToPair("(a,b)") := {row:a, col:b}
  indexStringToPair(indexString: string): PairObject<number> {
    var ab = indexString.substr(1, indexString.length-2).split(",");
    return {fst : parseInt(ab[0], 10), snd: parseInt(ab[1], 10)};
  },

  /* Gets the top left cell from the listKey. */
  // listKeyToListHead("I/reafe/(a,b)?(c,d)?LIST") := (a,b)"
  listKeyToListHead(listKey: string): (PairObject<number>) {
    if (listKey.split("?").length < 3 || listKey.split("?")[2] != "LIST") {
      throw new Error('There was an error with the format of listkey, no list head.');
    }
    return CU.indexStringToPair((listKey.split("?")[0]).split("/")[2]);
  },

  /* Gets the dimensions of the list from the listKey." */
  // listKeyToListDimensions("I/reafe/(a,b)?(c,d)?LIST") := (c,d)"
  listKeyToListDimensions(listKey: string): (PairObject<number>) {
    if (listKey.split("?").length < 3 || listKey.split("?")[2] != "LIST") {
      throw new Error('There was an error with the format of listkey, no dimensions.');
    }
    return CU.indexStringToPair(listKey.split("?")[1]);
  }
};

export default CU;
