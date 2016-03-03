/* @flow */

import type {
	ASClipboardAttrs
} from '../../types/Clipboard';

import type {
	ASLanguage
} from '../../types/Eval';

import type {
  EvalInstruction
} from '../../types/Messages';

import {logDebug} from '../Logger';

import CellStore from '../../stores/ASCellStore';
import SheetStateStore from '../../stores/ASSheetStateStore';

import Location from './Location';
import TC from './Conversion';

import ASIndex from '../../classes/ASIndex';
import ASRange from '../../classes/ASRange';

const Clipboard = {

	/* Generates AS-based html from a list of list of values */
	valsToHtml(vals: Array<Array<string>>, rng: ASRange): string {
		let table = document.createElement('table');
		table.setAttribute("id","alphasheets"); // how we indicate the copy originated from within alphasheets
    table.setAttribute("data-sheet-id", SheetStateStore.getCurrentSheetId());
    table.setAttribute("data-from-range", JSON.stringify(rng.obj().range)); // stringified NakedRange
		let tableBody = document.createElement('tbody');
		vals.forEach((rowVals) => {
			let row = document.createElement('tr');
			rowVals.forEach((elem) => {
				let cell = document.createElement('td');
				cell.appendChild(document.createTextNode(elem));
				row.appendChild(cell);
			});
			tableBody.appendChild(row);
		});
		table.appendChild(tableBody);
		return table.outerHTML;
	},

	/* Takes a list of list of values (row-major) and returns a plain string
	Ex. [[3,4]] -> "3\t4" */
	valsToPlain(vals: Array<Array<string>>): string {
		let rowStrs = [];
		vals.forEach((row) => {
			rowStrs.push(row.join('\t'));
		});
		return rowStrs.join('\n');
	},

	/* Checks if a html string was generated by an AS copy */
	htmlStringIsAlphaSheets(s: string): boolean {
		return s.indexOf("alphasheets") >= 0;
		// Simplest solution without creating the DOM element and checking for the tag
	},

  getAttrsFromHtmlString(s: string): ASRange {
    // #needsrefactor should build better model of what causes various formats to show up, and handle them better
    if (s.indexOf("<!--StartFragment-->") == -1) {
      // I'm aware this is dumb, but it's sufficient for now
      // assumes the format is something like:
      // <meta http-equiv="content-type" content="text/html; charset=utf-8"><table id="alphasheets" data-sheet-id="INIT_SHEET_ID" data-from-range="{&quot;tl&quot;:{&quot;row&quot;:6,&quot;col&quot;:2},&quot;br&quot;:{&quot;row&quot;:6,&quot;col&quot;:2}}"><tbody><tr><td>151515</td></tr></tbody></table>
      s += "</meta>"; // works in Chrome, which puts a single unclosed <meta> tag at the beginning of the paste string.
      let parser = new DOMParser(),
          doc = parser.parseFromString(s, "text/xml"),
          table = doc.firstChild.firstChild,
          fromSheetId = (table: any).getAttribute('data-sheet-id'),
          fromRange = JSON.parse((table: any).getAttribute('data-from-range'));
			return ASRange.fromNaked(fromRange, fromSheetId);
    } else {
      // assumes the format is something like
      // <html>
      // <body>
      // <!--StartFragment--><table id="alphasheets" data-sheet-id="Predictions" data-from-range="{&quot;tl&quot;:{&quot;row&quot;:7,&quot;col&quot;:1},&quot;br&quot;:{&quot;row&quot;:7,&quot;col&quot;:1}}"><tbody><tr><td>Critch uses on his own accord by 11/15</td></tr></tbody></table><!--EndFragment-->
      // </body>
      // </html>
      let parser = new DOMParser(),
          doc = parser.parseFromString(s, "text/xml"),
          table = (doc.firstChild: any).firstElementChild.firstElementChild,
          fromSheetId = (table: any).getAttribute('data-sheet-id'),
          fromRange = JSON.parse((table: any).getAttribute('data-from-range'));
			return ASRange.fromNaked(fromRange, fromSheetId);
    }
  },

	/* Takes a text/plain string like "3\t4" and returns a list of list of values (row-major)
	TODO: make correct in all cases (need to look at text/html for that)
	Right now, if a row has a tab, separate by tab. Else, push the row as a single value.
  Works for Sheets, MAY OR MAY NOT work for sheets,Libre,gfin.
	-- Ritesh 10/16
  -- Updated to not include commas, Alex 11/5*/
	plainStringToVals(s: string): Array<Array<string>> {
		logDebug("CONVERTING PLAIN STRING TO VALS: " + s);
		let rows = s.split('\n'),
			vals = [],
			self = Clipboard;
		rows.forEach(function(row) {
      vals.push(row.split('\t'));
		});
		logDebug("VALS: " + JSON.stringify(vals));
		return vals;
	},

	/* Takes an array of strings and replaces all possible entries with numbers */
	formatRow(arr: Array<string>): Array<string|number> {
		let newArr = [];
		arr.forEach(function(elem) {
			let f = parseFloat(elem);
			if (isNaN(f)) {
				newArr.push(elem);
			} else {
				newArr.push(f);
			}
		});
		return newArr;
	},

  /**************************************************************************************************************************/
  /* External conversions */

	_isPlainNumber(str: string): boolean {
		return !isNaN(Number(str));
	},

  externalStringToExpression(str: string, lang: ASLanguage): string {
    if (lang == "Excel") {
      return str;
    } else {
      if (Clipboard._isPlainNumber(str, lang)) {
        return str;
      } else if (str.toUpperCase() == "TRUE") {
        return Clipboard.externalStringToBool(true, lang);
      } else if (str.toUpperCase() == "FALSE") {
        return Clipboard.externalStringToBool(false, lang);
      } else {
        return JSON.stringify(str);
      }
    }
  },

  externalStringToBool(b: boolean, lang: ASLanguage): string {
    if (b) {
      if (["R", "OCaml"].indexOf(lang) != -1) {
        return "true";
      } else {
        return "True";
      }
    } else {
      if (["R", "OCaml"].indexOf(lang) != -1) {
        return "false";
      } else {
        return "False";
      }
    }
  },

  _arrayToEvalInstructions(ind: ASIndex, language: ASLanguage):
	 	(i: number) => (v: string, j: number) => EvalInstruction {
    let self = Clipboard;
     return (i) => {
       return (v, j) => {
				const idx = ind.shift({ dr: i, dc: j });
				const expression = self.externalStringToExpression(v, language);
        return TC.makeEvalInstruction(idx, expression, language);
       };
     };
   },

  _rowValuesToEvalInstructions(ind: ASIndex, language: ASLanguage):
	  (values: Array<string>, i: number) => Array<EvalInstruction> {
    var self = Clipboard;
    return (values, i) => {
      return values.map(self._arrayToEvalInstructions(ind, language)(i));
    };
  },

  // takes in a set of locations and the values at those locations,
  externalStringsToEvalInstructions(ind: ASIndex, strs: Array<Array<string>>, language: ASLanguage):
	 	Array<Array<EvalInstruction>> {
    return strs.map(Clipboard._rowValuesToEvalInstructions(ind, language));
  }
}

export default Clipboard;
