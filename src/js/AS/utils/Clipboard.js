/* @flow */

import type {
	ASClipboardAttrs
} from '../../types/Clipboard';

import type {
	NakedIndex,
	NakedRange,
	ASLanguage,
	ASCell
} from '../../types/Eval';

import {logDebug} from '../Logger';

import CellStore from '../../stores/ASCellStore';
import SheetStateStore from '../../stores/ASSheetStateStore';

import Location from './Location';
import TC from './Conversion';

export default {

	/* Generates AS-based html from a list of list of values */
	valsToHtml(vals: Array<Array<string>>, rng: NakedRange): string {
		let table = document.createElement('table');
		table.setAttribute("id","alphasheets"); // how we indicate the copy originated from within alphasheets
    table.setAttribute("data-sheet-id", SheetStateStore.getCurrentSheet().sheetId);
    table.setAttribute("data-from-range", JSON.stringify(rng));
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

  getAttrsFromHtmlString(s: string): ASClipboardAttrs {
		// I'm aware this is dumb, but it's sufficient for now
    s += "</meta>"; // works in Chrome, which puts a single unclosed <meta> tag at the beginning of the paste string.
    let parser = new DOMParser(),
        doc = parser.parseFromString(s, "text/xml"),
        table = doc.firstChild.firstChild,
        fromSheetId = (table: any).getAttribute('data-sheet-id'),
        fromRange = JSON.parse((table: any).getAttribute('data-from-range'));
    return {fromSheetId: fromSheetId, fromRange: fromRange};
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
			self = this;
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

	_isPercent(str: string): boolean {
		let strLen = str.length;
		return (str[strLen-1] == '%') && this._isPlainNumber(str.substring(0, strLen-1));
	},

	_isCurrency(str: string): boolean {
		return (str[0] == '$') && this._isPlainNumber(str.substring(1));
	},

	_isDate(str: string): boolean {
		let parts = str.split('/');
		// not technically correct, since e.g. 1.0/2.0/3.0 gets counted, but for now
		// this is only used in one place, and having 1.0/2.0/3.0 interpreted literally
		// is fine in that case. (So is, e.g., 100/200/300, which does not make sense
		// as a date.)
		return (parts.length == 3 && parts.every(this._isPlainNumber));
	},

	_isNumeric(str: string, lang: ASLanguage): boolean {
		if (lang != 'Excel') {
			return this._isPlainNumber(str);
		} else {
			return this._isPlainNumber(str) || this._isPercent(str) || this._isCurrency(str) || this._isDate(str);
		}
	},

  externalStringToExpression(str: string, lang: ASLanguage): string {
    if (lang == "Excel") {
      return str;
    } else {
      if (this._isNumeric(Number(str), lang)) {
        return str;
      } else if (str.toUpperCase() == "TRUE") {
        return this.externalStringToBool(true, lang);
      } else if (str.toUpperCase() == "FALSE") {
        return this.externalStringToBool(false, lang);
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

  _arrayToASCells(ind: NakedIndex, language: ASLanguage):
	 	(i: number) => (v: string, j: number) => ASCell {
    let self = this;
     return (i) => {
       return (v, j) => {
        let asIndex = TC.simpleToASIndex(Location.shiftIndex(ind, i, j)),
            xpObj = { expression: self.externalStringToExpression(v, language),
                      language: language} ;
         return TC.makeEvalCell(asIndex, xpObj);
       };
     };
   },

  _rowValuesToASCells(ind: NakedIndex, language: ASLanguage):
	  (values: Array<string>, i: number) => Array<ASCell> {
    var self = this;
    return (values, i) => {
      return values.map(self._arrayToASCells(ind, language)(i));
    };
  },

  // takes in a set of locations and the values at those locations,
  externalStringsToASCells(ind: NakedIndex, strs: Array<Array<string>>, language: ASLanguage):
	 	Array<Array<ASCell>> {
    return strs.map(this._rowValuesToASCells(ind, language));
  }
}
