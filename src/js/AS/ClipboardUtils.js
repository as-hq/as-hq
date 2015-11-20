import {logDebug} from './Logger';

import Store from '../stores/ASEvaluationStore';
import TC from './TypeConversions';
import Util from './Util';

export default {

	/* Generates AS-based html from a list of list of values */
	valsToHtml(vals, rng) {
		let table = document.createElement('table');
		table.setAttribute("id","alphasheets"); // how we indicate the copy originated from within alphasheets
    table.setAttribute("data-sheet-id", Store.getCurrentSheet().sheetId); 
    table.setAttribute("data-from-range", JSON.stringify(rng)); 
		let tableBody = document.createElement('tbody');
		vals.forEach(function(rowVals) {
			let row = document.createElement('tr');
			rowVals.forEach(function(elem) {
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
	valsToPlain(vals) {
		let rowStrs = [];
		vals.forEach(function(row) {
			rowStrs.push(row.join('\t'));
		});
		return rowStrs.join('\n');
	},

	/* Checks if a html string was generated by an AS copy */
	htmlStringIsAlphaSheets(s) {
		return s.indexOf("alphasheets") >= 0;
		// Simplest solution without creating the DOM element and checking for the tag
	},

  getAttrsFromHtmlString(s) {
    s += "</meta>"; // works in Chrome, which puts a single unclosed <meta> tag at the beginning of the paste string.
    let parser = new DOMParser(), 
        doc = parser.parseFromString(s, "text/xml"),
        table = doc.firstChild.firstChild,
        fromSheetId = table.getAttribute('data-sheet-id'),
        fromRange = JSON.parse(table.getAttribute('data-from-range'));
    return {fromSheetId: fromSheetId, fromRange: fromRange};
  },

	/* Takes a text/plain string like "3\t4" and returns a list of list of values (row-major)
	TODO: make correct in all cases (need to look at text/html for that)
	Right now, if a row has a tab, separate by tab. Else, push the row as a single value.
  Works for Sheets, MAY OR MAY NOT work for sheets,Libre,gfin.
	-- Ritesh 10/16 
  -- Updated to not include commas, Alex 11/5*/
	plainStringToVals(s) {
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
	formatRow(arr) {
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

  externalStringToExpression(str, lang) {
    if (lang.Server == "Excel") { // is language.Editor the correct thing?
      return str;
    } else if (str != null && typeof(str) != "undefined") {
      if (!isNaN(Number(str))) {
        return str;
      } else if (str.toUpperCase() == "TRUE") {
        return this.externalStringToBool(true, lang.Server);
      } else if (str.toUpperCase() == "FALSE") {
        return this.externalStringToBool(false, lang.Server);
      } else {
        return JSON.stringify(str);
      }
    } else {
      return ""; // unclear if we ever get here -- Alex 10/19
    }
  },

  externalStringToBool(b, lang) {
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
    throw "Should never make it to the end of _dispBoolInLang";
  },

  _arrayToASCells(ind, language) {
    let self = this;
     return function(i) {
       return function(v, j) {
        let asIndex = TC.simpleToASIndex(Util.shiftIndex(ind, i, j)),
            xpObj = { expression: self.externalStringToExpression(v, language),
                      language: language} ;
         return TC.makeEvalCell(asIndex, xpObj);
       };
     };
   },

  _rowValuesToASCells(ind, language) {
    var self = this;
    return function(values, i) {
      return values.map(self._arrayToASCells(ind, language)(i));
    };
  },

  // takes in a set of locations and the values at those locations,
  externalStringsToASCells(ind, strs, language) {
    return strs.map(this._rowValuesToASCells(ind, language));
  }
}
