/* @flow */

import type {
  ASCurrency
} from '../../types/Format';

import Constants from '../../Constants';

let FormatUtils = {
  isFormattable(contents: string): boolean {
    return (!!contents) && !isNaN(contents);
  },

  formatMoney(currency: ASCurrency, contents: string): string {
    if (!FormatUtils.isFormattable(contents)) {
      return contents;
    }

    let delim: string = '',
        sign: string = '',
        val = Number(contents);
    switch(currency) {
      case "$":
        delim = ".";
        sign = "$";
        break;
      case "GBP":
        delim = ",";
        sign = "£";
      case "EUR":
        delim = ",";
        sign = "€";
    }
    let formatted = (Math.round(val * 100) / 100).toFixed(2),
        len = formatted.length;
    return String(sign + formatted.substring(0,len-3) + delim + formatted.substring(len-2));
  },

  formatPercentage(contents: string): string {
    if (!FormatUtils.isFormattable(contents)) {
      return contents;
    }
    return Number(contents)*100 + "%";
  },

  // If a string is a float, return the number of decimal places it has
  getNumberOfDecimalPlaces(x: number): number {
    let pieces = String(x).split('.');
    if (pieces.length === 2) {
      return pieces[1].length || 0;
    } else {
      return 0;
    }
  },

  // Given a possible offset and a hypergrid string which may or may not be a number, format it so the
  // number of decimal places it has is: 
  //   - numDecs, if numDec is not null. 
  //   - min(10, its current number of decimal places), if numDec is null. 
  // numDecs is not null exactly when the value passed in belongs to a cell with a ValueFormat
  // that also specifies how many decimal digits to show. 
  formatDecimal(numDecs: ?number, content: string): string {
    let num = parseFloat(content),
        isNumber = !isNaN(num),
        isInteger = Number.isInteger(num);
    if (isNumber && num != null) {
      if (numDecs != null) {
        return String(num.toFixed(numDecs));
      } else { 
        let curNumDecs = FormatUtils.getNumberOfDecimalPlaces(num);
        if (curNumDecs > Constants.numDecimalDigitsToShow) { 
          return String(num.toFixed(Constants.numDecimalDigitsToShow));
        }
      }
    }
    return content;
  },

  formatDate(contents: string): string {
    if (!FormatUtils.isFormattable(contents)) {
      return contents;
    }

    let diff = 25566, // number of days between 1/1/1900 (Excel's date base) and 1/1/1970 (Javascript's date base)
        millisecondsElapsed = (Number(contents) - diff)*24*60*60*1000,
        d = new Date(millisecondsElapsed);

    return String(Number(d.getMonth()) + 1) + "/" + d.getDate() + "/" + d.getFullYear();
  }
};

export default FormatUtils;
