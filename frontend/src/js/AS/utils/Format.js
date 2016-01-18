/* @flow */

import type {
  ASCurrency
} from '../../types/Format';

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
  getNumberOfDecimalPlaces(content: string): ?number {
    let pieces = content.split('.');
    if (pieces.length === 2) {
      return pieces[1].length || 0;
    } else {
      return null;
    }
  },

  // Given a possible offset and the Hypergrid string, give it the right number of decimal places
  // Doubles will have 10 decimal places by default if they have more than 10 already, as in Google Sheets
  formatDecimal(offset: ?number, content: string): string {
    let numDecPlaces = FormatUtils.getNumberOfDecimalPlaces(content),
        num = parseFloat(content),
        isNumber = !isNaN(num),
        isInteger = Number.isInteger(num);
    if (isNumber && !isInteger && num != null) {
      if (offset != null && numDecPlaces != null) {
        let newNumDecPlaces = (numDecPlaces + offset) >= 0 ? (numDecPlaces + offset) : 0; // don't go lower than 0 decimal places
        newNumDecPlaces = Math.min(newNumDecPlaces, 10);
        return num.toFixed(newNumDecPlaces) + '';
      }  
      if (numDecPlaces != null && numDecPlaces >= 10) { // limit to 10 decimal places
        return num.toFixed(10) + '';
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
