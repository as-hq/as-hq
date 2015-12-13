/* @flow */

import type {
  ASCurrency
} from '../../types/Format';

let FormatUtils = {
  isFormattable(contents: number): boolean {
    return (!!contents) && !isNaN(contents);
  },

  formatMoney(currency: ASCurrency, contents: number): (?(string|number)) {
    if (!FormatUtils.isFormattable(contents)) {
      return contents;
    }

    let delim: string = '',
        sign: string = '',
        val = contents.toString();
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
    let formatted = (Math.round(contents * 100) / 100).toFixed(2),
        len = formatted.length;
    return sign + formatted.substring(0,len-3) + delim + formatted.substring(len-2);
  },

  formatPercentage(contents: number): (?(number|string)) {
    if (!FormatUtils.isFormattable(contents)) {
      return contents;
    }
    return contents*100 + "%";
  },

  formatDate(contents: number): (?(number|string)) {
    if (!FormatUtils.isFormattable(contents)) {
      return contents;
    }

    let diff = 25566, // number of days between 1/1/1900 (Excel's date base) and 1/1/1970 (Javascript's date base)
        millisecondsElapsed = (contents - diff)*24*60*60*1000,
        d = new Date(millisecondsElapsed);

    return String(Number(d.getMonth()) + 1) + "/" + d.getDate() + "/" + d.getFullYear();
  }
};

export default FormatUtils;
