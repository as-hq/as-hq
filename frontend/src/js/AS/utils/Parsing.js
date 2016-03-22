/* @flow */

import type {
  EditorSelection
} from '../../types/Editor';

import type {
  ASLanguage
} from '../../types/Eval';

import _ from 'lodash';

import {logDebug} from '../Logger';

import Constants from '../../Constants';

import StringU from './String';
import Location from './Location';
import Conversion from './Conversion';
import KeyUtils from './Key';

import ASIndex from '../../classes/ASIndex';
import ASRange from '../../classes/ASRange';

import ASExcelRef from '../../classes/ASExcelRef';

// Excel parsing

const Parsing = {

  isFiniteExcelRef(xp: string): boolean {
    let regIdx      = /^!?\$?[A-Za-z]+\$?[0-9]+$/,
        regRng      = /^!?\$?[A-Za-z]+\$?[0-9]+:\$?[A-Za-z]+\$?[0-9]+$/;
    return regIdx.test(xp) || regRng.test(xp);
  },

  isWhitespace(xp: string): boolean {
    return /^\s*$/.test(xp);
  },

  parseRefs(str: string): Array<string> {
    if (str === "") {
      return [];
    } else {
      let regIdx      = /!?\$?[A-Za-z]+\$?[0-9]+/g,
          regRng      = /!?\$?[A-Za-z]+\$?[0-9]+:\$?[A-Za-z]+\$?[0-9]+/g,
          regCols     = /!?\$?[A-Za-z]+(\$?[0-9]+)?:\$?[A-Za-z]+(\$?[0-9]+)?/g, //matches ranges too, but only checks after we remove all ranges (see str2)
          rngs        = str.match(regRng),
          str2        = str.replace(regRng, ""),
          cols        = str2.match(regCols),
          str3        = str2.replace(regCols, ""),
          idxs        = str3.match(regIdx),
          firstNotExc = ((s) => s[0] != '!'),
          rngsOnSheet = rngs ? rngs.filter(firstNotExc) : [],
          idxsOnSheet = idxs ? idxs.filter(firstNotExc) : [],
          colsOnSheet = cols ? cols.filter(firstNotExc) : [],
          matches = rngsOnSheet.concat(idxsOnSheet).concat(colsOnSheet);
      return matches;
    }
  },

  expressionIsCode(exp: string): boolean {
    const nonEmpty = (ln) => ln !== '';
    const lines = exp.split('\n').filter(nonEmpty);

    if (lines.length > 0) {
      return _.startsWith(_.last(lines), '=')
    } else {
      return true;
    }
  },

  parseDependencies(str: string, lang: ?ASLanguage): Array<ASRange> {
    if (Parsing.expressionIsCode(str)) {
      const matches = Parsing.parseRefs(str);
      const parsed = matches.map((m) => ASRange.fromExcelString(m), Parsing);
      return parsed;
    } else {
      return [];
    }
  },

  canInsertRef(prefix: string, suffix: string): boolean {
    const leftNeighbor = prefix.trim().slice(-1);
    const rightNeighbor = suffix.trim()[0];
    const leftSatisfied = refInsertionCharTable.left.includes(leftNeighbor);
    const rightSatisfied = refInsertionCharTable.right.includes(rightNeighbor);
    const prevCharWasSpace = prefix.slice(-1) === ' ';
    return (
      (prefix.length === 0 && rightSatisfied) ||
      (suffix.length === 0 && leftSatisfied) ||
      (rightSatisfied && leftSatisfied) ||
      prevCharWasSpace
    );
  },

  /*
    Extracts the reference your cursor position is on.
   */
  liftHoveredReference(selection: EditorSelection, expression: string): {
    prefix: string;
    ref: ?ASExcelRef;
    suffix: string;
  } {
    const [prefix, suffix] = StringU.splitOnSelection(expression, selection);
    const [refStart, prefixNoRef] = StringU.takeWhileEnd(prefix, isReferenceCharacter);
    const [refEnd, suffixNoRef] = StringU.takeWhile(suffix, isReferenceCharacter);
    try {
      const ref = ASExcelRef.fromString(refStart + refEnd);
      return {prefix: prefixNoRef, ref, suffix: suffixNoRef};
    }
    catch(err) {
      return {prefix, ref: null, suffix};
    }
  }
};

const infixOps = ['+', '*', '=', '-', '/', ',', '&', '|', '%', '^', '>', '<'];
const prefixOps = ['[', '(', '{', '!'];
const postfixOps = [']', ')', '}'];

// TODO language-dependent parsing
const refInsertionCharTable = {
  left: infixOps.concat(prefixOps),
  right: infixOps.concat(postfixOps)
};

function isReferenceCharacter(c: string): boolean {
  return (
    ['!', '$', ':'].includes(c) ||
    /^[A-Za-z0-9]+$/i.test(c)
  );
}

export default Parsing;
