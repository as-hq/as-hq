/* @flow */

import type {
  ASLanguage
} from '../../types/Eval';

import {logDebug} from '../Logger';

import Constants from '../../Constants';

import StringU from './String';
import Location from './Location';
import Conversion from './Conversion';
import KeyUtils from './Key';

import ASIndex from '../../classes/ASIndex';
import ASRange from '../../classes/ASRange';

// Excel parsing

let Parsing = {
  infixOp: ['-','+','==','/','*','&','^',','],
  prefixOp: ['(', '='],
  postfixOp: [')'],

  isFiniteExcelRef(xp: string): boolean {
    let regIdx      = /^!?\$?[A-Za-z]+\$?[0-9]+$/,
        regRng      = /^!?\$?[A-Za-z]+\$?[0-9]+:\$?[A-Za-z]+\$?[0-9]+$/;
    return regIdx.test(xp) || regRng.test(xp);
  },

  isWhitespace(xp: string): boolean {
    return /^\s*$/.test(xp);
  },

  // counts the char : as part of a word.
  getExtendedWordRange(session: AESession, r: number, c: number): AEWordRange {
    let immWordRange = session.getWordRange(r, c),
        wordStart = immWordRange.start.column,
        wordEnd   = immWordRange.end.column;

    let beforeRange = immWordRange.clone();
        beforeRange.start.column = Math.max(0, wordStart-1);

    let afterRange = immWordRange.clone();
        afterRange.end.column = wordEnd + 1; // doesn't matter if wordEnd is too large

    if (wordStart > 0 && session.getTextRange(beforeRange)[0] == ':') {
      wordStart = session.getWordRange(r, wordStart - 1).start.column;
    } else {
      let after = session.getTextRange(afterRange);
      if (after[after.length - 1] == ':') {
        wordEnd = session.getWordRange(r, wordEnd + 1).end.column;
      }
    }

    let extRange = immWordRange.clone();
    extRange.start.column = wordStart;
    extRange.end.column = wordEnd;
    return extRange;
  },

  toggleReference(xp: string): ?string {
    // TODO generalize to arbitrary range lengths
    let deps = Parsing.parseRefs(xp);
    if (deps.length === 0) {
      return null;
    } else if (deps.length > 1) {
      throw "Single word contains multiple references.";
    }

    return Parsing._toggleReference(deps[0]);
  },

  _toggleReference(ref: string): ?string {
    let refs = ref.split(':');
    if (refs.length > 1) {
      return refs.map((r) => Parsing._toggleReference(r)).join(':');
    }

    let dollarIndices = StringU.getIndicesOf('$', ref),
        cleanRef = ref.replace(/\$/g, ''),
        row = cleanRef.split(/[A-Za-z]+/).pop(),
        col = cleanRef.substring(0, cleanRef.length - row.length);
    if (dollarIndices.length === 0) {
      return '$' + col + (row ? '$' + row : ''); // A -> $A, not $A$
    } else if (dollarIndices.length === 1 ) {
      if (dollarIndices[0] === 0) {
        return col + row;
      } else {
        return '$' + col + row;
      }
    } else if (dollarIndices.length === 2) {
      return col + '$' + row;
    } else {
      return null;
    }
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

  parseDependencies(str: string, lang: ?ASLanguage): Array<ASRange> {
    // logDebug("parsing dependencies of: " + str);
    if (lang == 'Excel' && str.length > 0 && str[0] != '=') {
      return [];
    }
    let matches = Parsing.parseRefs(str),
        parsed = matches.map((m) => ASRange.fromExcelString(m), Parsing);
    logDebug("parsed deps: "+JSON.stringify(matches));
    return parsed;
  },

  // TODO: make this actually correct?
  canInsertCellRefInXp(xp: string): boolean {
    let infix = ["+","-","*","/","(",",","&","="];
    return infix.includes(xp.substring(xp.length-1, xp.length));
  },

  // NOTE: do not modify editor
  canInsertCellRef(editor: AERawClass, lastRef: string): boolean {
    let lines = Parsing.getLinesWithoutLastRef(editor, lastRef),
        pos = Parsing.getCursorPosAfterDeletingLastRef(editor, lastRef),
        currentLine = lines[pos.row];
    logDebug("PARSING CELL REF: ", currentLine, pos);
    if (pos.column === 0 || currentLine.length === 0 || lines[0][0] !== '=') {
      return false;
    } else {
      var lookbackOk = false, lookforwardOk = false;
      for (var c = pos.column - 1; c > -1; c--) {
        let curChar = currentLine[c];
        if (curChar === ' ') continue;
        else if (Parsing.prefixOp.includes(curChar) ||
                 Parsing.infixOp.includes(curChar)) {
          lookbackOk = true;
          break;
        } else {
          lookbackOk = false;
          break;
        }
      }
      logDebug("LOOBKACKOK: ", lookbackOk);
      if (lookbackOk && (pos.column === currentLine.length)) return true;
      for (var c = pos.column; c < currentLine.length; c++) {
        let curChar = currentLine[c];
        if (curChar === ' ') continue;
        else if (Parsing.postfixOp.includes(curChar) ||
                 Parsing.infixOp.includes(curChar)) {
          lookforwardOk = true;
          break;
        } else {
          lookforwardOk = false;
          break;
        }
      }
      return lookbackOk && lookforwardOk;
    }
  },

  getLinesWithoutLastRef(editor: AERawClass, lastRef: string): Array<string> {
    if (lastRef !== null) {
      let pos = editor.getCursorPosition(),
        line = editor.getSession().doc.getLine(pos.row),
        lines = editor.getSession().doc.getAllLines(),
        prefixLine = line.substring(0,pos.column-lastRef.length),
        suffixLine = line.substring(pos.column);
        lines[pos.row] = prefixLine + suffixLine;
        return lines
    } else {
        return editor.getSession().doc.getAllLines();
    }
  },

  getCursorPosAfterDeletingLastRef(editor: AERawClass, lastRef: string): AECursorPosition {
    let pos = editor.getCursorPosition();
    if (lastRef !== null) {
      return {row: pos.row, column: pos.column - lastRef.length};
    } else {
      return pos;
    }
  },

  // Given current editor and a string (not null) that's the last ref (ex A4)
  // Delete that from the cursor position
  // Actually modifies editor
  deleteLastRef(editor: AERawClass, lastRef: string) {
    let lines = Parsing.getLinesWithoutLastRef(editor, lastRef);
    let newPos = Parsing.getCursorPosAfterDeletingLastRef(editor, lastRef);
    editor.setValue(lines.join('\n')); // or '' ???
    editor.moveCursorToPosition(newPos);
    editor.clearSelection();
  },

  // Can a reference be inserted after this prefix
  // For example, after '=sum(', a ref can  be inserted
  // Used to see if grid can insert a ref at the end of xp
  canInsertCellRefAfterPrefix(prefix: string): boolean {
    if (prefix.length === 0 || prefix[0] !== '=') return false;
    else {
      for (let c = prefix.length - 1; c > -1; c--) {
        let curChar = prefix[c];
        if (curChar === ' ') continue;
        else if (Parsing.prefixOp.includes(curChar) ||
                 Parsing.infixOp.includes(curChar)) {
          return true;
        } else return false;
      }
    }

    return false;
  }
};

export default Parsing;
