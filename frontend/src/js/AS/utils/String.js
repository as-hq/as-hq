/* @flow */

import type { EditorSelection } from '../../types/Editor';

const StringUtils = {
  // named after the Ruby function.
  toSentence(strs: Array<string>): string {
    switch (strs.length) {
      case 0: return "";
      case 1: return strs[0];
      case 2: return strs[0] + " " + strs[1];
      default:
        let strs2 = strs.splice(0);
        strs2[strs2.length-1] = "and " + strs2[strs2.length-1];
        return strs2.join(', ');
    }
  },

  // returns length of a line in characters.
  getLineLength(text: string): number {
    let length = 0;
    for (var c in text) {
      if (c === '\t') length += 4;
      else length += 1;
    }
    return length;
  },

  getSelectionLead({range, backwards}: EditorSelection): AEPoint {
    return backwards ?
      range.start :
      range.end;
  },

  selectionIsEnd({range: {start, end}}: EditorSelection, text: string): boolean {
    if (
      start.column === end.column &&
      start.row === end.row
    ) {
      const lines = text.split('\n');
      return (
        lines.length - 1 === start.row &&
        lines.slice(-1)[0].length === start.column
      );
    } else {
      return false;
    }
  },

  splitOnSelection(
    text: string,
    selection: EditorSelection): [string, string] {

    const lead = StringUtils.getSelectionLead(selection);
    const lines = text.split('\n');
    const roundedPrefix = lines.slice(0, lead.row).join('\n');
    const roundedSuffix = lines.slice(lead.row + 1, lines.length).join('\n');
    const prefix =
      roundedPrefix + (roundedPrefix.length > 0 ? '\n' : '') +
      lines[lead.row].substring(0, lead.column);
    const suffix =
      lines[lead.row].substring(lead.column) +
      (roundedSuffix.length > 0 ? '\n' : '') + roundedSuffix;
    return [prefix, suffix];
  },

  takeWhile(text: string, f: (c: string) => boolean): [string, string] {
    let i = 0;
    while(f(text[i]) && i < text.length) {
      i++;
    }
    return [
      text.substring(0, i),
      text.substring(i)
    ];
  },

  takeWhileEnd(text: string, f: (c: string) => boolean): [string, string] {
    let i = text.length - 1;
    while(f(text[i]) && i > 0) {
      i--;
    }
    return [
      text.substring(i+1),
      text.substring(0, i+1)
    ]
  }
};

export default StringUtils;
