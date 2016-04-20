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

  getInitialSelectionForText(text: string): EditorSelection {
    //#8020 TODO
    return ({
      range: {
        start: { row: 0, column: text.length },
        end: { row: 0, column: text.length }
      },
      backwards: false
    });
  },

  // returns length of a line in characters.
  getLineLength(text: string): number {
    return text.split('').map((char) => {
      if (char === '\t') {
        return 4;
      } else {
        return 1;
      }
    }).reduce((acc, cur) => acc + cur, 0);
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
  },

  // the usage of this function is temporary, and in the long-run we actually
  // want to be checking for the URL prop
  isLink(text: string): boolean {
    const urlRegex = '^(?!mailto:)(?:(?:http|https|ftp)://)?(?:\\S+(?::\\S*)?@)?(?:(?:(?:[1-9]\\d?|1\\d\\d|2[01]\\d|22[0-3])(?:\\.(?:1?\\d{1,2}|2[0-4]\\d|25[0-5])){2}(?:\\.(?:[0-9]\\d?|1\\d\\d|2[0-4]\\d|25[0-4]))|(?:(?:[a-z\\u00a1-\\uffff0-9]+-?)*[a-z\\u00a1-\\uffff0-9]+)(?:\\.(?:[a-z\\u00a1-\\uffff0-9]+-?)*[a-z\\u00a1-\\uffff0-9]+)*(?:\\.(?:[a-z\\u00a1-\\uffff]{2,})))|localhost)(?::\\d{2,5})?(?:(/|\\?|#)[^\\s]*)?$';
    const url = new RegExp(urlRegex, 'i');
    return text.length < 2083 && url.test(text);
  },

  linkHasProtocol(link: string): boolean {
    return link.split('://').length > 1;
  },

};

window.isLink = StringUtils.isLink;

export default StringUtils;
