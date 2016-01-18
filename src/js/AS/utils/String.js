/* @flow */

let StringUtils = {
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

  removeLastWord(str: string): string {
    let lastIndex = str.lastIndexOf(" ");
    if (lastIndex > 0)
      return str.substring(0, lastIndex);
    else return "";
  },

  getIndicesOf(searchStr: string, str: string): Array<number> {
    var startIndex = 0, searchStrLen = searchStr.length;
    var index, indices = [];
    while ((index = str.indexOf(searchStr, startIndex)) > -1) {
        indices.push(index);
        startIndex = index + searchStrLen;
    }
    return indices;
  },

  removeEmptyLines(str: string): string {
    var lines = str.split("\n");
    var filtered = lines.filter(StringUtils.isWhitespace);
    return filtered.join("\n");
  },

  isWhitespace(str: string): boolean {
    return /\S/.test(str);
  }
};

export default StringUtils;
