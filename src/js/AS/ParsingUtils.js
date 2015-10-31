import Util from './Util';
import Constants from '../Constants';
import KeyUtils from './KeyUtils';

// Excel parsing

export default {
  infixOp: ['-','+','==','/','*','&','^',','],
  prefixOp: ['(', '='],
  postfixOp: [')'],

  // NOTE: do not modify editor
  canInsertCellRef(editor,lastRef){
    let lines = this.getLinesWithoutLastRef(editor,lastRef),
        pos = this.getCursorPosAfterDeletingLastRef(editor,lastRef),
        currentLine = lines[pos.row];
    if (pos.column === 0 || currentLine.length === 0){
      return false;
    } else {
      var lookbackOk = false, lookforwardOk = false;
      for (var c = pos.col - 1; c > -1; c--) {
        let curChar = currentLine[c];
        if (curChar === ' ') continue;
        else if (Util.arrContains(this.prefixOp, curChar) ||
                 Util.arrContains(this.infixOp, curChar)) {
          lookbackOk = true;
          break;
        }
      }
      if (lookbackOk && currentLine.length === 1) return true;
      for (var c = pos.col + 1; c < currentLine.length; c++) {
        let curChar = currentLine[c];
        if (curChar === ' ') continue;
        else if (Util.arrContains(this.postfixOp, curChar) ||
                 Util.arrContains(this.infixOp, curChar)) {
          lookforwardOk = true;
          break;
        }
      }
      return lookbackOk && lookforwardOk;
    }
  },

  getLinesWithoutLastRef(editor,lastRef){
    if (lastRef !== null){
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

  getCursorPosAfterDeletingLastRef(editor,lastRef){
    let pos = editor.getCursorPosition();
    if (lastRef !== null){
      return {row: pos.row, column: pos.column - lastRef.length};
    } else {
      return pos;
    }
  },

  // Given current editor and a string (not null) that's the last ref (ex A4)
  // Delete that from the cursor position
  // Actually modifies editor
  deleteLastRef(editor,lastRef){
    let lines = this.getLinesWithoutLastRef(editor,lastRef);
    let newPos = this.getCursorPosAfterDeletingLastRef(editor,lastRef);
    editor.setValue(lines.join('\n')); // or '' ???
    editor.moveCursorToPosition(newPos);
    editor.clearSelection();
    console.log("Just deleted last ref, new expression: " + editor.getValue());
    console.log("Just deleted last ref; cur pos: " + JSON.stringify(editor.getCursorPosition()));
  },

  // Can a reference be inserted after this prefix
  // For example, after '=sum(', a ref can  be inserted
  // Used to see if grid can insert a ref at the end of xp
  canInsertCellRefAfterPrefix(prefix) {
    if (prefix.length === 0) return false;
    else {
      for (let c = prefix.length - 1; c > -1; c--) {
        let curChar = prefix[c];
        if (curChar === ' ') continue;
        else if (Util.arrContains(this.prefixOp, curChar) ||
                 Util.arrContains(this.infixOp, curChar)) {
          return true;
        }
      }
      return false;
    }
  }

}
