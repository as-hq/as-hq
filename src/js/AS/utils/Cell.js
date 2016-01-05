/* @flow */

import type {
  ASCellObject,
  ASCellProp
} from '../../types/Eval';

import _ from 'lodash';

let CellUtils = {
  isEmptyCell(c: ASCellObject): boolean {
    return !c || ((c.cellExpression.expression == "") && (c.cellProps.length == 0));
  },

  getPropByTag(tag: string, cell: ASCellObject): ?ASCellProp {
    let {cellProps} = cell,
        retProps = cellProps.filter(
          ({tag: cellPropTag}) => tag === cellPropTag
        );
    if (retProps.length === 0) {
      return null;
    } else {
      return retProps[0];
    }
  },

  cellHasProp(prop: ASCellProp, cell: ASCellObject): boolean {
    let {cellProps} = cell, {tag} = prop;
    let propInd = cellProps.map(({tag}) => tag).indexOf(tag);
    return (propInd >= 0 && _.isEqual(cellProps[propInd], prop));
  }
};

export default CellUtils;
