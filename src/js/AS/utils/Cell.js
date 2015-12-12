/* @flow */

import type {
  ASCell,
  ASCellProp
} from '../../types/Eval';

import _ from 'lodash';

let CellUtils = {
  isEmptyCell(c: ASCell): boolean {
    return !c || ((c.cellExpression.expression == "") && (c.cellProps.length == 0));
  },

  getPropByTag(tag: string, cell: ASCell): ?ASCellProp {
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

  cellHasProp(prop: ASCellProp, cell: ASCell): boolean {
    let {cellProps} = cell, {tag} = prop;
    let propInd = cellProps.map(({tag}) => tag).indexOf(tag);
    return (propInd >= 0 && _.isEqual(cellProps[propInd], prop));
  }
};

export default CellUtils;
