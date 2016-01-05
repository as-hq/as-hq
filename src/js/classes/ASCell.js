/* @flow */

import type {
  ASCellObject,
  ASCellProp,
  ASValue,
  ASExpression
} from '../types/Eval';

import type {
  ASClientExpression
} from '../types/State';

import ASIndex from './ASIndex';

import _ from 'lodash';

export default class ASCell {
  _location: ASIndex;
  _expression: ASExpression; // TODO: create a class for this
  _value: ASValue;
  _props: Array<ASCellProp>;

  static evalCell(asIndex: ASIndex, xpObj: ASClientExpression): ASCell {
    return new ASCell({
      cellLocation: asIndex.obj(),
      cellExpression: {
        tag: 'Expression',
        expression: xpObj.expression,
        language: xpObj.language
      },
      cellValue: {
        tag: 'NoValue',
        contents: []
      },
      cellProps: []
    });
  }

  static emptyCell(asIndex?: ASIndex): ASCell {
    let cl = asIndex ||
          new ASIndex({
            tag:"index",
            sheetId: "TEST_SHEET_ID",
            index:{row: -1, col:-1}
          }),
        ce = {tag:"Expression",expression:"",language:null},
        cv = {tag:"NoValue", contents: []},
        cp = [];
    return new ASCell({
      cellLocation: cl.obj(),
      cellExpression: ce,
      cellValue: cv,
      cellProps: cp
    });
  }

  constructor(obj: ASCellObject) {
    let {
      cellLocation,
      cellExpression,
      cellValue,
      cellProps
    } = obj;

    this._location = new ASIndex(cellLocation);
    this._expression = cellExpression;
    this._value = cellValue;
    this._props = cellProps;
  }

  obj(): ASCellObject {
    let muh = this;
    return ({
      cellLocation: muh._location.obj(),
      cellExpression: muh._expression,
      cellValue: muh._value,
      cellProps: muh._props
    });
  }

  isEmpty(): boolean {
    return (
      (this._expression.expression == "") &&
      (this._props.length == 0)
    );
  }

  getPropByTag(tag: string): ?ASCellProp {
    let retProps = this._props.filter(
          ({tag: cellPropTag}) => tag === cellPropTag
        );
    if (retProps.length === 0) {
      return null;
    } else {
      return retProps[0];
    }
  }

  cellHasProp(prop: ASCellProp): boolean {
    let cellProps = this._props, {tag} = prop;
    let propInd = cellProps.map(({tag}) => tag).indexOf(tag);
    return (propInd >= 0 && _.isEqual(cellProps[propInd], prop));
  }
}
