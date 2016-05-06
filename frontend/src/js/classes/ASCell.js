/* @flow */

import type {
  ASCellObject,
  ASCellProp,
  ASValue,
  ASExpression,
  ExpandingType,
  RangeKey
} from '../types/Eval';

import DescriptorStore from '../stores/ASRangeDescriptorStore';

import ASIndex from './ASIndex';

import _ from 'lodash';

export default class ASCell {
  _location: ASIndex;
  _expression: ASExpression; // TODO: create a class for this
  _value: ASValue;
  _props: Array<ASCellProp>;
  _expandingType: ?ExpandingType;
  _rangeKey: ?RangeKey;
  _display: ?string;

  get location(): ASIndex { return this._location; }
  get expression(): ASExpression { return this._expression; }
  get value(): ASValue { return this._value; }
  get props(): Array<ASCellProp> { return this._props; }
  get expandingType(): ?ExpandingType { return this._expandingType; }
  get display(): ?string { return this._display; }

  static emptyCellAt(asIndex?: ASIndex): ASCell {
    let cl = asIndex ||
          new ASIndex({
            tag:"index",
            sheetId: "TEST_SHEET_ID",
            index:{row: -1, col:-1}
          }),
        ce = {tag:"Expression",expression: '', language: 'Excel'},
        cv = {tag:"NoValue", contents: []},
        cp = [];
    return new ASCell({
      cellLocation: cl.obj(),
      cellExpression: ce,
      cellValue: cv,
      cellProps: cp
    });
  }

  static makeCells(objs: Array<ASCellObject>): Array<ASCell> {
    return objs.map((x) => new ASCell(x));
  }

  constructor(obj: ASCellObject) {
    let {
      cellLocation,
      cellExpression,
      cellValue,
      cellProps,
      cellRangeKey,
      cellDisplay
    } = obj;

    this._location = new ASIndex(cellLocation);
    this._expression = cellExpression;
    this._value = cellValue;
    this._props = cellProps;
    this._rangeKey = cellRangeKey;
    this._display = cellDisplay;

    if (this._rangeKey) {
      const rd = DescriptorStore.getRangeDescriptor(this._rangeKey);
      if (rd) {
        this._expandingType = rd.expandingType;
        delete this._rangeKey;
      }
    } else {
      this._expandingType = null;
    }
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

  hasProp(prop: ASCellProp): boolean {
    let cellProps = this._props, {tag} = prop;
    let propInd = cellProps.map(({tag}) => tag).indexOf(tag);
    return (propInd >= 0 && _.isEqual(cellProps[propInd], prop));
  }

  hasPropWithTag(propTag: string): boolean {
    return this.props.some(({tag}) => tag === propTag);
  }

  hasFormatType(ftype: string): boolean {
    return this._props.some(prop => {
      return (
        prop.tag === 'ValueFormat' &&
        prop.valFormat && 
        prop.valFormat.formatType === ftype
      );
    });
  }

  isImage(): boolean {
    return this._value.tag === 'ValueImage';
  }

  hasError(): boolean {
    return this._value.tag === 'ValueError';
  }

  hasOutput(): boolean {
    return (this._display != null) && (this._display !== '');
  }

  isObject(): boolean {
    // Object viewer activates for serialized values and long strings
    // for Python and R cells
    const lang = this._expression.language;
    if (lang !== 'Python' && lang !== 'R') {
      return false;
    }
    if (this._value.tag === 'ValueS' && this._value.contents.length > 80) {
      return true;
    } else return this._value.tag === 'ValueSerialized';
  }

}
