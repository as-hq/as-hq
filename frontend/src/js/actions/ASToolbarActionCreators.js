// @flow

import _ from 'lodash';

import Dispatcher from '../Dispatcher';
import API from './ASApiActionCreators';
import GridStore from '../stores/ASGridStore';

import ASRange from '../classes/ASRange';

import type { RangeBorderType } from '../types/Format';
import type { NakedIndex } from '../types/Eval';
import type {
  FormatType,
  ASCellProp,
  BooleanCellTag,
} from '../types/Eval';

// TODO(joel): A lot of these are really action creator *proxy*s. I don't
// endorse this pattern. I'd much rather create actions directly.
// * formatAs
// * toggleBooleanCellTag
// * setColor
// * handleDecimalChange

export default {
  // Dropdown was clicked, and it is either visible or not as a result (first argument), and has an ID (second argument)
  // click(visible, id) {
  //   Dispatcher.dispatch({
  //   	_type: Constants.ActionTypes.DROPDOWN_CLICKED,
  //    	visible,
  //    	id,
  //   });
  // },

  print() {
    console.error('print not yet implemented!');
  },

  undo() {
    API.undo();
  },

  redo() {
    API.redo();
  },

  paintFormat() {
    console.error('paint format not yet implemented!');
  },

  openItem(name: string) {
    Dispatcher.dispatch({
      _type: 'OPEN_TOOLBAR_ITEM',
      name,
    });
  },

  closeItem(name: string) {
    Dispatcher.dispatch({
      _type: 'CLOSE_TOOLBAR_ITEM',
      name,
    });
  },

  formatAs(tag: FormatType) {
    const {range} = GridStore.getActiveSelection();
    switch (tag) {
      case 'Money':
        API.toggleFormat('Money', range);
        break;
      case 'Percentage':
        API.toggleFormat('Percentage', range);
        break;
      default:
        // TODO(joel) - handle NoFormat / Date
        // $FlowFixMe
        const prop: ASCellProp = {tag, contents: []};
        API.toggleProp(prop, range);
    }
  },

  toggleBooleanCellTag(tag: BooleanCellTag) {
    const {range} = GridStore.getActiveSelection();
    // $FlowFixMe trust me on this one
    const prop: ASCellProp = {tag, contents: []};
    API.toggleProp(prop, range);
  },

  setColor(tag: 'TextColor' | 'FillColor' | 'BorderColor', contents: string) {
    // TODO(joel) figure out BorderColor case
    // $FlowFixMe union types still broken...?
    const prop: ASCellProp = {tag, contents};
    const {range} = GridStore.getActiveSelection();
    API.setProp(prop, range);
  },

  handleDecimalChange(i: number) {
    const {range} = GridStore.getActiveSelection();
    API.handleChangeDecimalPrecision(i, range);
  },

  setFormat(format: FormatType) {
    Dispatcher.dispatch({
      _type: 'SET_FORMAT',
      format,
    });

    // TODO(joel) - don't create two actions!
    const {range} = GridStore.getActiveSelection();
    API.setFormat(format, range);
  },

  setBorder(border: RangeBorderType) {
    const {range} = GridStore.getActiveSelection();
    _.range(range.tl.row, range.br.row+1).forEach(row => {
      _.range(range.tl.col, range.br.col+1).forEach(col => {
        const prop = getBorderProp(border, range, {row, col});
        const rng = ASRange.fromNaked({tl: {row,  col}, br: {row, col}});
        API.setProp(prop, rng);
      });
    });
  },
};

function getBorderProp(border: RangeBorderType, range: ASRange, idx: NakedIndex) {
  let borderTop = false, borderBottom = false, borderLeft = false, borderRight = false;
  if (range.isOnEdge(idx)) {
    const edges = range.getContactedEdges(idx);
    edges.forEach(edge => {
      switch(edge) {
        case 'top':
          borderTop = ['Outer', 'Top'].includes(border);
          borderRight = borderRight || (
            border === 'Inner' &&
            ! edges.includes('right')
          );
          borderBottom = borderBottom || (
            border === 'Inner' &&
            ! edges.includes('bottom')
          );
          break;
        case 'right':
          borderRight = ['Outer', 'Right'].includes(border);
          borderBottom = borderBottom || (
            border === 'Inner' &&
            ! edges.includes('bottom')
          );
          borderLeft = borderLeft || (
            border === 'Inner' &&
            ! edges.includes('left')
          );
          break;
        case 'bottom':
          borderBottom = ['Outer', 'Bottom'].includes(border);
          borderLeft = borderLeft || (
            border === 'Inner' &&
            ! edges.includes('left')
          );
          borderTop = borderTop || (
            border === 'Inner' &&
            ! edges.includes('top')
          );
          break;
        case 'left':
          borderLeft = ['Outer', 'Left'].includes(border);
          borderTop = borderTop || (
            border === 'Inner' &&
            ! edges.includes('top')
          );
          borderRight = borderRight || (
            border === 'Inner' &&
            ! edges.includes('right')
          )
          break;
      }
    });
  } else {
    borderTop = borderBottom = borderLeft = borderRight = (border === 'Inner');
  }
  return {
    tag: 'Border',
    borderTop,
    borderBottom,
    borderLeft,
    borderRight,
  };
}
