// @flow

import Dispatcher from '../Dispatcher';
import API from './ASApiActionCreators';
import GridStore from '../stores/ASGridStore';

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
        API.setFormat('Money', range);
        break;
      case 'Percentage':
        API.setFormat('Percentage', range);
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
};
