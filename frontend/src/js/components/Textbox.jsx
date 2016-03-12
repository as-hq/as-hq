/* @flow */

import type {
  EditorSelection,
  EditorIdentifier
} from '../types/Editor';

import type { PXRectangle } from '../types/Render';
import type { Callback } from '../types/Base';
import type { StoreToken } from 'flux';
import type ASIndex from '../classes/ASIndex';

import React from 'react';

import Constants from '../Constants';
import {textbox as zIndex} from '../styles/zIndex';

import Focusable from './transforms/Focusable.jsx';
import ASControlledCodeField from './basic-controls/ASControlledCodeField.jsx';

import ExpressionStore from '../stores/ASExpressionStore';
import FocusActions from '../actions/ASFocusActionCreators';
import ExpressionActions from '../actions/ASExpressionActionCreators';
import ClipboardActions from '../actions/ASClipboardActionCreators';
import {actions as Shortcuts} from '../AS/Shortcuts';

import U from '../AS/Util';

type Props = {
  getPixelCoordinates: (idx: ASIndex) => PXRectangle;
};

class Textbox extends React.Component {
  static defaultProps = {};
  props: Props;
  state: {};

  _storeListener: StoreToken;
  _editor: any;

  componentDidMount() {
    this._storeListener = ExpressionStore.addListener(() => this.forceUpdate());
  }

  componentWillUnmount() {
    this._storeListener.remove();
  }

  render(): React.Element {
    const selection = ExpressionStore.getSelection();
    const expression = ExpressionStore.getExpression();
    const language = ExpressionStore.getLanguage();
    const position = ExpressionStore.getTextboxPosition();
    const style = this._getRootStyle(expression, position);

    return (
      <div style={style}>
        <ASControlledCodeField
            ref={elem => this._editor = elem}
            style={{marginTop: 1}}
            name={name}
            selection={{
              value: selection,
              requestChange: (selection: EditorSelection, metadata: any) =>
                this._onSelectionRequestChange(selection, metadata)
            }}
            text={{
              value: expression,
              requestChange(expression: string) {
                ExpressionActions.setExpression(expression);
              }
            }}
            theme=''
            language={language}
            maxLines={Infinity}
            showGutter={false}
            onKeyDown={e => this._onKeyDown(e)}
            onMouseDown={() => FocusActions.focusTextboxFully()}
            onMouseEnter={() => FocusActions.hover(name)}
          />
      </div>
    );
  }

  _onKeyDown(e: SyntheticKeyboardEvent) {
    ExpressionActions.executeTextboxKey(e);
  }

  _onSelectionRequestChange(selection: EditorSelection, {eventSource}: any) {
    if (eventSource === 'keynav') {
      if (ExpressionStore.canInsertRefFromEditor(name)) {
        ExpressionActions.resync();
        return;
      }
    }
    ExpressionActions.setSelection(selection);
  }

  _getRootStyle(expression: string, position: ASIndex): any {
    const isEditing = ExpressionStore.isEditing();
    const pxPosition = this.props.getPixelCoordinates(position);

    return {
      display: isEditing ? 'block' : 'none',
      position: 'absolute',
      minHeight: pxPosition.extent.y + 1,
      height: 'auto',
      lineHeight: pxPosition.extent.y + 5, /* height of each line in the editor, add 5 to give some leeway at top and bottom */
      top: pxPosition.origin.y - 1,
      left: pxPosition.origin.x - 1,
      zIndex,
      width: this._getWidth(expression, pxPosition),
      border: "solid 2px black",
      background: 'white',
      '-webkit-box-shadow': '1px 2px 5px 0px rgba(0,0,0,0.75)',
      '-moz-box-shadow': '1px 2px 5px 0px rgba(0,0,0,0.75)',
      'box-shadow': '1px 2px 5px 0px rgba(0,0,0,0.75)'
    };
  }

  _getWidth(expression: string, pxPosition: PXRectangle): number {
    const lineLengths = expression
      .split("\n")
      .map(U.String.getLineLength);
    const expressionLength = Math.max(...lineLengths);
    return Math.max(pxPosition.extent.x + 2, expressionLength * fontWidth);
  }

  _addEventListener(type: string, cb: Callback) {
    this.__getAce().on(type, cb);
  }

  _takeFocus() {
    this.__getAce().focusSync();
  }

  __getAce() {
    return this._editor.getInstance().editor;
  }
}

const name = 'textbox';
const fontWidth = 9.4;

export default Focusable(Textbox, {
  name,
  async: true, // take focus asynchronously, since the editor may not have CSS visibility yet.
  addFocusListener: (component, listener) => {
    component._addEventListener('focus', listener);
  },
  takeFocus: (component) => {
    component._takeFocus();
  }
});
