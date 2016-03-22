/* @flow */

import type {
  EditorSelection
} from '../types/Editor';

import type {
  Callback
} from '../types/Base';

import type { StoreToken } from 'flux';

import React from 'react';
import Constants from '../Constants';

import Focusable from './transforms/Focusable.jsx';
import ASControlledCodeField from './basic-controls/ASControlledCodeField.jsx';

import FocusStore from '../stores/ASFocusStore';
import ExpressionStore from '../stores/ASExpressionStore';
import ExpressionActions from '../actions/ASExpressionActionCreators';
import FocusActions from '../actions/ASFocusActionCreators';
import {actions as Shortcuts} from '../AS/Shortcuts';

class ASCodeEditor extends React.Component {
  static defaultProps = {};
  props: {};
  state: {};

  _expressionListener: StoreToken;
  _focusListener: StoreToken;
  _editor: any;

  componentDidMount() {
    this._expressionListener = ExpressionStore.addListener(() => this.forceUpdate());
    this._focusListener = FocusStore.addListener(() => this.forceUpdate());
    this.__getAce().commands.addCommand({
      name: 'altenter',
      bindKey: {win: 'Alt-Enter', mac: 'Alt-Enter'},
      exec: function(editor) {
        editor.insert('\n');
      }
    });
    this.__getAce().commands.addCommand({
      name: 'ctrlenter',
      bindKey: {win: 'Ctrl-Enter', mac: 'Command-Enter'},
      exec: function(editor) {
        editor.insert('\n');
      }
    });
  }

  componentWillUnmount() {
    this._expressionListener.remove();
    this._focusListener.remove();
  }

  render(): React.Element {
    const selection = ExpressionStore.getSelection();
    const expression = ExpressionStore.getExpression();
    const language = ExpressionStore.getLanguage();
    const isActive = FocusStore.isFocused(name);
    const maxLines = isActive ? 10: 1;

    return (
      <div style={styles.root}>
        <ASControlledCodeField
            ref={elem => this._editor = elem}
            name={name}
            style={styles.editor}
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
            theme='monokai'
            scrollMargin={{ top: 5, bottom: 5 }}
            maxLines={maxLines}
            minLines={1}
            language={language}
            onKeyDown={e => this._onKeyDown(e)}
            onMouseEnter={() => FocusActions.hover(name)}
          />
      </div>
    );
  }

  _onKeyDown(e: SyntheticKeyboardEvent) {
    Shortcuts.try(e, 'editor');
  }

  _onSelectionRequestChange(selection: EditorSelection, {eventSource}: any) {
    if (eventSource === 'keynav') {
      if (ExpressionStore.canInsertRefFromEditor('editor')) {
        ExpressionActions.resync();
        return;
      }
    }
    ExpressionActions.setSelection(selection);
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

const name = 'editor';
const styles = {
  root: {
    display: 'flex',
    flexDirection: 'column',
    flexGrow: 0,
    flexShrink: 0,
    flexBasis: 'auto',
  },
  editor: {
    height: '100%'
  }
};

export default Focusable(ASCodeEditor, {
  name,
  addFocusListener: (component, listener) => {
    component._addEventListener('focus', listener);
  },
  takeFocus: (component) => {
    component._takeFocus();
  }
});
