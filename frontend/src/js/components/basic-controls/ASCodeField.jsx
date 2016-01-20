/* @flow */

import type {
  Callback
} from '../../types/Base';

import type {
  ASLanguage
} from '../../types/Eval';

import React from 'react';
// $FlowFixMe
import ace from 'brace';

import {logDebug} from '../../AS/Logger';

import U from '../../AS/Util';

import shortid from 'shortid';

const defaultEditorProps = {
  theme  : 'monokai',
  fontSize   : 12,
  showGutter : true,
  readOnly   : false,
  highlightActiveLine : true,
  showPrintMargin     : true,
};

function onPropsSet(editor, props) {
  editor.getSession().setMode('ace/mode/'+props.mode);
  editor.setOption('maxLines', props.maxLines);

  editor.setTheme('ace/theme/'+defaultEditorProps.theme);
  editor.setFontSize(defaultEditorProps.fontSize);
  editor.renderer.setShowGutter(defaultEditorProps.showGutter);

  editor.setOption('readOnly', defaultEditorProps.readOnly);
  editor.setOption('highlightActiveLine', defaultEditorProps.highlightActiveLine);
  editor.setShowPrintMargin(defaultEditorProps.showPrintMargin);

  editor.getSession().setUseSoftTabs(false);
}

type EditorDefaultProps = {
  mode: string;
  value: string;
  height: string;
  width: string;
  maxLines: ?number;

  onKeyDown: Callback<SyntheticKeyboardEvent>;
  onKeyUp: Callback<SyntheticKeyboardEvent>;
  onFocus: Callback<SyntheticFocusEvent>;
};

type EditorProps = {
  mode: string;

  value: string;
  requestChange: Callback<string>;

  onKeyDown: Callback<SyntheticKeyboardEvent>;
  onKeyUp: Callback<SyntheticKeyboardEvent>;
  onFocus: Callback<SyntheticFocusEvent>;

  height: string;
  width: string;
  maxLines: ?number;
  language: ASLanguage;
};

type EditorState = {
  name: string;
};

/*

  PROOF THAT THIS COMPONENT WORKS
  ===============================

  (1) This component maintains the invariant that

                        props.value == editor.getValue()

      If props.value changes, then the editor's value is set to the new prop.

      If editor.getValue() changes, then _handleEditorChange() happens, which
      causes editor's value to be reset to the prop and requestChange() to be
      called, all unless the props.value changed.

  (2) This component will call requestChange() if and only if the user
      organically triggered a value change (by typing).

      Value changes happen through two avenues:

        - typing, in which cause _handleEditorChange happens, and requestChange
        - if props.value changes, in which case everything is silent.

*/

export default class ASCodeField
  extends React.Component<EditorDefaultProps, EditorProps, EditorState>
{
  /***************************************************************************************************************************/
  // React methods

  editor: any;
  silent: boolean;

  constructor(props: EditorDefaultProps) {
    super(props);

    this.silent = false;
    this.state = {
      name: shortid.generate()
    };
  }

  getRawEditor(): any {
    return this.editor;
  }

  componentDidMount() {
    this.editor = ace.edit(this.state.name);
    this.editor.$blockScrolling = Infinity;
    this.editor.setValue(this._getPropsValue(), 1);
    this.editor.on('change', () => this._handleEditorChange());
    this.editor.on('focus', (evt) => this._handleEditorFocus(evt));

    onPropsSet(this.editor, this.props);
  }

  componentWillReceiveProps(nextProps: EditorProps) {
    this._silently(() => {
      const {value} = nextProps;
      if (this.editor.getValue() !== value) {
        this.editor.setValue(value, 1);
      }
    });

    onPropsSet(this.editor, nextProps);
  }

  render(): React.Element {
    let divStyle = {
      width: this.props.width,
      height: this.props.height,
      zIndex: 0
    };

    return (
      <div
        id={this.state.name}
        style={divStyle}
        onKeyDown={(evt) => this._handleKeyDown(evt)}
        onKeyUp={(evt) => this._handleKeyUp(evt)}
      />
    );
  }

  _getPropsValue(): string {
    return this.props.value;
  }

  _propsRequestChange(str: string) {
    this.props.requestChange(str);
  }

  _resetEditorValue() {
    this._silently(() => {
      this.editor.setValue(this._getPropsValue());
    });
  }

  _handleEditorChange() {
    if (!this.silent) {
      const newVal = this.editor.getValue();
      this._resetEditorValue();
      this._propsRequestChange(newVal);
    }
  }

  _handleEditorFocus(evt: SyntheticFocusEvent) {
    this.props.onFocus(evt);
  }

  _handleKeyDown(evt: SyntheticKeyboardEvent) {
    this.props.onKeyDown(evt);
  }

  _handleKeyUp(evt: SyntheticKeyboardEvent) {
    this.props.onKeyUp(evt);
  }

  /*
    NOTE:

    This method does two things to ensure that setValue is relatively side
    effect free.

    - sets silent to true. This ensures that requestChange won't go into an
      infinite loop. (Note that turning it on and then off again is safe to do
      because setValue is synchronously calling onChange, so onChange will in
      fact happen before silent is set to false again.)

    - fixes cursor position. setValue actually sets the cursor position to the
      beginning of the line by default, and cp makes sure that this is reversed.
  */

  _silently(cb: Callback) {
    this.silent = true;
    const cp = this.editor.getCursorPosition();

    cb();

    this.editor.moveCursorToPosition(cp);
    this.silent = false;
  }
}

ASCodeField.defaultProps = {
  mode     : 'python',
  value    : '',
  height   : '100px',
  width    : '100%',
  name     : 'brace-editor',
  maxLines : null,

  onKeyDown(evt) { },
  onKeyUp(evt) { },
  onFocus() { }
};
