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

import Util from '../../AS/Util';
import {asCodeField as zIndex} from '../../styles/zIndex';
import FocusStore from '../../stores/ASFocusStore';

import shortid from 'shortid';
import Constants from '../../Constants';

type Props = {
  name: string;
  language: ASLanguage;

  theme: string;
  fontSize: number;
  showGutter: boolean;
  readOnly: boolean;
  highlightActiveLine: boolean;
  showPrintMargin: boolean;
  minLines: ?number;
  maxLines: ?number;
  style?: any;

  onKeyDown: Callback<SyntheticKeyboardEvent>;
  onKeyUp: Callback<SyntheticKeyboardEvent>;
  onFocus: Callback<SyntheticFocusEvent>;
  onMouseDown: Callback;
  onMouseEnter: Callback;
};

export default class ASCodeField extends React.Component {
  static defaultProps: Props = {
    name: shortid.generate(),
    language : 'Python',

    theme: 'monokai',
    fontSize: 14,
    showGutter: true,
    readOnly: false,
    highlightActiveLine: true,
    showPrintMargin: true,
    maxLines: null,
    minLines: 1,

    onKeyDown(evt) { },
    onKeyUp(evt) { },
    onFocus() { },
    onMouseDown() { },
    onMouseEnter() { },
  }; 
  props: Props;
  state: {};

  editor: AERawClass;

  componentDidMount() {
    this.editor = ace.edit(this.props.name);
    this.editor.$blockScrolling = Infinity;
    this._onPropsSet(this.props);
    this.editor.on('alphasheets-keydown', (e) => this.props.onKeyDown(e));
    // $FlowFixMe ::ALEX::
    this.editor.container.addEventListener('mousedown', () => this.props.onMouseDown());
    // $FlowFixMe ::ALEX::
    this.editor.container.addEventListener('mouseenter', () => this.props.onMouseEnter());
  }

  componentWillReceiveProps(nextProps: Props) {
    this._onPropsSet(nextProps);
  }

  isFocused(): boolean {
    return FocusStore.isFocused(this.props.name);
  }

  render(): React.Element {
    const divStyle = {
      zIndex,
      tabIndex: -1,
      ...this.props.style
    };

    return (
      <div
        id={this.props.name}
        style={divStyle}
        onKeyUp={e => this.props.onKeyUp(e)} />
    );
  }

  _onPropsSet(props: Props) {
    this.editor.getSession().setMode('ace/mode/' + Constants.AceMode[props.language]);
    this.editor.setOption('maxLines', props.maxLines);
    this.editor.setOption('minLines', props.minLines);

    this.editor.setTheme('ace/theme/' + props.theme);
    this.editor.setFontSize(props.fontSize);
    this.editor.renderer.setShowGutter(props.showGutter);

    this.editor.setOption('readOnly', props.readOnly);
    this.editor.setOption('highlightActiveLine', props.highlightActiveLine);
    this.editor.setShowPrintMargin(props.showPrintMargin);

    this.editor.getSession().setUseSoftTabs(false);
    this.editor.resize();
  }
}

  