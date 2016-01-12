/* @flow */

import React from 'react';
import AceEditor from './AceEditor.jsx';
import ActionCreator from '../actions/ASCodeEditorActionCreators';
import ExpStore from '../stores/ASExpStore';
import Constants from '../Constants';
import FocusStore from '../stores/ASFocusStore';

import type {
  ASFocusType
} from '../types/State';

type ASCodeEditorProps = {
  handleEditorFocus: () => void;
  hideToast: () => void;
  setFocus: (elem: ASFocusType) => void;
  onDeferredKey: (e: SyntheticKeyboardEvent) => void;
  maxLines: number;
  theme: string;
  value: string;
  width: string;
  height: string;
}

type ASCodeEditorDefaultProps = {
  theme: string;
};

export default class ASCodeEditor
  extends React.Component<ASCodeEditorDefaultProps, ASCodeEditorProps, {}>
{
  constructor(props: ASCodeEditorProps) {
    super(props);
  }

  handleEditorFocus() {
    FocusStore.refocus();

    if (this.props.handleEditorFocus()) {
      this.props.handleEditorFocus();
    }
  }

  render(): React.Element {
    const {theme, value, width, height} = this.props;
    const outerStyle = {
      display: 'flex',
      flexDirection: 'column',
      flexGrow: 0,
      flexShrink: 0,
      flexBasis: 'auto'
    };

    return (
      <div style={outerStyle}>
        <AceEditor
          ref="editor"
          handleEditorFocus={() => this.handleEditorFocus()}
          hideToast={this.props.hideToast}
          theme={theme}
          width="100%"
          height="100%"
          maxLines={this.props.maxLines}
          setFocus={this.props.setFocus}
          onDeferredKey={this.props.onDeferredKey} />
      </div>
    );
  }
}

ASCodeEditor.propTypes = {
  onDeferredKey: React.PropTypes.func.isRequired,
  handleEditorFocus: React.PropTypes.func,
};

ASCodeEditor.defaultProps = {
  theme: 'monokai'
};
