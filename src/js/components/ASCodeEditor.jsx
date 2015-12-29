/* @flow */

import React from 'react';
import AceEditor from './AceEditor.jsx';
import ActionCreator from '../actions/ASCodeEditorActionCreators';
import Constants from '../Constants';
import Toolbar from './toolbar/Toolbar.jsx';

import type {
  ASFocusType
} from '../types/State';

type ASCodeEditorProps = { 
  handleEditorFocus: () => void; 
  hideToast: () => void; 
  setFocus: (elem: ASFocusType) => void; 
  onDeferredKey: (e: SyntheticKeyboardEvent) => void; 
  maxLines: number; 
  theme?: string; // ::ALEX::
  value: string; 
  width: string; 
  height: string; 
}

export default class ASCodeEditor
  extends React.Component<{}, ASCodeEditorProps, {}>
{
  constructor(props: ASCodeEditorProps) {
    super(props);
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
        <Toolbar />
        <AceEditor
          ref="editor"
          handleEditorFocus={this.props.handleEditorFocus}
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
  onDeferredKey: React.PropTypes.func.isRequired
};

ASCodeEditor.defaultProps = {
  theme: 'monokai'
};

export default ASCodeEditor;