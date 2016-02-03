/* @flow */

/*
  This view component renders ANSI-formatted text in a bottom pane with a title.
*/

import React from 'react';
import {Paper} from 'material-ui';

// $FlowFixMe declaring this is not urgent right now
import Ansi from 'ansi_up';

import ASIndex from '../../classes/ASIndex';

type ASOutputPaneProps = {
  ansiContent: ?string;
  title: ?string;
};

class ASOutputPane extends React.Component<{}, ASOutputPaneProps, {}> {

  shouldComponentUpdate(nextProps: ASOutputPaneProps, _: {}): boolean {
    return (
      this.props.ansiContent !== nextProps.ansiContent ||
      this.props.title !== nextProps.title
    );
  }

  render(): React.Element {
    let {ansiContent, title} = this.props;

    return (
      <div style={styles.root} >

        <Paper style={styles.topBar}>
          <span style={styles.topBarTitle}>
            {title}
          </span>
        </Paper>

        <Paper style={styles.contentPane}>
          <div style={styles.contentContainer} >
            {ansiContent ?
              this._getFormattedAnsiHTML(ansiContent)
              : <h3 style={styles.altMessage}>Nothing to display.</h3>}
          </div>
        </Paper>

      </div>
    );
  }

  // transform an ANSI-formatted string into a list of formatted div's
  _getFormattedAnsiHTML(content: string): Array<ReactElement> {
    return content
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;')
            .split('\n')
            .map((line) =>
      (
        <div
          style={styles.outputLine}
          dangerouslySetInnerHTML={
            {__html: Ansi.ansi_to_html(line, {use_classes: true})}
          } />
      )
    );
  }
}

const styles = {
  root: {
    height: '100%',
    width: '100%'
  },
  topBar: {
    height: '26px',
    width: '100%',
    background: '#212121'
  },
  topBarTitle: {
    color: '#f8f8f2',
    fontSize: '12',
    lineHeight: '26px',
    fontWeight: 'bold',
    position: 'inline',
    paddingLeft: '10px'
  },
  contentPane: {
    height: 'calc(100% - 26px)',
    width: '100%',
    overflow: 'auto'
  },
  outputLine: {
    lineHeight: '14px',
    fontFamily: 'monospace',
    color: '#f8f8f2' // the default un-formatted text color
  },
  contentContainer: {
    paddingTop: '5px',
    paddingLeft: '10px'
  },
  altMessage: {
    color: 'grey'
  }
};

export default ASOutputPane;
