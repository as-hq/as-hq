/* @flow */

import type { Callback } from '../types/Base';
import type { StoreToken } from 'flux';
import type { BottomPaneType } from '../types/State';

import React from 'react';

import ConfigStore from '../stores/ASConfigurationStore';
import FocusActions from '../actions/ASFocusActionCreators';

import Focusable from './transforms/Focusable.jsx';

import ASErrorPaneController from './bottom-panes/ASErrorPaneController.jsx';
import ASCellPaneController from './bottom-panes/ASCellPaneController.jsx';
import ASHeaderPaneController from './bottom-panes/ASHeaderPaneController.jsx';
import ASObjectViewerController from './bottom-panes/ASObjectViewerController.jsx';

type Props = {};
type State = {};

class BottomPane extends React.Component {
  static defaultProps = {};
  props: Props;
  state: State;
  _configListener: StoreToken;
  _view: any;
  _onFocus: Callback;

  constructor(props: Props) {
    super(props);
    // overridden by the Focusable HOC
    this._onFocus = () => {};
  }

  componentDidMount() {
    this._configListener = ConfigStore.addListener(() => this.forceUpdate());
  }

  componentWillUnmount() {
    this._configListener.remove();
  }

  render(): React.Element {
    const pane = ConfigStore.getCurrentBottomPane();

    return (
      <div style={styles.full}
           ref={elem => this._view = elem}
           onClick={() => this._onFocus()}
           onMouseEnter={() => FocusActions.hover(name)}
           onMouseLeave={() => FocusActions.unhover(name)}>
        { this._getBottomPane(pane) }
      </div>
    );
  }

  _getBottomPane(pane: BottomPaneType): React.Element {
    switch(pane) {
      case 'errors': {
        return <ASErrorPaneController />;
      }
      case 'header_output': {
        return <ASHeaderPaneController />;
      }
      case 'cell_output': {
        return <ASCellPaneController />;
      }
      case 'object_viewer': {
        return <ASObjectViewerController />;
      }
      default: {
        return <noscript />;
      }
    }
  }

  _takeFocus() {
    this._view.focus();
  }
}

const styles = {
  full: {
  },
};

const name = 'bottompane';

export default Focusable(BottomPane, {
  name,
  addFocusListener: (component, listener) => {
    component._onFocus = () => listener();
  },
  takeFocus: (component) => component._takeFocus()
});
