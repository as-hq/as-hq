/* @flow */

import type {
  ASLanguage
} from '../../types/Eval';

import type {
  Callback
} from '../../types/Base';

import type {StoreToken} from 'flux';

import React from 'react';
import ReactDOM from 'react-dom';
import EvalHeader from './EvalHeader.jsx';
import Focusable from '../transforms/Focusable.jsx';

import FocusActions from '../../actions/ASFocusActionCreators';
import HeaderActions from '../../actions/ASHeaderActionCreators';
import HeaderStore from '../../stores/ASHeaderStore';
import ConfigStore from '../../stores/ASConfigurationStore';

import NotificationActions from '../../actions/ASNotificationActionCreators';
import API from '../../actions/ASApiActionCreators';

// the 'open' prop ensures that the editor re-renders upon being opened.
// otherwise, ace will not attach itself until the next render.
type Props = { open: boolean };

class EvalHeaderController extends React.Component {
  static defaultProps = {};
  props: Props;
  state: {};

  _view: any;
  _storeListener: StoreToken;
  _configListener: StoreToken;

  componentDidMount() {
    this._storeListener = HeaderStore.addListener(() => this.forceUpdate());
    this._configListener = ConfigStore.addListener(() => {
      // wait for the changed component (in this case, any of the bottom panes)
      // to finish mounting, then recalculate self.
      setTimeout(() => { this.__getAce().resize() }, 100);
    });
  }

  componentWillUnmount() {
    this._storeListener.remove();
    this._configListener.remove();
  }

  render(): React.Element {
    const expression = HeaderStore.getCurrentExpression();
    const language = HeaderStore.getCurrentLanguage();
    return (
      <EvalHeader
        ref={elem => this._view = elem}
        name={name}
        open={this.props.open}
        expressionLink={{
          value: expression,
          requestChange(newExpression) {
            HeaderActions.update(newExpression, language);
          }
        }}
        languageLink={{
          value: language,
          requestChange(newLanguage) {
            HeaderActions.setLanguage(newLanguage);
          }
        }}
        onEvaluate={() => this._onEvaluate(expression, language)}
      />
    );
  }

  _onEvaluate(expression: string, language: ASLanguage) {
    NotificationActions.addNotification({
      title: evaluateMessage,
      level: 'success',
      autoDismiss: 1
    });
    API.evaluateHeader(expression, language);
  }

  _addEventListener(type: string, cb: Callback) {
    this.__getAce().on(type, cb);
  }

  _takeFocus() {
    this.__getAce().focusSync();
  }

  __getAce() {
    return this._view._editor.getInstance().editor;
  }
}

const evaluateMessage = 'Evaluated!';
const name = 'header';

export default Focusable(EvalHeaderController, {
  name,
  addFocusListener: (component, listener) => {
    component._addEventListener('focus', listener);
  },
  takeFocus: (component) => {
    component._takeFocus();
  }
});
