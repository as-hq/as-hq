/* @flow */

import type {
  ASLanguage
} from '../../types/Eval';

import React from 'react';
import ReactDOM from 'react-dom';
import EvalHeader from './EvalHeader.jsx';
import Focusable from '../transforms/Focusable.jsx';

import FocusActions from '../../actions/ASFocusActionCreators';
import HeaderActions from '../../actions/ASHeaderActionCreators';
import HeaderStore from '../../stores/ASHeaderStore';

import NotificationActions from '../../actions/ASNotificationActionCreators';
import API from '../../actions/ASApiActionCreators';

// the 'open' prop ensures that the editor re-renders upon being opened.
// otherwise, ace will not attach itself until the next render.
type Props = { open: boolean };

class EvalHeaderController extends React.Component<{}, Props, {}> {
  _view: any;
  _storeListener: any;

  componentDidMount() {
    this._storeListener = HeaderStore.addListener(() => this.forceUpdate());
  }

  componentWillUnmount() {
    this._storeListener.remove();
  }

  render(): React.Element {
    const expression = HeaderStore.getCurrentExpression();
    const language = HeaderStore.getCurrentLanguage();
    return (
      <EvalHeader
        ref={elem => this._view = elem}
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
    return this._view._editor.editor;
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
