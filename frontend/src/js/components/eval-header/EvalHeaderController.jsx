/* @flow */

import type {
  ASLanguage
} from '../../types/Eval';

import React from 'react';
import ReactDOM from 'react-dom';
import EvalHeader from './EvalHeader.jsx';

import HeaderActions from '../../actions/ASHeaderActionCreators';
import HeaderStore from '../../stores/ASHeaderStore';

import * as NotificationActions from '../../actions/ASNotificationActionCreators';
import API from '../../actions/ASApiActionCreators';

// the 'open' prop ensures that the editor re-renders upon being opened.
// otherwise, ace will not attach itself until the next render.
type Props = { open: boolean };

class EvalHeaderController extends React.Component<{}, Props, {}> {
  _view: ReactComponent;
  _storeListener: any;

  constructor(props: Props) {
    super(props);
  }

  componentDidMount() {
    this._storeListener = HeaderStore.addListener(() => this.forceUpdate());
    // focus upon mount
    // TODO this has no effect, integrate with @joel's focus manager
    // ReactDOM.findDOMNode(this._view).focus();
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
    NotificationActions.addSimpleNotification(evaluateMessage, 1);
    API.evaluateHeader(expression, language);
  }
}

const evaluateMessage = 'Evaluated!';

export default EvalHeaderController;
