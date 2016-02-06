/* @flow */

import type {
  ASLanguage
} from '../../types/Eval';

import React from 'react';
import ReactDOM from 'react-dom';
import EvalHeader from './EvalHeader.jsx';

import HeaderStore from '../../stores/ASHeaderStore';

import {addSimpleNotification} from '../../actions/ASNotificationActionCreators';
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
      <EvalHeader ref={elem => this._view = elem}
                  open={this.props.open}
                  expression={expression}
                  language={language}
                  onEvaluate={(newExpression) =>
                    this._onEvaluate(newExpression, language)} />
    );
  }

  _onEvaluate(expression: string, language: ASLanguage) {
    addSimpleNotification(evaluateMessage);
    API.evaluateHeader(expression, language);
  }
}

const evaluateMessage = 'Evaluated!';

export default EvalHeaderController;
