/* @flow */

import type {
  ASLanguage
} from '../../types/Eval';

import React from 'react';
import ReactDOM from 'react-dom';
import EvalHeader from './EvalHeader.jsx';

import HeaderStore from '../../stores/ASHeaderStore';

import HeaderActions from '../../actions/ASHeaderActionCreators';
import API from '../../actions/ASApiActionCreators';

class EvalHeaderController extends React.Component<{}, {}, {}> {
  _view: ReactComponent;
  _storeListener: any;

  constructor(props: {}) {
    super(props);
  }

  componentDidMount() {
    this._storeListener = HeaderStore.addListener(() => this.forceUpdate());
    // focus upon mount
    // TODO integrate with @joel's focus manager
    ReactDOM.findDOMNode(this._view).focus();
  }

  componentWillUnmount() {
    this._storeListener.remove();
  }

  render(): React.Element {
    const expression = HeaderStore.getCurrentExpression();
    const language = HeaderStore.getCurrentLanguage();
    return (
      <EvalHeader ref={elem => this._view = elem}
                  expression={expression}
                  language={language}
                  onSave={(newExpression) =>
                    this._onSave(newExpression, language)} />
    )
  }

  _onSave(expression: string, language: ASLanguage) {
    HeaderActions.update(expression, language);
    API.evaluateHeader(expression, language);
  }
}

export default EvalHeaderController;
