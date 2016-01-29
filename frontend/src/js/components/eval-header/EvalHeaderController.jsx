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
  _view: any;
  _storeListener: () => void;

  constructor(props: {}) {
    super(props);
    this._storeListener = () => this.forceUpdate();
  }

  componentDidMount() {
    HeaderStore.addChangeListener(this._storeListener);
    // focus upon mount
    // TODO integrate with @joel's focus manager
    ReactDOM.findDOMNode(this._view).focus();
  }

  componentWillUnmount() {
    HeaderStore.removeChangeListener(this._storeListener);
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
