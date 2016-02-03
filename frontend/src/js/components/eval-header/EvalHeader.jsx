/* @flow */

import React from 'react';
import EvalHeaderEditor from '../EvalHeaderEditor.jsx';
import Constants from '../../Constants.js';

import HeaderStore from '../../stores/ASHeaderStore';
import HeaderActions from '../../actions/ASHeaderActionCreators';

// $FlowFixMe
import {AppBar, Toolbar, DropDownMenu, Styles, FlatButton} from 'material-ui';
// $FlowFixMe
let NavigationClose = require('material-ui/lib/svg-icons/navigation/close');

import type {
  ASLanguage
} from '../../types/Eval';

// $FlowFixMe
require('brace/mode/python');
// $FlowFixMe
require('brace/mode/r');
// $FlowFixMe
require('brace/mode/ocaml');
// $FlowFixMe
require('brace/theme/monokai');

type EvalHeaderProps = {
  language: ASLanguage;
  expression: string;
  onEvaluate: (xp: string) => void;
}

type LanguageItem = {
  payload: string;
  text: string;
}

class EvalHeader extends React.Component<{}, EvalHeaderProps, {}> {
  _languages: Array<LanguageItem>;

  constructor(props: EvalHeaderProps) {
    super(props);
    this._languages = [];
    for (const key in Constants.Languages) {
      this._languages.push({ payload: key, text: key });
    }
  }

  shouldComponentUpdate(nextProps: EvalHeaderProps, _: {}): boolean {
    return (
      this.props.language !== nextProps.language ||
      this.props.expression !== nextProps.expression
    );
  }

  render(): React.Element {
    const {language, expression} = this.props;
    const {onEvaluate} = this.props;

    return (
      <div style={styles.root}>

        <Toolbar
          style={styles.toolbar}
          showMenuIconButton={false}>

          <DropDownMenu
            menuItems={this._languages}
            onChange={(_,__,{payload}) =>
                            HeaderActions.setLanguage(payload)}
            selectedIndex={this._getLanguageIndex(language)}
            underlineStyle={styles.dropdownUnderline} />

          <FlatButton
            label={buttonText}
            style={styles.evalButton}
            onClick={() =>
              onEvaluate(this._getValue())} />

        </Toolbar >

        <EvalHeaderEditor ref="editor"
                          name="evalHeader"
                          onSave={(xp) =>
                            HeaderActions.update(xp, language)}
                          mode={Constants.AceMode[language]}
                          language={language}
                          value={expression}
                          height="100%" />

      </div>
    );
  }

  _getValue(): string {
    return this.refs.editor.getRawEditor().getValue();
  }

  _getLanguageIndex(language: ASLanguage): number {
    return this._languages.map(l => l.text).indexOf(language);
  }

};

const buttonText = 'Evaluate';

const styles = {
  root: {
    width: '100%',
    height: '100%',
    marginLeft: '6px'
  },
  toolbar: {
    backgroundColor: Styles.Colors.grey900
  },
  dropdownUnderline: {
    display: 'none'
  },
  evalButton: {
    fontFamily: '"Lucida Console", Monaco, monospace'
  }
};

export default EvalHeader;
