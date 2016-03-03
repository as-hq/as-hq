/* @flow */

import React from 'react';

import Constants from '../../Constants.js';

import HeaderStore from '../../stores/ASHeaderStore';
import HeaderActions from '../../actions/ASHeaderActionCreators';
import { actions as Shortcuts } from '../../AS/Shortcuts';

import KeyUtils from '../../AS/utils/Key';
// $FlowFixMe
import {Toolbar, Styles, FlatButton} from 'material-ui';
import ASControlledCodeField from '../basic-controls/ASControlledCodeField.jsx';
import ASDropdownMenu from '../basic-controls/ASDropdownMenu.jsx';
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
  open: boolean;
  languageLink: ReactLink<ASLanguage>;
  expressionLink: ReactLink<string>;
  selectionLink: ReactLink<EditorSelection>;
  onEvaluate: () => void;
}

type LanguageItem = {
  payload: any;
  text: any;
}

class EvalHeader extends React.Component<{}, EvalHeaderProps, {}> {
  _languages: Array<LanguageItem>;
  _editor: any;

  constructor(props: EvalHeaderProps) {
    super(props);
    this._languages = [];
    for (const key in Constants.Languages) {
      this._languages.push({ payload: key, text: key });
    }
  }

  shouldComponentUpdate(nextProps: EvalHeaderProps, _: {}): boolean {
    return (
      this.props.open !== nextProps.open ||
      this.props.languageLink.value !== nextProps.languageLink.value ||
      this.props.expressionLink.value !== nextProps.expressionLink.value
    );
  }

  render(): React.Element {
    const {languageLink, expressionLink, selectionLink, onEvaluate, name} = this.props;

    return (
      <div style={styles.root}>

        <Toolbar style={styles.toolbar}
                 showMenuIconButton={false}>

          <ASDropdownMenu
            menuItems={this._languages}
            valueLink={languageLink}
            underlineStyle={styles.dropdownUnderline} />

          <FlatButton label={buttonText}
                      style={styles.evalButton}
                      onClick={() => onEvaluate()} />

        </Toolbar>

        <ASControlledCodeField
          ref={elem => this._editor = elem}
          style={styles.codeField}
          name={name}
          text={expressionLink}
          language={languageLink.value}
          onKeyDown={e => this._onKeyDown(e)}
        />

      </div>
    );
  }

  _onKeyDown(e: SyntheticKeyboardEvent) {
    Shortcuts.try(e, 'header'); // will kill the event if a shortcut matches.
  }
};

const buttonText = 'Evaluate';

const styles = {
  root: {
    height: '100%',
    marginLeft: '6px'
  },
  codeField: {
    height: '100%'
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
