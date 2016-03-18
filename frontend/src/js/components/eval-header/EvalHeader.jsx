/* @flow */

import React from 'react';

import Constants from '../../Constants.js';

import HeaderActions from '../../actions/ASHeaderActionCreators';
import { actions as Shortcuts } from '../../AS/Shortcuts';

import KeyUtils from '../../AS/utils/Key';
import U from '../../AS/Util';

// $FlowFixMe
import {Toolbar, Styles, FlatButton} from 'material-ui';
import ASControlledCodeField from '../basic-controls/ASControlledCodeField.jsx';
import ASDropdownMenu from '../basic-controls/ASDropdownMenu.jsx';
// $FlowFixMe
let NavigationClose = require('material-ui/lib/svg-icons/navigation/close');
// $FlowFixMe: react-tooltip not actually defined
import Tooltip from 'react-tooltip';

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
  onEvaluate: () => void;
  onMouseEnter: () => void;
}

type LanguageItem = {
  payload: any;
  text: any;
}

class EvalHeader extends React.Component {
  static defaultProps = {};
  props: EvalHeaderProps;
  state: {};

  _languages: Array<LanguageItem>;
  _editor: any;

  constructor(props: EvalHeaderProps) {
    super(props);
    this._languages = [];
    for (const key in Constants.HeaderLanguages) {
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
    const {languageLink, expressionLink, onEvaluate} = this.props;

    return (
      <div style={styles.root}>

        <Toolbar style={styles.toolbar}
                 showMenuIconButton={false}>

          <ASDropdownMenu
            menuItems={this._languages}
            valueLink={languageLink}
            underlineStyle={styles.dropdownUnderline} />

          <span>
            <FlatButton label={buttonText}
                        style={styles.evalButton}
                        onClick={() => onEvaluate()}
                        data-tip={U.Browser.metaKeyName() + '+S'} />

            <Tooltip
              delayHide={50}
              delayShow={300}
              place="bottom"
              type="info"
              effect="solid"
              offset={{'top': 10, 'left': 0}} />
          </span>

        </Toolbar>

        <ASControlledCodeField
          ref={elem => this._editor = elem}
          style={styles.codeField}
          text={expressionLink}
          language={languageLink.value}
          onKeyDown={e => this._onKeyDown(e)}
          onMouseEnter={() => this.props.onMouseEnter()}
        />

      </div>
    );
  }

  _onKeyDown(e: SyntheticKeyboardEvent) {
    Shortcuts.try(e, 'header'); // will kill the event if a shortcut matches.
  }
};

const buttonText = 'Evaluate';

// #needsrefactor the correct way to do this is flex; this overconstrains the toolbar height.
const toolbarHeight = '56px';

const styles = {
  root: {
    height: '100%',
    marginLeft: '6px'
  },
  codeField: {
    height: `calc(100% - ${toolbarHeight})`
  },
  toolbar: {
    backgroundColor: Styles.Colors.grey900,
    height: toolbarHeight
  },
  dropdownUnderline: {
    display: 'none'
  },
  evalButton: {
    fontFamily: '"Lucida Console", Monaco, monospace'
  }
};

export default EvalHeader;
