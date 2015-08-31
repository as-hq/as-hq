import React, {PropTypes} from 'react';

import {HomeTab, ChartsTab, DataTab} from './tabs/index.jsx';

import ASEvaluationPane from './ASEvaluationPane.jsx';
import ASNavBar from './ASNavbar.jsx';

import {AppCanvas, LeftNav, Paper, Styles} from 'material-ui';
let {Colors} = Styles;

const ThemeManager = new Styles.ThemeManager();

export default React.createClass({
  componentWillMount() {
    ThemeManager.setTheme(ThemeManager.types.DARK);
  },

  propTypes: {
  },

  getInitialState() {
    return {
      activeDocumentTab: 'test',
      activeRibbonTab: 'Home'
    }
  },

  getDefaultProps() {
    return {
      tasks: []
    }
  },

  childContextTypes: {
    muiTheme: React.PropTypes.object
  },

  getChildContext() {
    return {
      muiTheme: ThemeManager.getCurrentTheme()
    };
  },

  render() {
    let leftNavMenuItems = [
      { route: 'all-files', text: 'All files' },
      { route: 'logout', text: 'Log out' }
    ];
    let ribbonHeight = '110px';

    let ribbonTabs = {
      Home: <HomeTab /> /*(
        <div>
          <ASHorizontalDropdownButton
            iconClassName="muidocs-icon-action-home"
            label="Home"
            menuItems={[ 'item1', 'item2' ]}
          />
          <ASBlockDropdownButton
            width={60}
            height={88}
            iconClassName="muidocs-icon-action-home"
            label="Home"
            menuItems={[ 'item1', 'item2' ]}
          />
          <ASDropdown
            menuItems={[
              { payload: '1', text: 'test1' },
              { payload: '2', text: 'test2' }
            ]}
          />
          <ASCheckedButton
            iconClassName="muidocs-icon-action-home"
            onCheckChange={this._onCheckChange}
          />
          <ASRibbonDivider />
        </div>
      )*/,
      Code: <div />,
      Data: <DataTab />,
      Charts: <ChartsTab />,
      Team: <div />,
      Layout: <div />
    };

    return (
      <div className="full">
        <LeftNav
          ref="leftNav"
          menuItems={leftNavMenuItems}
          docked={false}
        />
        <ASNavBar
          onDocumentTabChange={this._onDocumentTabChange}
          onRibbonTabChange={this._onRibbonTabChange}
          onAlphaButtonTap={this._onAlphaButtonTap}
        />
        <div>
          {ribbonTabs[this.state.activeRibbonTab]}
        </div>
        <ASEvaluationPane behavior="default" />
      </div>
    );
  },

  _onCheckChange(e) {
  },

  _onDocumentTabChange(tabKey) {
    this.setState({ activeDocumentTab: tabKey });
  },

  _onRibbonTabChange(tabTitle) {
    this.setState({ activeRibbonTab: tabTitle });
  },

  _onAlphaButtonTap() {
    this.refs.leftNav.toggle();
  }
});
