import React, {PropTypes} from 'react';
import ASEvaluationPane from './ASEvaluationPane.jsx';
import ASNavBar from './ASNavbar.jsx';
import ASButton from './ASButton.jsx';
import ASCheckedButton from './ASCheckedButton.jsx';
import ASDropdown from './ASDropdown.jsx';
import ASBlockDropdownButton from './ASBlockDropdownButton.jsx';
import ASHorizontalDropdownButton from './ASHorizontalDropdownButton.jsx';
import ASRibbonDivider from './ASRibbonDivider.jsx';
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
      Home: (
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
      ),
      Code: <div />,
      Data: <div />,
      Charts: <div />,
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
        <Paper style={{ backgroundColor: Colors.grey800 }}>
          <Paper style={{ backgroundColor: Colors.grey800, height: ribbonHeight }}>
            {ribbonTabs[this.state.activeRibbonTab]}
          </Paper>
          <div
            style={{
              color: '#ffffff',
              textTransform: 'uppercase',
              textShadow: '-1px -1px 1px #000000',
              fontWeight: 'normal',
              paddingTop: '3px',
              paddingBottom: '3px'
            }}
          >
            Test
          </div>
        </Paper>
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
