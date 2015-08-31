import React, {PropTypes} from 'react';

import {Styles} from 'material-ui';
import {HomeTab, ChartsTab, DataTab} from './tabs/index.jsx';
const ThemeManager = new Styles.ThemeManager();

export default React.createClass({
  componentWillMount() {
    ThemeManager.setTheme(ThemeManager.types.DARK);
  },

  propTypes: {
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

    let ribbonTabs = {
      Home: <HomeTab />,
      Code: <div />,
      Data: <DataTab />,
      Charts: <ChartsTab />,
      Team: <div />,
      Layout: <div />
    };

    return (
        <div>
          {ribbonTabs[this.props.activeTab]}
        </div>
    );
  },

  // _onCheckChange(e) {
  // },
});
