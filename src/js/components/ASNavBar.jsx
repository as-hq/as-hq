import React, {PropTypes} from 'react';
import {AppBar, Tabs, Tab, Styles} from 'material-ui';

let {Colors} = Styles;

export default React.createClass({
  propTypes: {
    onDocumentTabChange: React.PropTypes.func.isRequired,
    onRibbonTabChange: React.PropTypes.func.isRequired,
    onAlphaButtonTap: React.PropTypes.func.isRequired
  },

  getInitialState() {
    return {
    }
  },

  render() {
    let backgroundColor = Colors.grey900;
    let tabs = ['Home', 'Code', 'Data', 'Charts', 'Team', 'Layout'];
    let tabHeight = 40;
    let appBarHeight = 64;
    let leftJustify = 65;
    let topMargin = appBarHeight - tabHeight;

    return (
      <div>
        <AppBar
          style={{
            backgroundColor: backgroundColor,
            height: appBarHeight + 'px'
          }}
          onLeftIconButtonTouchTap={this._onAlphaButtonTap}
        />
        <Tabs
          style={{
            position: 'absolute',
            left: leftJustify,
            top: topMargin + 'px',
            height: tabHeight + 'px'
          }}
          tabItemContainerStyle={{
            backgroundColor: backgroundColor,
            height: tabHeight + 'px'
          }}
          tabWidth={120}
        >
          { tabs.map((tabTitle) =>
            <Tab
              style={{
                height: tabHeight + 'px'
              }}
              label={tabTitle}
              onActive={this._onActive(tabTitle)}
            />
          ) }
        </Tabs>
      </div>
    );
  },

  _onActive(tabTitle) {
    return () => {
      this.props.onRibbonTabChange(tabTitle);
    };
  },

  _onAlphaButtonTap() {
    this.props.onAlphaButtonTap();
  }
});
