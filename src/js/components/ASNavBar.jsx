import React, {PropTypes} from 'react';
import {AppBar, Tabs, Tab, Styles} from 'material-ui';
import DraggableTabs from 'react-draggable-tab';

let {Colors} = Styles;
let DocumentTabs = DraggableTabs.Tabs;
let DocumentTab = DraggableTabs.Tab;

export default React.createClass({
  propTypes: {
    onDocumentTabChange: React.PropTypes.func.isRequired,
    onRibbonTabChange: React.PropTypes.func.isRequired,
    onAlphaButtonTap: React.PropTypes.func.isRequired
  },

  getInitialState() {
    return {
      tabs: [
        <DocumentTab
          key="document1"
          title="Document 1"
        >
          <div>
          </div>
        </DocumentTab>
      ],
      selectedTab: 'document1'
    }
  },

  render() {
    let backgroundColor = Colors.grey900;
    let tabs = ['Home', 'Code', 'Data', 'Charts', 'Team', 'Layout'];
    let tabHeight = 40;
    let appBarHeight = 75;
    let leftJustify = '65px';
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
        <div style={{
          position: 'absolute',
          left: leftJustify,
          top: '5px',
          width: '100%'
        }}>
          <DocumentTabs
            selectedTab={this.state.selectedTab}
            onTabSelect={this._onDocumentTabSelect}
            onTabClose={this._onDocumentTabClose}
            onTabPositionChange={this._onDocumentTabPositionChange}
            tabs={this.state.tabs}
            tabAddButton={
              <div style={{ display: 'none' }}>
              </div>
            }
          />
        </div>
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

  _onDocumentTabSelect(e, key, currentTabs) {
    if (key === this.state.selectedTab) return;

    this.setState({
      selectedTab: key,
      tabs: currentTabs
    });

    this.props.onDocumentTabChange(key);
  },

  _onDocumentTabClose(e, key, currentTabs) {
    this.setState({
      tabs: currentTabs
    });
  },

  _onDocumentTabPositionChange(e, key, currentTabs) {
    this.setState({
      tabs: currentTabs
    });
  },

  _onActive(tabTitle) {
    return () => {
      this.props.onRibbonTabChange(tabTitle);
    };
  },

  _onAlphaButtonTap() {
    this.props.onAlphaButtonTap();
  }

/*
  _onDocumentTabAddButtonClick(e, currentTabs) {
    let key = 'newTab_' + Date.now();
    let newTab = (
      <DocumentTab
        key={key}
        title="untitled"
      >
        <div>
        </div>
      </DocumentTab>
    );
    let newTabs = currentTabs.concat([newTab]);

    this.setState({
      tabs: newTabs,
      selectedTab: key
    });

    this.props.onDocumentTabChange(key);
  }*/
});
