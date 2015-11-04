import {logDebug} from '../AS/Logger';

import React from 'react';
import Tab from './tab-components/Tab.js'
import Tabs from './tab-components/Tabs.js'


export default React.createClass({
  getInitialState(){
    return {
      tabs:[
            (<Tab key={'tab0'} title={'Sheet-1'} >
              <div>
              </div>
            </Tab>)
          ],
        selectedTab: 'tab0',
        numTabs: 1
      };
  },

  handleTabSelect(e, key, currentTabs) {
      logDebug('handleTabSelect key:', key);
      this.setState({selectedTab: key, tabs: currentTabs});
  },

  handleTabClose(e, key, currentTabs) {
      logDebug('tabClosed key:', key);
      this.setState({tabs: currentTabs});
  },

  handleTabPositionChange(e, key, currentTabs) {
      logDebug('tabPositionChanged key:', key);
      this.setState({tabs: currentTabs});
  },

  handleTabAddButtonClick(e, currentTabs) {
      // key must be unique
      let numTabs = this.state.numTabs;
      const key = 'sheet' + (numTabs+1);
      const title = 'Sheet-'+ (numTabs+1);
      let newTab = (<Tab key={key} title={title}>
                      <div>
                      </div>
                    </Tab>);
      let newTabs = currentTabs.concat([newTab]);

      this.setState({
        tabs: newTabs,
        selectedTab: key
      });
  },

  render() {
    let tabsClassNames = {
      tabBar: 'myTabBar',
      tabBarAfter: 'myTabBarAfter',
      tab:      'myTab',
      tabTitle: 'myTabTitle',
      tabCloseIcon: 'tabCloseIcon',
      tabBefore: 'myTabBefore',
      tabAfter: 'myTabAfter'
    };

    let tabsStyles = {
      tabBar: {},
      tab:{},
      tabTitle: {},
      tabCloseIcon: {},
      tabBefore: {},
      tabAfter: {}
    };
      return (
       <div><Tabs
        selectedTab={this.state.selectedTab}
          onTabSelect={this.handleTabSelect}
          onTabClose={this.handleTabClose}
          onTabAddButtonClick={this.handleTabAddButtonClick}
          onTabPositionChange={this.handleTabPositionChange}
          tabs={this.state.tabs}
          /></div>
      );
    }



})
