import React from 'react';
import {Tab,Tabs} from 'react-draggable-tab'

export default React.createClass({
  getInitialState(){
    return {
      tabs:[
            (<Tab key={'tab0'} title={'Sheet-1'} >
              <div>
              </div>
            </Tab>)
          ],
        selectedTab: 'tab1'
      };
  },

  handleTabSelect(e, key, currentTabs) {
      console.log('handleTabSelect key:', key);
      this.setState({selectedTab: key, tabs: currentTabs});
  },

  handleTabClose(e, key, currentTabs) {
      console.log('tabClosed key:', key);
      this.setState({tabs: currentTabs});
  },

  handleTabPositionChange(e, key, currentTabs) {
      console.log('tabPositionChanged key:', key);
      this.setState({tabs: currentTabs});
  },

  handleTabAddButtonClick(e, currentTabs) {
      // key must be unique
      const key = 'newTab_' + Date.now();
      let newTab = (<Tab key={key} title='untitled'>
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
