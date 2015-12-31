import {logDebug} from '../AS/Logger';

import React from 'react';
import ReactDOM from 'react-dom';
import {Styles} from 'material-ui';
import $ from 'jquery';

const config = {
  separatorSize: 6,
  minSize: 45,
  maxSize: 80,
  defaultSize: '70%'
};


export default React.createClass({

  /**********************************************************************************************************************************/
  // Props and state methods

  getInitialState() {
    return {
      dragging: false,
      // The relevant dimension of the content. For example, if side = 'right', this would be the width of the content. 
      // If the side was 'top', it would be the height of the content, which would be below the sidebar. 
      // The units are in percent relative to the whole resizable panel
      contentSize: this.props.sidebarVisible ? config.defaultSize : '100%',
      // Keeps track of the content size during closes of the sidebar, so that it "remembers" its last size upon reopening
      lastContentSize: config.defaultSize
    };
  },

  propTypes: {
    // component with the actual content; not the sidebar
    content: React.PropTypes.node.isRequired,
    // sidebar content to render 
    sidebar: React.PropTypes.node.isRequired,
    // boolean if sidebar is visible 
    sidebarVisible: React.PropTypes.bool,
    // which side the sidebar pops open from
    side: React.PropTypes.string
  },

  getDefaultProps() {
    return {
      sidebarVisible: false,
      side: 'right'
    };
  },

  // Position and size info about the entire resizable pane. Top from window, left from window, and width/height, which can be
  // determined after the initial render
  rootPosition: {
    top:    0,
    left:   0,
    width:  0,
    height: 0
  },

  /**********************************************************************************************************************************/
  // Component lifecycle methods

  // Keep track of mousemove and mouseup for the separator between the content pane and the side pane
  // This is for being able to track dragging motions
  // Also initialize the rootPosition, which won't change unless the parent itself rerenders
  componentDidMount() {
    document.addEventListener('mousemove', this._onMouseMove);
    document.addEventListener('mouseup', this._onMouseUp);
    let root = ReactDOM.findDOMNode(this.refs.root);
    this.rootPosition = {
      top:    $(root).offset().top,
      left:   $(root).offset().left,
      width:  $(root).width(),
      height: $(root).height()
    }
    console.log($(root).width(), $(root).offset().left, $(window).width());
  },

  componentWillUnmount() {
    document.removeEventListener('mousemove', this._onMouseMove);
    document.removeEventListener('mouseup', this._onMouseUp);
  },

  componentWillReceiveProps(nextProps) {
    // If the sidebar visibility prop hasn't changed, don't do anything here 
    if (this.props.sidebarVisible === nextProps.sidebarVisible) {
      return;
    }
    // Change the content size to full once sidebar is tucked away 
    // Make sure that we keep this size for future use upon reopening
    if (!nextProps.sidebarVisible) {
      this.setState({
        lastContentSize: this.state.contentSize,
        contentSize: '100%'
      });                                                                                                                                                      
    }
    // When sidebar about to be visible (content size is currently full), change the content size to what it previously was
    if (nextProps.sidebarVisible && this.state.leftWidth === '100%') {
      this.setState({
        contentSize: this.state.lastContentSize
      });
    }
  },

  /**********************************************************************************************************************************/
  // Mouse events

  stopEvent(e) {
    e.stopPropagation();
    e.preventDefault();
  },

  _onMouseDown(e) {
    // Do not enable dragging if the sidebar is tucked away (not visible). Else, set dragging = true
    if (this.props.sidebarVisible) {
      this.setState({dragging: true});
    }
    this.stopEvent(e);
  },

  _onMouseUp(e) {
    this.setState({dragging: false});
    this.stopEvent(e);
  },

  _onMouseMove(e) {
    if (this.state.dragging) {
      let contentSize = 0;
      if (this.props.side === 'right') {
        contentSize = (e.pageX - this.rootPosition.left)/this.rootPosition.width * 100;
      } else if (this.props.side === 'left') {
        contentSize = (this.rootPosition.left + this.rootPosition.width - e.pageX)/this.rootPosition.width * 100;
      } else if (this.props.side === 'top') {
        contentSize = (this.rootPosition.top + this.rootPosition.height - e.pageY)/this.rootPosition.height * 100;
      } else if (this.props.side === 'bottom') {
        contentSize = (e.pageY - this.rootPosition.top)/this.rootPosition.height * 100;
      }
      if (contentSize > config.minSize && contentSize < config.maxSize) {
        this.setState({contentSize: (contentSize + '%')});
      }
    }
    this.stopEvent(e);
  },

  /**********************************************************************************************************************************/
  // Styling and rendering

  horizontal() {
    return this.props.side === 'left' || this.props.side === 'right';
  },

  getStyles() {
    // The sidebar's size is the whole pane's size (percent) minus the content size (percent) minus the separator size (pixels)
    let sidebarSize = `calc(${100 - parseFloat(this.state.contentSize)}% - ${config.separatorSize}px)`;

    let separatorStyle = this.horizontal() ? {
      display: 'inline-block',
      verticalAlign: 'top',
      backgroundColor: 'black',
      width: config.separatorSize,
      height: '100%',
      cursor: 'col-resize'
    } : {
      backgroundColor: 'black',
      height: config.separatorSize,
      width: '100%',
      cursor: 'row-resize'
    };

    let contentStyle = this.horizontal() ? {
      display: 'inline-block',
      verticalAlign: 'top',
      width: this.state.contentSize,
      overflow: 'hidden', // don't make the content larger than contentSize dictates
      height: '100%',
      cursor: this.state.dragging ? 'col-resize' : 'auto' // always show correct cursor everywhere while dragging
    } : {
      height: this.state.contentSize,
      width: '100%',
      overflow: 'hidden',
      cursor: this.state.dragging ? 'row-resize' : 'auto' 
    };

    let sidebarStyle = this.horizontal() ? {
      display: 'inline-block',
      verticalAlign: 'top',
      width: sidebarSize,
      height: '100%',
      overflow: 'hidden', // don't show parts of the sidebar that are "outside the bounds" specified by the content
      backgroundColor: Styles.Colors.grey700,
      cursor: this.state.dragging ? 'col-resize' : 'auto' 
    } : {
      height: sidebarSize,
      width: '100%',
      overflow: 'hidden', // don't show parts of the sidebar that are "outside the bounds" specified by the content
      backgroundColor: Styles.Colors.grey700,
      cursor: this.state.dragging ? 'row-resize' : 'auto' 
    };

    return {
      root: {
        width: '100%',
        height: '100%',
        overflow: 'hidden' // Don't overflow original size
      },
      separator: separatorStyle,
      content: contentStyle,
      sidebar: sidebarStyle
    };
  },

  render() {
    let styles = this.getStyles();
    let content = 
      <div ref="content" style={styles.content}>
        {this.props.content}
      </div>;
    let sidebar = 
      <div style={styles.sidebar}>
        {this.props.sidebar}
      </div>;
    // For rendering purposes, the children of the root need to be ordered. 
    // For top, the sidebar first, then separator, then content. Etc.
    let firstDiv =  (this.props.side === 'top' || this.props.side === 'left') ? sidebar : content;
    let secondDiv = (this.props.side === 'top' || this.props.side === 'left') ? content : sidebar;
    return (
      <div ref="root" style={styles.root}>
        {firstDiv}
        <div style={styles.separator} onMouseDown={this._onMouseDown} />
        {secondDiv}
      </div>
    );
  }
});
