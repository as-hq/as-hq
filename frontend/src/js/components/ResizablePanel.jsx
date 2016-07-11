/* @flow */

import {logDebug} from '../AS/Logger';

import React from 'react';
import ReactDOM from 'react-dom';
import {Styles} from 'material-ui';

type ResizablePanelConfigType = {
  separatorSize: number;
  minSize: number;
  maxSize: number;
  defaultSize: string;
};

const config: ResizablePanelConfigType = {
  separatorSize: 6,
  minSize: 45,
  maxSize: 80,
  defaultSize: '70%'
};

type ResizablePanelDefaultProps = {
  sidebarVisible: boolean;
  side: string;
};

type ResizablePanelProps = {
  content: React.Element;
  sidebar: React.Element;
  sidebarVisible: boolean;
  side: string;
};

type ResizablePanelState = {
  dragging: boolean;
  contentSize: string;
  lastContentSize: string;
};

export default class ResizablePanel extends React.Component
{
  static defaultProps: ResizablePanelDefaultProps = {
    sidebarVisible: false,
    side: 'right'
  };
  props: ResizablePanelProps;
  state: ResizablePanelState;


  //----------------------------------------------------------------------------
  // Props and state methods

  rootPosition: {
    top: number;
    left: number;
    width: number;
    height: number;
  };

  constructor(props: ResizablePanelDefaultProps) {
    super(props);

    // Position and size info about the entire resizable pane. 
    // Top from window, left from window, and width/height, which can be
    // determined after the initial render
    this.rootPosition = {
      top:    0,
      left:   0,
      width:  0,
      height: 0
    };

    this.state = {
      dragging: false,
      // The relevant dimension of the content. 
      // For example, if side = 'right', this would be the width of the content.
      // If the side was 'top', it would be the height of the content, 
      // which would be below the sidebar.
      // The units are in percent relative to the whole resizable panel
      contentSize: this.props.sidebarVisible ? config.defaultSize : '100%',
      // Keeps track of the content size during closes of the sidebar, so that 
      // it "remembers" its last size upon reopening
      lastContentSize: config.defaultSize
    };
  }

  //----------------------------------------------------------------------------
  // Component lifecycle methods

  /*
    Keep track of mousemove and mouseup for the separator between the content 
    pane and the side pane. This is for being able to track dragging motions
    Also initialize the rootPosition, which won't change unless the parent 
    itself rerenders
  */
  componentDidMount() {
    // $FlowFixMe what's going on here?
    document.addEventListener('mousemove', this._onMouseMove.bind(this));
    // $FlowFixMe what's going on here?
    document.addEventListener('mouseup', this._onMouseUp.bind(this));

    const root = ReactDOM.findDOMNode(this.refs.root);
    const doc = document.documentElement;
    const rect = root.getBoundingClientRect();
    const computedStyle = window.getComputedStyle(root, null);

    // TODO(joel) - is this necessary? are width and height what we want here?
    this.rootPosition = {
      // use the bounding position + page scroll - page border
      top:    rect.top + window.pageYOffset - doc.clientTop,
      left:   rect.left + window.pageXOffset - doc.clientLeft,

      // use computed size, minus "px"
      width:  +computedStyle.getPropertyValue('width').slice(0, -2),
      height: +computedStyle.getPropertyValue('height').slice(0, -2),
    };
  }

  componentWillUnmount() {
    document.removeEventListener('mousemove', this._onMouseMove.bind(this));
    document.removeEventListener('mouseup', this._onMouseUp.bind(this));
  }

  componentWillReceiveProps(nextProps: ResizablePanelDefaultProps) {
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
    // When sidebar about to be visible (content size is currently full), 
    // change the content size to what it previously was
    if (nextProps.sidebarVisible) {
      this.setState({
        contentSize: this.state.lastContentSize
      });
    }
  }

  //----------------------------------------------------------------------------
  // Mouse events

  stopEvent(e: SyntheticMouseEvent) {
    e.stopPropagation();
    e.preventDefault();
  }

  _onMouseDown(e: SyntheticMouseEvent) {
    // Do not enable dragging if the sidebar is tucked away (not visible). 
    // Else, set dragging = true
    if (this.props.sidebarVisible) {
      this.setState({dragging: true});
    }
  }

  _onMouseUp(e: SyntheticMouseEvent) {
    this.setState({dragging: false});
  }

  _onMouseMove(e: SyntheticMouseEvent) {
    if (this.state.dragging) {
      let contentSize  = 0;
      const rootWidth  = this.rootPosition.width;
      const rootHeight = this.rootPosition.height;
      if (this.props.side === 'right') {
        contentSize = (e.pageX - this.rootPosition.left)/rootWidth;
      } else if (this.props.side === 'left') {
        contentSize = (this.rootPosition.left + rootWidth - e.pageX)/rootWidth;
      } else if (this.props.side === 'top') {
        contentSize = (this.rootPosition.top + rootHeight - e.pageY)/rootHeight;
      } else if (this.props.side === 'bottom') {
        contentSize = (e.pageY - this.rootPosition.top)/rootHeight;
      }
      contentSize *= 100;
      if (contentSize > config.minSize && contentSize < config.maxSize) {
        this.setState({contentSize: (contentSize + '%')});
      }
      this.stopEvent(e);
    }
  }

  //----------------------------------------------------------------------------
  // Styling and rendering

  isHorizontal(): boolean {
    return this.props.side === 'left' || this.props.side === 'right';
  }

  getStyles(): any {
    const contentSize = parseFloat(this.state.contentSize);
    const sepSize     = config.separatorSize;
    // The sidebar's size is the whole pane's size (percent) minus the 
    // content size (percent) minus the separator size (pixels)
    // Be a little careful that this doesn't turn out negative.
    const sidebarSize = `calc(${100 - contentSize}% - ${sepSize}px)`;

    const separatorStyle = this.isHorizontal() ? {
      display: 'inline-block',
      verticalAlign: 'top',
      backgroundColor: 'black',
      width: sepSize,
      height: '100%',
      cursor: 'col-resize'
    } : {
      backgroundColor: 'black',
      height: sepSize,
      width: '100%',
      cursor: 'row-resize'
    }

    const contentStyle = this.isHorizontal() ? {
      display: 'inline-block',
      verticalAlign: 'top',
      width: this.state.contentSize,
      // Don't make the content larger than contentSize dictates
      overflow: 'hidden', 
      height: '100%',
      // Always show correct cursor everywhere while dragging
      cursor: this.state.dragging ? 'col-resize' : 'auto'
    } : {
      height: this.state.contentSize,
      width: '100%',
      overflow: 'hidden',
      cursor: this.state.dragging ? 'row-resize' : 'auto'
    }

    const sidebarStyle = this.isHorizontal() ? {
      display: 'inline-block',
      verticalAlign: 'top',
      width: sidebarSize,
      height: '100%',
      // Don't show parts of the sidebar that are "outside the bounds" 
      // specified by the content
      overflow: 'hidden',
      backgroundColor: Styles.Colors.grey800,
      cursor: this.state.dragging ? 'col-resize' : 'auto'
    } : {
      height: sidebarSize,
      width: '100%',
      // Don't show parts of the sidebar that are "outside the bounds" 
      // specified by the content
      overflow: 'hidden', 
      backgroundColor: Styles.Colors.grey800,
      cursor: this.state.dragging ? 'row-resize' : 'auto'
    }

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
  }

  render(): React.Element {
    const isTopLeft = this.props.side === 'top' || this.props.side === 'left';
    const styles = this.getStyles();
    const content =
      <div ref="content" style={styles.content}>
        {this.props.content}
      </div>;
    const sidebar =
      <div style={styles.sidebar}>
        {this.props.sidebar}
      </div>;
    // For rendering purposes, the children of the root need to be ordered.
    // For top, the sidebar first, then separator, then content. Etc.
    const firstDiv  = isTopLeft ? sidebar : content;
    const secondDiv = isTopLeft ? content : sidebar;
    const separator = this.props.sidebarVisible ? 
      <div style={styles.separator} 
             onMouseDown={this._onMouseDown.bind(this)} /> : null;
    return (
      <div ref="root" style={{...styles.root}}>
        {firstDiv}
        {separator}
        {secondDiv}
      </div>
    );
  }
}

ResizablePanel.propTypes = {
  // component with the actual content; not the sidebar
  content: React.PropTypes.node.isRequired,
  // sidebar content to render
  sidebar: React.PropTypes.node.isRequired,
  // boolean if sidebar is visible
  sidebarVisible: React.PropTypes.bool,
  // which side the sidebar pops open from
  side: React.PropTypes.string
};
