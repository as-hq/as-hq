import React from 'react/addons';
const update = React.addons.update;
import {Styles} from 'material-ui'

const config = {
  splitBarWidth:"6px",
  minLeft:30,
  maxLeft:90,
  defaultLeft:"65%"
};


export default React.createClass({

  getStyles(){
    return {
      root: {
        width: '100%',
        height: '100%',
        overflow: 'hidden' /* The sidebar can't go past the left comp, and vice versa */
      },
      splitBar: {
        backgroundColor:'black',
        height:'100%',
        float: 'left',
        width: config.splitBarWidth,
        cursor: 'col-resize'
      }

    };
  },

  getInitialState() {
    return {
      dragging:false,
      leftWidth:'100%',
      lastLeftWidth:config.defaultLeft
    };
  },

  propTypes: {
    /* component to the left of the sidebar */
    leftComp: React.PropTypes.node.isRequired,
    /* sidebar content to render */
    sidebar: React.PropTypes.node.isRequired,
    /* boolean if sidebar should be docked */
    docked: React.PropTypes.bool,
  },

  getDefaultProps(){
    return {
      docked: false
    };
  },

  componentDidMount() {
    document.addEventListener('mousemove', this.onMouseMove);
    document.addEventListener('mouseup', this.onMouseUp);
  },

  componentWillUnmount() {
    document.removeEventListener('mousemove', this.onMouseMove);
    document.removeEventListener('mouseup', this.onMouseUp);
  },

  componentWillReceiveProps(nextProps) {
    /* If the docked prop hasn't changed, don't do anything here */
    if (this.props.docked === nextProps.docked)
      return;
    /* Change the state (width of leftComp) to full once sidebar is tucked away */
    if (this.props.docked){
      this.setState({
        lastLeftWidth:this.state.leftWidth,
        leftWidth:"100%"
      });
    }
    /* When sidebar about to be visible (left width is full), change left width */
    if (!this.props.docked && this.state.leftWidth === "100%"){
      console.log("received props");
      this.setState({
        leftWidth:this.state.lastLeftWidth
      });
    }
  },

  onMouseDown(e) {
    if (e.button !== 0) return;
    /* Do not enable dragging if the sidebar is tucked away */
    if (!this.props.docked) return;
    this.setState({
      dragging: true
    });
    e.stopPropagation();
    e.preventDefault();
  },

  onMouseUp(e) {
    this.setState({dragging: false});
    e.stopPropagation();
    e.preventDefault();
  },

  onMouseMove(e) {
    if (!this.state.dragging) return;
    let parentWidth = parseFloat(this.getStyles().root.width)/100 * $(window).width();
    let x = (e.pageX - $(React.findDOMNode(this.refs.leftComp)).offset().left)/parentWidth*100;
    if (x > config.minLeft && x < config.maxLeft)
      this.setState({leftWidth:(x+"%")});
    console.log("new x " + x);
    e.stopPropagation();
    e.preventDefault();
  },

  render() {
    let splitBarStyle = this.getStyles().splitBar,
        rootProps = {style: this.getStyles().root},
        sidebarWidth = (100 - parseFloat(this.state.leftWidth))+ "%";

    let sidebarDivStyle = {
      width:sidebarWidth,
      height:"100%",
      backgroundColor:Styles.Colors.grey700,
      marginLeft:this.state.leftWidth
    };

    let leftDivStyle = {
      float:'left',
      width:this.state.leftWidth,
      height:"100%"
    };


    return (
      <div {...rootProps}>
        <div ref="leftComp" style={leftDivStyle}>
          {this.props.leftComp}
        </div>
        <div style={sidebarDivStyle}>
             <div style={splitBarStyle} onMouseDown={this.onMouseDown} onMouseMove={this.onMouseMove} onMouseUp={this.onMouseUp} />
             {this.props.sidebar}
        </div>
      </div>
    );
  }
});
