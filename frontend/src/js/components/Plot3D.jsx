import React from 'react';

export default class Plot3D extends React.Component {
  static defaultProps = {};
  props: {};
  state: {};


  constructor(props) {
    super(props);
  }

  componentDidMount() {
    var selected=surfaces[0];

    var ul=d3.select('#plot3d')
             .append('ul');
    var svg=d3.select('#plot3d')
            .append('svg')
              .attr('height',height)
              .attr('width',width);

    var group = svg.append("g");

    var md=group.data([surfaces[0].data])
      .surface3D(width,height)
        .surfaceHeight(function(d){ 
          return d;
        }).surfaceColor(function(d){
          var c=d3.hsl((d+100), 0.6, 0.5).rgb();
          return "rgb("+parseInt(c.r)+","+parseInt(c.g)+","+parseInt(c.b)+")";
        });

    ul.selectAll('li')
      .data(surfaces)
        .enter().append('li')
          .html(function(d){
            return d.name
          }).on('mousedown',function(){
            md.data([d3.select(this).datum().data]).surface3D()
              .transition().duration(500)
              .surfaceHeight(function(d){
                return d;
              }).surfaceColor(function(d){
                var c=d3.hsl((d+100), 0.6, 0.5).rgb();
                return "rgb("+parseInt(c.r)+","+parseInt(c.g)+","+parseInt(c.b)+")";
              });
          });

    svg.on("mousedown",function(){
      drag=[d3.mouse(this),yaw,pitch];
    }).on("mouseup",function(){
      drag=false;
    }).on("mousemove",function(){
      if(drag){            
        var mouse=d3.mouse(this);
        yaw=drag[1]-(mouse[0]-drag[0][0])/50;
        pitch=drag[2]+(mouse[1]-drag[0][1])/50;
        pitch=Math.max(-Math.PI/2,Math.min(Math.PI/2,pitch));
        md.turntable(yaw,pitch);
      }
    });
 
  }

  componentWillUnmount() {
  }

 
  render() {
   return <div style={{height: 300, width: 300}} id="plot3d" />;
  }

}




