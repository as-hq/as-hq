export default {
  defaultCellRenderer: {
    paint: function(gc, x, y, width, height, isLink) {
      isLink = isLink || false;
      var colHEdgeOffset = this.config.properties.cellPadding,
          halignOffset = 0,
          valignOffset = this.config.voffset,
          halign = this.config.halign,
          isColumnHovered = this.config.isColumnHovered,
          isRowHovered = this.config.isRowHovered,
          val = this.config.value,
          paintBorders = this.config.paintBorders,
          borderConfig = this.config.borderConfig;

      var leftIcon, rightIcon, centerIcon, ixoffset, iyoffset;

      if (val && val.constructor === Array) {
          leftIcon = val[0];
          rightIcon = val[2];
          val = val[1];
          if (typeof val === 'object') { // must be an image
              centerIcon = val;
              val = null;
          }
          if (leftIcon && leftIcon.nodeName !== 'IMG') {
              leftIcon = null;
          }
          if (rightIcon && rightIcon.nodeName !== 'IMG') {
              rightIcon = null;
          }
          if (centerIcon && centerIcon.nodeName !== 'IMG') {
              centerIcon = null;
          }
      }

      if (gc.font !== this.config.font) {
          gc.font = this.config.font;
      }
      if (gc.textAlign !== 'left') {
          gc.textAlign = 'left';
      }
      if (gc.textBaseline !== 'middle') {
          gc.textBaseline = 'middle';
      }

      var fontMetrics = this.config.getTextHeight(this.config.font);
      var textWidth = this.config.getTextWidth(gc, val);


      //we must set this in order to compute the minimum width
      //for column autosizing purposes
      // this.config.minWidth = textWidth + (2 * colHEdgeOffset);
      this.config.minWidth = 100;

      if (halign === 'right') {
          //textWidth = this.config.getTextWidth(gc, this.config.value);
          halignOffset = width - colHEdgeOffset - textWidth;
      } else if (halign === 'center') {
          //textWidth = this.config.getTextWidth(gc, this.config.value);
          halignOffset = (width - textWidth) / 2;
      } else if (halign === 'left') {
          halignOffset = colHEdgeOffset;
      }

      halignOffset = Math.max(0, halignOffset);
      valignOffset = valignOffset + Math.ceil(height / 2);

      //fill background only if our bgColor is populated or we are a selected cell
      if (this.config.bgColor || this.config.isSelected) {
          gc.fillStyle = this.config.isSelected ? this.config.bgSelColor : this.config.bgColor;
          gc.fillRect(x, y, width, height);
      }

      // draw borders
      if (paintBorders.length > 0) {
        gc.beginPath();
        gc.lineWidth = borderConfig.width;
        gc.strokeStyle = borderConfig.color;
        if (borderConfig.lineType === 1)
          gc.setLineDash([5,5]); // 5px dash, 5px space
        for (var i=0; i<paintBorders.length; i++){
          gc.moveTo(x + paintBorders[i][0][0] * width, y + paintBorders[i][0][1] * height);
          gc.lineTo(x + paintBorders[i][1][0] * width, y + paintBorders[i][1][1] * height);
        }
        gc.stroke();
      }



      //draw text
      var theColor = this.config.isSelected ? this.config.fgSelColor : this.config.fgColor;
      if (gc.fillStyle !== theColor) {
          gc.fillStyle = theColor;
          gc.strokeStyle = theColor;
      }
      if (val !== null) {
          gc.fillText(val, x + halignOffset, y + valignOffset);
      }
      // streaming cell
      if (this.config.isStreaming) {
        gc.fillText("S", x + halignOffset, y + valignOffset);
      }
      if (isColumnHovered && isRowHovered) {
          gc.beginPath();
          if (isLink) {
              gc.beginPath();
              underline(this.config, gc, val, x + halignOffset, y + valignOffset + Math.floor(fontMetrics.height / 2), 1);
              gc.stroke();
              gc.closePath();
          }
      }

      var iconWidth = 0;
      if (leftIcon) {
          iyoffset = Math.round((height - leftIcon.height) / 2);
          ixoffset = Math.round((halignOffset - leftIcon.width) / 2);
          gc.drawImage(leftIcon, x + ixoffset, y + iyoffset);
          iconWidth = Math.max(leftIcon.width + 2);
      }
      if (rightIcon) {
          iyoffset = Math.round((height - rightIcon.height) / 2);
          ixoffset = Math.round((halignOffset - rightIcon.width) / 2);
          gc.drawImage(rightIcon, x + width - ixoffset - rightIcon.width, y + iyoffset);
          iconWidth = Math.max(rightIcon.width + 2);
      }
      if (centerIcon) {
          iyoffset = Math.round((height - centerIcon.height) / 2);
          ixoffset = Math.round((width - centerIcon.width) / 2);
          gc.drawImage(centerIcon, x + width - ixoffset - centerIcon.width, y + iyoffset);
          iconWidth = Math.max(centerIcon.width + 2);
      }
      this.config.minWidth = this.config.minWidth + 2 * (iconWidth);
    }
  },

  imageCellRenderer: {
    paint: function(gc, x, y, width, height, isLink) {
      isLink = isLink || false;
      var colHEdgeOffset = this.config.properties.cellPadding,
          val = this.config.value,
          img = this.config.ASImage;

      //we must set this in order to compute the minimum width
      //for column autosizing purposes
      var textWidth = this.config.getTextWidth(gc, val);
      this.config.minWidth = textWidth + (2 * colHEdgeOffset);

      // draw image
      gc.drawImage(img, x, y);
    }
  }
}
