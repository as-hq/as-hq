/* @flow */

import type {
  ExpandingType,
  ValueError,
  NakedIndex,
  NakedRange,
  ASIndex,
  ASValue,
  ASCellProp,
  ASCell
} from '../../types/Eval';

import type {
  CellBorder,
  ASOverlaySpec
} from '../../types/Hypergrid';

import {logDebug} from '../Logger';

import Constants from '../../Constants';

import LocationUtils from './Location';

export default {
  safeExtractContentsFromValue(cv: ASValue): string {
    switch (cv.tag) {
      case 'NoValue':
      case 'ValueNaN':
      case 'ValueInf':
      case 'ValueImage':
        return cv.tag;
      case 'ValueSerialized':
        return cv.displayName;
      case 'ValueError':
        return cv.errorMsg;
      case 'ValueB':
      case 'ValueD':
      case 'ValueI':
      case 'ValueS':
        return cv.contents.toString();
    }
    return 'Extraneous type';
  },

  /* Used to know what to display on the sheet */
  showValue(cv: ASValue, isRepl: boolean = false): (string|number) {
    // logDebug("In show value: " + JSON.stringify(cv));
    let self = this;
    switch (cv.tag) {
      case "NoValue":
        return "";
      case "ValueNaN":
        return "NaN";
      case "ValueInf":
        return "Inf";
      case "ValueB":
        return cv.contents.toString().toUpperCase();
      case "ValueD":
        return cv.contents;
      case "ValueI":
        return cv.contents;
      case "ValueS":
        return cv.contents;
      case "ValueError":
        if (isRepl)
          return ((cv: any): ValueError).errorMsg;
        else return "ERROR"; // TODO: show more descriptive errors. (#REF? #NAME?)
      case "ValueImage":
        return "IMAGE";
      case "ValueSerialized":
        return cv.displayName;
      default:
        logDebug("CELL CONTENTS SHOW VALUE: ", cv);
        return JSON.stringify(cv);
    }
  },

  propsToRenderConfig(config: HGRendererConfig, props: Array<ASCellProp>): HGRendererConfig {
    let self = this;
    for (var i=0; i<props.length; i++) {
      let prop = props[i];
      switch (prop.tag) {
        case "TextColor":
          config.fgColor = self.colorToHtml(prop.contents);
          break;
        case "FillColor":
          config.bgColor = self.colorToHtml(prop.contents);
          break;
        case "TopAlign": // not implemented yet
          break;
        case "HAlign":
          config.halign = self.asHAlignToHtml(prop.contents);
          break;
        case "FontSize": //not implemented yet
          break;
        case "FontName": //not implemented yet
          break;
        case "URL": //not implemented yet
          break;
        case "ValueFormat":
          switch (prop.formatType) {
            case "Money":
              config.value = self.formatMoney("$", config.value, 2);
              break;
            case "Percentage":
              config.value = self.formatPercentage(config.value);
              break;
            case "Date":
              config.value = self.formatDate(config.value);
              break;
          }
          break;
        case "Bold":
          config.font = "bold " + config.font;
          break;
        case "Italic":
          config.font = "italic " + config.font;
          break;
        case "Streaming": // obsolete; need to update to StreamInfo
          config.isStreaming = true;
          break;
        default:
          break;
      }
    }
    return config;
  },

  expandingTypeToRenderConfig(config: HGRendererConfig, etype: ExpandingType): HGRendererConfig {
    switch (etype) {
      case "List":
        config.bgColor = Constants.Colors.cornsilk;
        return config;
      default:
        config.bgColor = Constants.Colors.lightcyan;
        return config;
    }
  },

  valueToRenderConfig(config: HGRendererConfig, val: ASValue): HGRendererConfig {
    switch(val.tag) {
      case "ValueI":
      case "ValueD":
        config.halign = 'right';
        return config;
      case "ValueS":
        config.halign = 'left';
        return config;
      case "ValueImage":
        config.bgColor = Constants.Colors.lavender;
        return config;
      case "ValueSerialized":
        config.bgColor = Constants.Colors.powderblue;
        return config;
      default:
        config.halign = 'center';
        return config;
    }
  },

  getPaintedBordersForSingleCell(): CellBorder {
    return [[[0,0],[1,0]],
            [[1,0],[1,1]],
            [[1,1],[0,1]],
            [[0,1],[0,0]]];
  },

  getBordersForInteriorCell(col: number, row: number, rng: NakedRange): CellBorder {
    let {tl, br} = rng;
    if (LocationUtils.isIndex(rng) && (col === tl.col && row === tl.row)) {
      return this.getPaintedBordersForSingleCell();
    } else {
      let borders: CellBorder = [null,null,null,null];
      if (col === tl.col) // left intersection
        borders[0] = [[0,0],[0,1]];
      if (col === br.col) // right intersection
        borders[1] = [[1,0],[1,1]];
      if (row === tl.row) // top intersection
        borders[2] = [[0,0],[1,0]];
      if (row === br.row) // bottom intersection
        borders[3] = [[0,1],[1,1]];
      return borders;
    }
  },

// determines borders of a cell to be painted, given that it falls somewhere within a list of locs
// returns a list of edges that can be painted in any order
// each edge is a 2-length array [start, end]
// executed by graphicscontext.moveTo(startx, starty) -> graphicscontext.lineTo(endx, endy)
  getPaintedBorders(col: number, row: number, rngs: Array<NakedRange>): Array<CellBorder> {
    let result = rngs.map((rng) => this.getBordersForInteriorCell(col, row, rng), this);
    return this.concatAll(result);
  },

  getImageOverlay(c: ASCell, originX: number, originY: number): ?ASOverlaySpec {
    let {cellValue: cv} = c;
    if (cv.tag === 'ValueImage') {
      let self = this,
          ct = c.cellProps,
          imageWidth   = 300,
          imageHeight  = 300,
          imageOffsetX = 0,
          imageOffsetY = 0;
      for (var i = 0 ; i < ct.length; i++) {
        if (ct[i].tag === "ImageData") {
          imageOffsetX = ct[i].imageOffsetX;
          imageOffsetY = ct[i].imageOffsetY;
          imageWidth   = ct[i].imageWidth;
          imageHeight  = ct[i].imageHeight;
        }
      }
      return {
        id: self.getUniqueId(),
        src: Constants.HOST_STATIC_URL + "/images/" + cv.imagePath,
        width: imageWidth,
        height: imageHeight,
        offsetX: imageOffsetX,
        offsetY: imageOffsetY,
        left: originX,
        top: originY,
        loc: c.cellLocation
      };
    }

    return null;
  },

  locEquals(c1: ASIndex, c2: ASIndex): boolean {
    let tagE = c1.tag === c2.tag,
        colE = c1.index.col === c2.index.col,
        rowE = c1.index.row === c2.index.row,
        sheetE = c1.sheetId === c2.sheetId
    return tagE && colE && rowE && sheetE;
  },

  simpleIndexEquals(c1: NakedIndex, c2: NakedIndex): boolean {
    return (c1.row === c2.row) && (c1.col === c2.col);
  },

  getX(col: number, scrollX: number): string {
    return (col-scrollX) * Constants.cellWidthPx + Constants.gridXOffset + "px";
  },

  getY(row: number, scrollY: number): string {
    return (row-scrollY)* Constants.cellHeightPx + Constants.gridYOffset + "px";
  }
};
