/* @flow */

import type {
  ExpandingType,
  ValueError,
  ASValue,
  ASCellProp
} from '../../types/Eval';

import type {
  CellBorder,
  GridSpec
} from '../../types/Hypergrid';

import {logDebug} from '../Logger';
import Constants from '../../Constants';
import Conversion from './Conversion';
import LocationUtils from './Location';
import Format from './Format';
import shortid from 'shortid';
import U from '../Util';

import ASIndex from '../../classes/ASIndex';
import ASRange from '../../classes/ASRange';

const Render = {
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

  // If there's info in the props about how many decimal places to show, insert that
  // information into the config.
  formatNumberOfDecimalPlaces(config: HGRendererConfig, props: Array<ASCellProp>) {
    let numDec = null,
        formatInd = props.map(({tag}) => tag).indexOf("ValueFormat");
    if (formatInd >= 0) {
      let format = props[formatInd];
      if (format.tag === "ValueFormat") { // for flow
        numDec = format.valFormat.numDecimals;
      }
    }
    config.value = Format.formatDecimal(numDec, config.value);
  },

  propsToRenderConfig(config: HGRendererConfig, props: Array<ASCellProp>) {
    for (var i=0; i<props.length; i++) {
      let prop = props[i];
      switch (prop.tag) {
        case "TextColor":
          config.fgColor = Conversion.colorToHtml(prop.contents);
          break;
        case "FillColor":
          config.bgSelColor = '#64B5F6';
          config.bgColor = Conversion.colorToHtml(prop.contents);
          break;
        case "TopAlign": // not implemented yet
          break;
        case "HAlign":
          config.halign = Conversion.asHAlignToHtml(prop.contents);
          break;
        case "FontSize": //not implemented yet
          break;
        case "FontName": //not implemented yet
          break;
        case "URL": //not implemented yet
          break;
        case "ValueFormat":
          switch (prop.valFormat.formatType) {
            case "Money":
              config.value = Format.formatMoney("$", config.value, 2);
              break;
            case "Percentage":
              config.value = Format.formatPercentage(config.value);
              break;
            case "Date":
              config.value = Format.formatDate(config.value);
              break;
            default:
              break;
          }
          // Change the precision accordingly if the number of decimal offsets is given
          config.value = Format.formatDecimal(prop.valFormat.numDecimals, config.value);
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
  },

  expandingTypeToRenderConfig(config: HGRendererConfig, etype: ExpandingType) {
    config.bgSelColor = '#64B5F6';
    switch (etype) {
      case "List":
        config.bgColor = Constants.Colors.cornsilk;
        break;
      default:
        config.bgColor = Constants.Colors.lightcyan;
    }
  },

  valueToRenderConfig(config: HGRendererConfig, val: ASValue) {
    switch(val.tag) {
      case "ValueI":
      case "ValueD":
        config.halign = 'right';
        break;
      case "ValueS":
        config.halign = 'left';
        break;
      case "ValueImage":
        config.bgColor = Constants.Colors.lavender;
        break;
      case "ValueSerialized":
        config.bgColor = Constants.Colors.powderblue;
        break;
      default:
        config.halign = 'center';
        break;
    }
  },

  getPaintedBordersForSingleCell(): CellBorder {
    return [[[0,0],[1,0]],
            [[1,0],[1,1]],
            [[1,1],[0,1]],
            [[0,1],[0,0]]];
  },

  getBordersForInteriorCell(col: number, row: number, rng: ASRange): CellBorder {
    let {tl, br} = rng;
    if (rng.isIndex() && (col === tl.col && row === tl.row)) {
      return Render.getPaintedBordersForSingleCell();
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
  getPaintedBorders(col: number, row: number, rngs: Array<ASRange>): Array<CellBorder> {
    return rngs.map((rng) => Render.getBordersForInteriorCell(col, row, rng), Render);;
  },

  getUniqueId(): string {
    return shortid.generate();
  },

  getX(col: number, scrollX: number): string {
    return (col-scrollX) * Constants.cellWidthPx + Constants.gridXOffset + "px";
  },

  getY(row: number, scrollY: number): string {
    return (row-scrollY)* Constants.cellHeightPx + Constants.gridYOffset + "px";
  },

  getGridSpec(renderer: HGRendererElement): GridSpec {
    const grid = renderer.getGrid();
    const fixedColCount = grid.getFixedColumnCount();
    const fixedRowCount = grid.getFixedRowCount();
    const scrollX = grid.getHScrollValue();
    const scrollY = grid.getVScrollValue();
    const firstVisibleColumn = renderer.getVisibleColumns()[0];
    const firstVisibleRow = renderer.getVisibleRows()[0];
    const lastVisibleColumn = renderer.getVisibleColumns().slice(-1)[0];
    const lastVisibleRow = renderer.getVisibleRows().slice(-1)[0];

    return {
      fixedColCount,
      fixedRowCount,
      scrollX,
      scrollY,
      firstVisibleColumn,
      firstVisibleRow,
      lastVisibleColumn,
      lastVisibleRow
    };
  },
};

export default Render;
