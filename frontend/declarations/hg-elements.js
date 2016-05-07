declare class HGPoint {
  x: number; y: number;
}

declare class HGRectangle {
  origin: HGPoint;
  extent: HGPoint;

  height(): number;
  width(): number;
}

declare class HGPrimitiveMouseEvent extends Event {
  // TODO: this doesn't include some fields
  detail: {
    mouse: HGPoint;
    keys: string;
    primitiveEvent: SyntheticMouseEvent & {
      x: number,
      y: number,
      offsetX: number,
      offsetY: number,
      which: number
    };
  };
}

declare class GraphicsContext {
  font: string;
  textAlign: string;
  textBaseline: string;
  lineWidth: number;
  fillStyle: string;
  fillRect: (x: number, y: number, width: number, height: number) => void;
  rect: (x: number, y: number, width: number, height: number) => void;
  strokeStyle: string;
  fillText: (val: string, x: number, y: number, maxWidth?: number) => void;
  beginPath: () => void;
  stroke: () => void;
  closePath: () => void;
  drawImage: (rightIcon: any, x: number, y: number) => void;
  setLineDash: (dashes: Array<number>) => void;
  fill: () => void;
  moveTo: (x: number, y: number) => void;
  lineTo: (x: number, y: number) => void;
  measureText: (line: string) => { width: number };
}

declare class HGRendererConfig {
  fgColor: string;
  bgColor: string;
  bgSelColor: ?string;
  fgSelColor: ?string;
  font: string;
  halign: string;
  value: string;
  isStreaming: boolean;
  x: number;
  y: number;
  wrap: boolean;
  getTextWidth: (gc: GraphicsContext, text: string) => number;
  isLink: boolean;
  borderTop?: boolean;
  borderBottom?: boolean;
  borderLeft?: boolean;
  borderRight?: boolean;
}


declare class HGRendererObject {
  config?: HGRendererConfig;
  paint?: (gc: GraphicsContext, x: number, y: number, width: number, height: number, isLink?: boolean) => void;
}


declare class HGCellEditorElement extends HTMLElement {

}

declare class HGFeatureChainElement extends HTMLElement {
  isFixedRow: (grid: HGElement, evt: HGMouseEvent) => boolean;
  isFixedColumn: (grid: HGElement, evt: HGMouseEvent) => boolean;
  handleMouseUp: (grid: HGElement, evt: HGMouseEvent) => void;
  handleMouseDrag: (grid: HGElement, evt: HGMouseEvent) => void;
  handleDoubleClick: (grid: HGElement, evt: HGMouseEvent) => void;
  handleMouseMove: (grid: HGElement, evt: HGMouseEvent) => void;
  handleMouseDown: (grid: HGElement, evt: HGMouseEvent) => void;
}

declare class HGCellProviderElement extends HTMLElement {
  getCell: (cfg: HGRendererConfig) => HGRendererObject;
}

declare class HGFeatureElement extends HTMLElement {
}

declare class HGBehaviorElement extends HTMLElement {
  changed(): void;

  handleMouseDown: (grid: HGElement, evt: HGMouseEvent) => void;

  getCellProvider(): HGCellProviderElement;
  getColumnCount: () => number;
  getRowCount: () => number;
  getValue: () => any;
  getCellEditorAt: (x: number, y: number) => ?HGCellEditorElement;
  getCursorAt: (x: number, y: number) => ?string;

  highlightCellOnHover: (isColumnHovered: number, isRowHovered: number) => boolean;

  onMouseMove: (grid: HGElement, evt: HGMouseEvent) => void;
  onMouseDrag: (grid: HGElement, evt: HGMouseEvent) => void;
  onMouseUp: (grid: HGElement, evt: HGMouseEvent) => void;
  onDoubleClick: (grid: HGElement, evt: HGMouseEvent) => void;

  swapColumns: (c1: number, c2: number) => void;
  featureChain: HGFeatureChainElement;

  _setColumnWidth(columnIndex: number, columnWidth: ?number): void;
  setRowHeight: (rowNum: number, height: ?number) => void;

  setCursor(grid: HGElement): void;
  setValue(x: number, y: number, val: string): void;
}

declare class HGSelectionModelElement extends HTMLElement {
  getSelections(): Array<HGRectangle>;
}

declare class HGRectangleElement extends HTMLElement {
  point: {
    create: (x: number, y: number) => HGPoint;
  };
}

declare class HGRendererElement extends HTMLElement {
  addExtraRenderer(renderer: (gc: GraphicsContext) => void): void;
  getGrid(): HGElement;
  getGridCellFromMousePoint(pt: HGPoint): {gridCell: HGPoint};
  startAnimator(): void;
  renderedColumns: Array<number>;
  renderedRows: Array<number>;
  getVisibleColumns: () => Array<number>;
  getVisibleRows: () => Array<number>;

  _getBoundsOfCell(): HGRectangle;
}

declare class HGMouseEvent {
  gridCell: HGPoint;
  mousePoint: HGPoint;
  primitiveEvent: HGPrimitiveMouseEvent;
  detail: any;
}

declare class HGElement extends HTMLElement {
  hScrollValue: number;
  vScrollValue: number;
  autoScrollAcceleration: boolean;
  renderer: HGRendererElement;

  addFinEventListener(
    name: string,
    cb: (ev: (HGMouseEvent)) => void
  ): void;
  addGlobalProperties(props: mixed): void;

  clearSelections(): void;

  resolveProperty(prop: string): any;

  getBehavior(): HGBehaviorElement;
  getBoundsOfCell(cell: HGPoint): HGRectangle;
  getDataBounds(): HGRectangle;
  getFixedColumnCount(): number;
  getFixedRowCount(): number;
  getHScrollValue(): number;
  getVScrollValue(): number;
  getRenderer(): HGRendererElement;
  getSelectionModel(): HGSelectionModelElement;
  getVisibleColumns(): Array<number>;
  getVisibleRows(): Array<number>;

  setDragExtent(pt: HGPoint): void;
  setMouseDown(pt: HGPoint): void;
  setHScrollValue(x: number): void;
  setVScrollValue(y: number): void;

  setColumnWidth: (columnIndex: number, columnWidth: ?number) => void;
  getColumnWidth: (columnIndex: number) => number;

  setRowHeight: (rowIndex: number, rowHeight: ?number) => void;
  getRowHeight: (rowIndex: number) => number;

  repaint(): void;
  scrollBy(x: number, y: number): void;
  select(x: number, y: number, dx: number, dy: number): void;
  takeFocus(): void;

  overColumnDivider: (evt: HGMouseEvent) => number;
  overRowDivider: (evt: HGMouseEvent) => number;
}
