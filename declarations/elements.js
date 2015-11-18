declare class GraphicsContext {}

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
    primitiveEvent: HGMouseEvent;
  };
}

declare class HGRendererConfig {
  fgColor: string;
  bgColor: string;
  font: string;
  halign: string;
  value: string;
  isStreaming: boolean;
  x: number;
  y: number;
}

declare class HGRendererObject {
  paint: (gc: GraphicsContext) => void;
}

declare class HGCellEditorElement extends HTMLElement {

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

  onMouseMove: (grid: HGElement, evt: HGMouseEvent) => void;
  onMouseDrag: (grid: HGElement, evt: HGMouseEvent) => void;
  onMouseUp: (grid: HGElement, evt: HGMouseEvent) => void;

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
  addExtraRenderer(renderer: HGRendererObject): void;

  getGridCellFromMousePoint(pt: HGPoint): {gridCell: HGPoint};

  startAnimator(): void;
}

declare class HGMouseEvent {
  gridCell: HGPoint;
  mousePoint: HGPoint;
  primitiveEvent: HGPrimitiveMouseEvent;
}

declare class HGElement extends HTMLElement {
  hScrollValue: number;
  vScrollValue: number;
  renderer: HGRendererElement;

  addFinEventListener(
    name: string,
    cb: (ev: (HGMouseEvent)) => void
  ): void;
  addGlobalProperties(props: mixed): void;

  clearSelections(): void;

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

  repaint(): void;
  scrollBy(x: number, y: number): void;
  select(x: number, y: number, dx: number, dy: number): void;
  takeFocus(): void;
}

declare class AEPoint {
  row: number;
  column: number;
}

declare class AEWordRange {
  start: AEPoint;
  end: AEPoint;

  clone(): AEWordRange;
}

declare class AESession {
  getWordRange(r: number, c: number): AEWordRange;
  getTextRange(rng: AEWordRange): string;
}

declare class AERawClass {
  focus(): void;
  getValue(): string;
  getSession(): AESession;
  navigateFileEnd(): void;
}

declare class AEElement extends HTMLElement {
}
