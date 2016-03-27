declare class AEPoint {
  row: number;
  column: number;
}

declare class AEWordRange {
  start: AEPoint;
  end: AEPoint;
}

declare class AEDocument {
  getAllLines(): Array<string>;
  getLine(n: number): string;
}

declare class AESession {
  doc: AEDocument;
  getWordRange(r: number, c: number): AEWordRange;
  getTextRange(rng: AEWordRange): string;
  replace(rng: AEWordRange, str: string): void;
  on(evtType: string, callback: (e: SyntheticKeyboardEvent) => void): void;
  setMode(mode: string): void;
  setUseWrapMode(x: boolean): void;
  setUseSoftTabs(x: boolean): void;
}

declare class AESelection {
  setRange(rng: AEWordRange): void;
  setSelectionRange(rng: AEWordRange, reverse: boolean): void;

  on(evtType: string, callback: (e: SyntheticKeyboardEvent) => void): void;
  off(evtType: string, callback: (e: SyntheticKeyboardEvent) => void): void;
}

declare class AECursorPosition {
  row: number;
  column: number
}

declare class AERenderer {
  setScrollMargin(top: number, bottom: number): void;
  setShowGutter(showGutter: boolean): void;
}

declare class AECommands {
  removeCommand(command: string): void;
};

declare class AERawClass {
  commands: AECommands;
  clearSelection(): void;
  focus(): void;
  getValue(): string;
  getSession(): AESession;
  getSelection(): AESelection;
  getCursorPosition(): AECursorPosition;
  moveCursorTo(row: number, column: number): void;
  getSelectedText(): string;
  moveCursorToPosition(pos: AECursorPosition): void;
  setValue(str: string): void;
  selectAll(): void;
  setTheme(theme: string): void;
  setShowPrintMargin(shouldShow: boolean): void;
  setFontSize(size: number): void;
  setOption(property: string, value: any): void;
  setOptions(options: any): void;
  insert(str: string): void;
  navigateFileStart(): void;
  navigateFileEnd(): void;
  selection: AESelection;
  renderer: AERenderer;
  resize(): void;

  on(evtType: string, callback: (e: SyntheticKeyboardEvent) => void): void;
  off(evtType: string, callback: (e: SyntheticKeyboardEvent) => void): void;
}

declare class AEElement extends HTMLElement {
}
