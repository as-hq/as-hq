declare class SyntheticEvent {
    bubbles?: boolean;
    cancelable?: boolean;
    currentTarget?: EventTarget;
    defaultPrevented?: boolean;
    eventPhase?: number;
    isDefaultPrevented?: () => boolean;
    isPersistent?: boolean;
    isPropagationStopped?: () => boolean;
    isTrusted?: boolean;
    nativeEvent?: Event;
    persist: () => void;
    preventDefault: () => void;
    stopPropagation: () => void;
    target?: EventTarget;
    timeStamp?: number;
    type?: string;
}

declare class SyntheticClipboardEvent extends SyntheticEvent {
    clipboardData: any;
}

declare class SyntheticCompositionEvent extends SyntheticEvent {
    data: any;
}

declare class SyntheticInputEvent extends SyntheticEvent {
    data: any;
}

declare class SyntheticUIEvent extends SyntheticEvent {
    detail: number;
    view: any;
}

declare class SyntheticFocusEvent extends SyntheticUIEvent {
    relatedTarget: EventTarget;
}

declare class SyntheticKeyboardEvent extends SyntheticUIEvent {
    altKey: boolean;
    charCode: number;
    ctrlKey: boolean;
    getModifierState: any;
    key: string;
    keyCode: number;
    locale: string;
    location: number;
    metaKey: boolean;
    repeat: boolean;
    shiftKey: boolean;
    which: number;
}

declare class SyntheticMouseEvent extends SyntheticUIEvent {
    altKey: boolean;
    button: number;
    buttons: number;
    clientX: number;
    clientY: number;
    ctrlKey: boolean;
    getModifierState: any;
    metaKey: boolean;
    pageX: number;
    pageY: number;
    relatedTarget: EventTarget;
    screenX: number;
    screenY: number;
    shiftKey: boolean;
}

declare class SyntheticDragEvent extends SyntheticMouseEvent {
    dataTransfer: any;
}

declare class SyntheticWheelEvent extends SyntheticMouseEvent {
    deltaMode: number;
    deltaX: number;
    deltaY: number;
    deltaZ: number;
}

declare class SyntheticTouchEvent extends SyntheticUIEvent {
    altKey: boolean;
    changedTouches: any;
    ctrlKey: boolean;
    getModifierState: any;
    metaKey: boolean;
    shiftKey: boolean;
    targetTouches: any;
    touches: any;
}

declare class ReactCSSProperties {
  boxFlex?: number;
  boxFlexGroup?: number;
  columnCount?: number;
  flex?: number | string;
  flexGrow?: number;
  flexShrink?: number;
  fontWeight?: number | string;
  lineClamp?: number;
  lineHeight?: number | string;
  opacity?: number;
  order?: number;
  orphans?: number;
  widows?: number;
  zIndex?: number;
  zoom?: number;

  fontSize?: number | string;

  // SVG-related properties
  fillOpacity?: number;
  strokeOpacity?: number;
  strokeWidth?: number;

  [propertyName: string]: any;
}

declare class ReactProps<T> {
  children?: React$Element;
  key?: string | number;
  ref?: string | ((component: T) => any);
}

declare class ReactDOMAttributesBase<T> extends ReactProps<T> {
  onCopy?: (e: SyntheticClipboardEvent) => void;
  onCut?: (e: SyntheticClipboardEvent) => void;
  onPaste?: (e: SyntheticClipboardEvent) => void;
  onKeyDown?: (e: SyntheticKeyboardEvent) => void;
  onKeyPress?: (e: SyntheticKeyboardEvent) => void;
  onKeyUp?: (e: SyntheticKeyboardEvent) => void;
  onFocus?: (e: SyntheticFocusEvent) => void;
  onBlur?: (e: SyntheticFocusEvent) => void;
  onChange?: (e: SyntheticEvent) => void;
  onInput?: (e: SyntheticEvent) => void;
  onSubmit?: (e: SyntheticEvent) => void;
  onClick?: (e: SyntheticMouseEvent) => void;
  onContextMenu?: (e: SyntheticMouseEvent) => void;
  onDoubleClick?: (e: SyntheticMouseEvent) => void;
  onDrag?: (e: SyntheticDragEvent) => void;
  onDragEnd?: (e: SyntheticDragEvent) => void;
  onDragEnter?: (e: SyntheticDragEvent) => void;
  onDragExit?: (e: SyntheticDragEvent) => void;
  onDragLeave?: (e: SyntheticDragEvent) => void;
  onDragOver?: (e: SyntheticDragEvent) => void;
  onDragStart?: (e: SyntheticDragEvent) => void;
  onDrop?: (e: SyntheticDragEvent) => void;
  onMouseDown?: (e: SyntheticMouseEvent) => void;
  onMouseEnter?: (e: SyntheticMouseEvent) => void;
  onMouseLeave?: (e: SyntheticMouseEvent) => void;
  onMouseMove?: (e: SyntheticMouseEvent) => void;
  onMouseOut?: (e: SyntheticMouseEvent) => void;
  onMouseOver?: (e: SyntheticMouseEvent) => void;
  onMouseUp?: (e: SyntheticMouseEvent) => void;
  onTouchCancel?: (e: SyntheticTouchEvent) => void;
  onTouchEnd?: (e: SyntheticTouchEvent) => void;
  onTouchMove?: (e: SyntheticTouchEvent) => void;
  onTouchStart?: (e: SyntheticTouchEvent) => void;
  onScroll?: (e: SyntheticUIEvent) => void;
  onWheel?: (e: SyntheticWheelEvent) => void;

  className?: string;
  id?: string;

  dangerouslySetInnerHTML?: {
      __html: string;
  };
}

declare class ReactHTMLAttributesBase<T> extends ReactDOMAttributesBase<T> {
  accept?: string;
  acceptCharset?: string;
  accessKey?: string;
  action?: string;
  allowFullScreen?: boolean;
  allowTransparency?: boolean;
  alt?: string;
  async?: boolean;
  autoComplete?: boolean;
  autoFocus?: boolean;
  autoPlay?: boolean;
  cellPadding?: number | string;
  cellSpacing?: number | string;
  charSet?: string;
  checked?: boolean;
  classID?: string;
  cols?: number;
  colSpan?: number;
  content?: string;
  contentEditable?: boolean;
  contextMenu?: string;
  controls?: any;
  coords?: string;
  crossOrigin?: string;
  data?: string;
  dateTime?: string;
  defaultChecked?: boolean;
  defaultValue?: string;
  defer?: boolean;
  dir?: string;
  disabled?: boolean;
  download?: any;
  draggable?: boolean;
  encType?: string;
  form?: string;
  formAction?: string;
  formEncType?: string;
  formMethod?: string;
  formNoValidate?: boolean;
  formTarget?: string;
  frameBorder?: number | string;
  headers?: string;
  height?: number | string;
  hidden?: boolean;
  high?: number;
  href?: string;
  hrefLang?: string;
  htmlFor?: string;
  httpEquiv?: string;
  icon?: string;
  label?: string;
  lang?: string;
  list?: string;
  loop?: boolean;
  low?: number;
  manifest?: string;
  marginHeight?: number;
  marginWidth?: number;
  max?: number | string;
  maxLength?: number;
  media?: string;
  mediaGroup?: string;
  method?: string;
  min?: number | string;
  multiple?: boolean;
  muted?: boolean;
  name?: string;
  noValidate?: boolean;
  open?: boolean;
  optimum?: number;
  pattern?: string;
  placeholder?: string;
  poster?: string;
  preload?: string;
  radioGroup?: string;
  readOnly?: boolean;
  rel?: string;
  required?: boolean;
  role?: string;
  rows?: number;
  rowSpan?: number;
  sandbox?: string;
  scope?: string;
  scoped?: boolean;
  scrolling?: string;
  seamless?: boolean;
  selected?: boolean;
  shape?: string;
  size?: number;
  sizes?: string;
  span?: number;
  spellCheck?: boolean;
  src?: string;
  srcDoc?: string;
  srcSet?: string;
  start?: number;
  step?: number | string;
  style?: {[key: string]: any};
  tabIndex?: number;
  target?: string;
  title?: string;
  type?: string;
  useMap?: string;
  value?: string;
  width?: number | string;
  wmode?: string;

  // Non-standard Attributes
  autoCapitalize?: boolean;
  autoCorrect?: boolean;
  property?: string;
  itemProp?: string;
  itemScope?: boolean;
  itemType?: string;
  unselectable?: boolean;
}

declare class ReactLink<T> {
  value: T;
  requestChange(newValue: T): void;
}

declare module 'react-dom' {
  declare function findDOMNode(
    object: React$Component<any, any, any> | HTMLElement
  ): any;

  declare function render<DefaultProps, Props, State>(
    element: React$Element<DefaultProps, Props, State>,
    container: any
  ): React$Component<DefaultProps, Props, State>;

  declare function unmountComponentAtNode(container: any): boolean;
}

declare module 'react-dom/server' {
  declare function renderToString(
    element: React$Element<any, any, any>
  ): string;
  declare function renderToStaticMarkup(
    element: React$Element<any, any, any>
  ): string;
}

declare module 'react-addons-perf' {
  // We don't actually use anything from this module in our code, just assign
  // it to window.Perf
  declare var exports: any;
}

declare module 'react-tooltip' {
  declare type TooltipProps = {
    id: string;
    delayHide: ?number;
    delayShow: ?number;
    place?: 'top' | 'right' | 'bottom' | 'left';
    type?: 'success' | 'warning' | 'error' | 'info' | 'light';
    effect?: 'float' | 'solid';
    offset?: {
      top?: number;
      right?: number;
      bottom?: number;
      left?: number;
    };
  };

  declare var exports: ReactClass<{}, TooltipProps, {}>;
}

declare module 'react-addons-shallow-compare' {
  declare function exports(this: any, newProps: any, newState: any): boolean;
}

declare class Image extends React$Component { }
