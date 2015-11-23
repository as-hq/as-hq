/**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */
/* React */

/**
 * Base class of all React classes, modeled as a polymorphic class whose type
 * parameters are DefaultProps, Props, State.
 */
declare class ReactComponent<DefaultProps, Props, State> {
  props: Props;
  state: State;
  refs: any;
  context: any;

  getInitialState(): State;
  getChildContext(): any;

  setProps(props: $Shape<Props>, callback?: () => void): void;
  replaceProps(props: Props, callback?: () => void): void;

  setState(state: $Shape<State>, callback?: () => void): void;
  replaceState(state: State, callback?: () => void): void;

  render(): ?ReactElement<any, any, any>;

  forceUpdate(callback?: () => void): void;
  getDOMNode(): any;
  componentWillMount(): void;
  componentDidMount(component?: any): void;
  componentWillReceiveProps(nextProps: Props, nextContext?: any): void;
  shouldComponentUpdate(nextProps: Props, nextState: State, nextContext: any): boolean;
  componentWillUpdate(nextProps: Props, nextState: State, nextContext: any): void;
  componentDidUpdate(nextProps: Props, nextState: State, nextContext: any, component: any): void;
  componentWillUnmount(): void;
  isMounted(): bool;

  static propTypes: $Subtype<{[_: $Keys<Props>]: any}>; //object whose keys are in PropTypes
  static contextTypes: any;
  static childContextTypes: any;
  static displayName: string;
  static defaultProps: DefaultProps;
}

/**
 * Type of a React class (not to be confused with the type of instances of a
 * React class, which is the React class itself). A React class is any subclass
 * of ReactComponent. We make the type of a React class polymorphic over the
 * same type parameters (DefaultProps, Props, State) as ReactComponent. The required constraints
 * are set up using a "helper" type alias, that takes an additional type
 * parameter C representing the React class, which is then abstracted with an
 * existential type (*). The * can be thought of as an "auto" instruction to the
 * typechecker, telling it to fill in the type from context.
 */
type ReactClass<DefaultProps, Props, State> = _ReactClass<DefaultProps, Props, State, *>;
type _ReactClass<DefaultProps, Props, State, C: ReactComponent<DefaultProps, Props, State>> = Class<C>;

/**
 * Type of a React element. React elements are commonly created using JSX
 * literals, which desugar to React.createElement calls (see below).
 */
declare class ReactElement<DefaultProps, Props, State> {
    type: ReactClass<DefaultProps, Props, State>;
    props: Props;
    key: ?string;
    ref: any;
}

type ReactPropsCheckType = (
  props:any,
  propName: string,
  componentName: string,
  href?: string) => ?Error;

type ReactPropsChainableTypeChecker = {
  isRequired: ReactPropsCheckType;
  (props:any, propName: string, componentName: string, href?: string): ?Error;
};

type ReactPropTypes = {
  array: ReactPropsChainableTypeChecker;
  bool: ReactPropsChainableTypeChecker;
  func: ReactPropsChainableTypeChecker;
  number: ReactPropsChainableTypeChecker;
  object: ReactPropsChainableTypeChecker;
  string: ReactPropsChainableTypeChecker;

  any: ReactPropsChainableTypeChecker;
  arrayOf: (typeChecker: ReactPropsCheckType) => ReactPropsChainableTypeChecker;
  element: ReactPropsChainableTypeChecker;
  instanceOf: (expectedClass: any) => ReactPropsChainableTypeChecker;
  node: ReactPropsChainableTypeChecker;
  objectOf: (typeChecker: ReactPropsCheckType) => ReactPropsChainableTypeChecker;
  oneOf: (expectedValues: Array<any>) => ReactPropsChainableTypeChecker;
  oneOfType: (arrayOfTypeCheckers: Array<ReactPropsCheckType>) => ReactPropsChainableTypeChecker;
  shape: (shapeTypes: { [key: string]: ReactPropsCheckType }) => ReactPropsChainableTypeChecker;
}

declare module react {
  declare var Children: any;
  declare var DOM: any;
  declare var PropTypes: ReactPropTypes;
  declare var version: string;

  declare function initializeTouchEvents(shouldUseTouch: boolean): void;

  // compiler magic
  declare function createClass(spec: any): ReactClass<any, any, any>;

  declare function cloneElement<DefaultProps, Props, State> (
    element: ReactElement<DefaultProps, Props, State>,
    attributes: $Shape<Props>,
    children?: any
  ): ReactElement<DefaultProps, Props, State>;

  /**
   * Methods that take an `attributes` argument of type A (= Attributes),
   * describing objects whose properties must cover (at least) the difference
   * between the properties in Props and the properties in DefaultProps.
   * This is intended to model what happens at run time: the
   * attributes are merged with the default props to obtain the props of a
   * ReactElement / ReactComponent instance.
   */
  // TODO: React DOM elements
  declare function createElement<DefaultProps, Props, State, A: $Diff<Props, DefaultProps>>(
    name: ReactClass<DefaultProps, Props, State>,
    attributes: A,
    children?: any
  ): ReactElement<DefaultProps, Props, State>;

  // TODO: React DOM elements
  declare function createFactory<DefaultProps, Props, State, A: $Diff<Props, DefaultProps>>(
    name: ReactClass<DefaultProps, Props, State>
  ): (attributes: A, children?: any) => ReactElement<DefaultProps, Props, State>;

  // TODO: React DOM elements
  declare function constructAndRenderComponent<DefaultProps, Props, State, A: $Diff<Props, DefaultProps>>(
    name: ReactClass<DefaultProps, Props, State>,
    attributes: A,
    container: any
  ): ReactComponent<DefaultProps, Props, State>;

  // TODO: React DOM elements
  declare function constructAndRenderComponentByID<DefaultProps, Props, State, A: $Diff<Props, DefaultProps>>(
    name: ReactClass<DefaultProps, Props, State>,
    attributes: A,
    id: string
  ): ReactComponent<DefaultProps, Props, State>;

  declare function findDOMNode(
    object: ReactComponent<any, any, any> | HTMLElement
  ): any;

  declare function render<DefaultProps, Props, State>(
    element: ReactElement<DefaultProps, Props, State>,
    container: any
  ): ReactComponent<DefaultProps, Props, State>;

  declare function renderToString(
    element: ReactElement<any, any, any>
  ): string;
  declare function renderToStaticMarkup(
    element: ReactElement<any, any, any>
  ): string;

  declare function unmountComponentAtNode(container: any): boolean;

  declare function isValidElement(element: any): boolean;
  declare function withContext(context: any, callback: () => void): any;

  declare var Component: typeof ReactComponent;
  declare var Element: typeof ReactElement;
}

// TODO Delete this once https://github.com/facebook/react/pull/3031 lands
// and "react" becomes the standard name for this module
declare module React {
    declare var exports: $Exports<'react'>;
}

declare class SyntheticEvent {
    bubbles: boolean;
    cancelable: boolean;
    currentTarget: EventTarget;
    defaultPrevented: boolean;
    eventPhase: number;
    isDefaultPrevented(): boolean;
    isPersistent: boolean;
    isPropagationStopped(): boolean;
    isTrusted: boolean;
    nativeEvent: Event;
    persist(): void;
    preventDefault(): void;
    stopPropagation(): void;
    target: EventTarget;
    timeStamp: number;
    type: string;
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
  children?: ReactElement;
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

declare module 'react-dom' {
  declare function findDOMNode(): HTMLElement;
}
