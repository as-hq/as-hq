declare module 'material-ui' {
  declare class Snackbar extends React$Component {
    show(): void;
    dismiss(): void;
  }

  declare class FontIconProps {
    color?: string;
    hoverColor?: string;
    onMouseLeave?: (e: SyntheticMouseEvent) => void;
    onMouseEnter?: (e: SyntheticMouseEvent) => void;
    style?: {[key: string]: any};
    className?: string;
  }

  declare class PaperProps extends ReactHTMLAttributesBase<any> {
    circle?: boolean;
    rounded?: boolean;
    transitionEnabled?: boolean;
    zDepth?: number;
  }

  declare class TextFieldProps extends ReactProps<any> {
    errorStyle?: {[key: string]: any};
    errorText?: string;
    floatingLabelText?: string;
    floatingLabelStyle?: {[key: string]: any};
    fullWidth?: boolean;
    hintText?: string; //| React$Element<any>;
    id?: string;
    inputStyle?: {[key: string]: any};
    multiLine?: boolean;
    onEnterKeyDown?: (e: SyntheticKeyboardEvent) => void;
    style?: {[key: string]: any};
    rows?: number,
    underlineStyle?: {[key: string]: any};
    underlineFocusStyle?: {[key: string]: any};
    underlineDisabledStyle?: {[key: string]: any};
    type?: string;
    hintStyle?: {[key: string]: any};

    disabled?: boolean;
    isRtl?: boolean;
    value?: string;
    defaultValue?: string;
    valueLink?: ReactLink<string>;

    onBlur?: (e: SyntheticFocusEvent) => void;
    onChange?: (e: SyntheticUIEvent) => void;
    onFocus?: (e: SyntheticFocusEvent) => void;
    onKeyDown?: (e: SyntheticKeyboardEvent) => void;
  }

  declare class MenuItemRequest extends ReactProps<any> {
    // use value from MenuItem.Types.*
    type?: string;

    text?: string;
    data?: string;
    payload?: string;
    icon?: React$Element;
    attribute?: string;
    number?: string;
    toggle?: boolean;
    onTouchTap?: (e: SyntheticTouchEvent) => void;
    isDisabled?: boolean;

    // for MenuItems.Types.NESTED
    items?: MenuItemRequest[];
  }

  declare class DropDownMenuProps extends ReactProps<any> {
    displayMember?: string;
    valueMember?: string;
    autoWidth?: boolean;
    menuItems: Array<MenuItemRequest>;
    menuItemStyle?: {[key: string]: any};
    selectedIndex?: number;
    underlineStyle?: {[key: string]: any};
    iconStyle?: {[key: string]: any};
    labelStyle?: {[key: string]: any};
    style?: {[key: string]: any};
    disabled?: boolean;
    valueLink?: ReactLink<any>;
    value?: number;

    onChange?: (e: SyntheticTouchEvent, index: number, menuItem: MenuItemRequest) => void;
  }

  declare type DialogAction = {
    id?: string;
    text: string;
    ref?: string;

    onTouchTap?: (e: SyntheticTouchEvent) => void;
    onClick?: (e: SyntheticMouseEvent) => void;
  }

  declare class DialogProps extends ReactProps<any> {
    actions?: Array<DialogAction | React$Element>;
    actionFocus?: string;
    autoDetectWindowHeight?: boolean;
    autoScrollBodyContent?: boolean;
    style?: {[key: string]: any};
    bodyStyle?: {[key: string]: any};
    contentClassName?: string;
    contentInnerStyle?: {[key: string]: any};
    contentStyle?: {[key: string]: any};
    modal?: boolean;
    openImmediately?: boolean;
    repositionOnUpdate?: boolean;
    title?: string;
    defaultOpen?: boolean;
    open?: boolean;

    onClickAway?: () => void;
    onDismiss?: () => void;
    onShow?: () => void;
    onRequestClose?: (buttonClicked: boolean) => void;
  }

  declare class FlatButtonProps extends ReactProps<any> {
    hoverColor?: string;
    label?: string;
    labelPosition?: string;
    labelStyle?: {[key: string]: any};
    linkButton?: boolean;
    primary?: boolean;
    secondary?: boolean;
    rippleColor?: string;
    style?: {[key: string]: any};
  }

  declare class PopoverOrigin {
    horizontal: 'left' | 'right';
    vertical: 'top' | 'bottom';
  }

  declare class PopoverProps extends ReactProps<any> {
    anchorOrigin: PopoverOrigin;
    anchorEl: ?HTMLElement;
    open: boolean;
    onRequestClose: () => void;
  }

  declare class FontIcon extends React$Component {
    props: FontIconProps;
  }

  declare class Paper extends React$Component {
    props: PaperProps;
  }

  declare class TextField extends React$Component {
    props: TextFieldProps;
  }

  declare class DropDownMenu extends React$Component {
    props: DropDownMenuProps;
  }

  declare class Dialog extends React$Component {
    props: DialogProps;
  }

  declare class FlatButton extends React$Component {
    props: FlatButtonProps;
  }

  declare class Popover extends React$Component {
    props: PopoverProps;
  }

  declare var Styles: {
    Colors: {[key: string]: string};
  };
}
