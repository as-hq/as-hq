declare module 'material-ui' {
  declare class Snackbar extends ReactComponent {
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
    hintText?: string; //| ReactElement<any>;
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
    icon?: ReactElement;
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

  declare class FontIcon extends ReactComponent<FontIconProps, FontIconProps, {}> {
  }

  declare class Paper extends ReactComponent<PaperProps, PaperProps, {}> {
  }

  declare class TextField extends ReactComponent<TextFieldProps, TextFieldProps, {}> {
  }

  declare class DropDownMenu extends ReactComponent<DropDownMenuProps, DropDownMenuProps, {}> {
  }

  declare var Styles: {
    Colors: {[key: string]: string};
  };
}
