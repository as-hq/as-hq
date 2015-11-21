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

  declare class FontIcon extends ReactComponent<FontIconProps, FontIconProps, {}> {
  }

  declare class Paper extends ReactComponent<PaperProps, PaperProps, {}> {
  }

  declare var Styles: {
    Colors: {[key: string]: string};
  };
}
