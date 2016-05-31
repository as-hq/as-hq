/* @flow */

import type { Callback } from '../../types/Base';
import type { StoreToken } from 'flux';
import _ from 'lodash';

import React from 'react';
import { Styles } from 'material-ui';

// $FlowFixMe
import IconMenu from 'material-ui/lib/menus/icon-menu';
// $FlowFixMe
import MenuItem from 'material-ui/lib/menus/menu-item';
// $FlowFixMe
import IconButton from 'material-ui/lib/icon-button';
// $FlowFixMe
import MoreVertIcon from 'material-ui/lib/svg-icons/navigation/more-vert';

type Props = {
  title: string;
  active: boolean;
  value: any;
  menuItems: any; // XXX
  mutable: boolean;
  onSelect?: () => void;
  onTitleChange?: (name: string) => void;
  onDelete?: () => void;
};

type State = {
  isEditing: boolean;
  hovered: boolean;
};

class Tab extends React.Component {
  props: Props;
  state: State;
  _titleField: any;

  constructor(props: Props) {
    super(props);
    this.state = {
      isEditing: false,
      hovered: false,
    };
  }

  render(): React.Element {
    const styles = getStyles(this.props, this.state);

    const { isEditing } = this.state;
    const {
      title,
      active,
      value,
      mutable,
      onSelect,
      onTitleChange,
      menuItems,
    } = this.props;

    const titleContent = isEditing && mutable?
      <div ref={elem => this._titleField = elem}
           contentEditable={true}
           onKeyDown={e => this._onTitleChange(e)}
           >
        {title}
      </div>
      :
      title
      ;

    if (isEditing) {
      setTimeout(() => {
        this._titleField.focus();
        setTimeout(() =>
          document.execCommand('selectAll', false, null)
        );
      }, 100);
    }

    const allMenuItems = {
      Rename: {
        disabled: ! mutable,
        action: () => this.setState({isEditing: true})
      },
      ...menuItems
    };

    return (
      <div style={styles.root}
           onClick={() => this._onClick()}
           onMouseEnter={() => this.setState({hovered: true})}
           onMouseLeave={() => this.setState({hovered: false})}>
        <div style={styles.leftFin} />

        <div style={styles.tabTitle}
             onDoubleClick={() => this.setState({isEditing: true})}
             >
          {titleContent}
        </div>

        <div style={styles.rightFin} />

        <IconMenu
          onTouchTap={(e) => e.stopPropagation()}
          style={styles.menuIcon}
          menuStyle={styles.menu}
          iconButtonElement={<IconButton><MoreVertIcon /></IconButton>}
          anchorOrigin={{horizontal: 'left', vertical: 'bottom'}}
          targetOrigin={{horizontal: 'left', vertical: 'bottom'}}>

          {_.map(allMenuItems, (v: any, k: string) => {
            const { disabled, action } = v;
            return (
              <MenuItem primaryText={k}
                        disabled={disabled}
                        onTouchTap={() => action(value)} />
            );
          })
          }
        </IconMenu>

      </div>
    )

  }

  _onTitleChange(e: SyntheticKeyboardEvent) {
    if (e.which === 13) {
      const { onTitleChange } = this.props;
      const newTitle = this._titleField.textContent;
      this.setState({isEditing: false});
      if (onTitleChange) {
        onTitleChange(newTitle);
      }
    }
  }

  _onClick() {
    const { active, onSelect, value } = this.props;
    if (! active && onSelect) {
      onSelect(value);
    }
  }
}

function getStyles(props: Props, state: State): any {
  const { active } = props;
  const { hovered } = state;
  const color = hovered ? colors.hover : (
    active ? colors.active : colors.inactive
  );
  const zIndex = active ? 100 : 10;
  const finWidth = 8;
  const finHeight = 30;
  const tabSpacing = 3;

  return {
    root: {
      position: 'relative', // NOTE: this ensures z-indices work properly
      display: 'inline-block',
      background: color,
      width: 'fitContent',
      height: '100%',
      textAlign: 'center',
      borderRadius: '0 0 16px 16px',
      marginLeft: tabSpacing,
      marginRight: tabSpacing,
      cursor: 'pointer',
      zIndex,
    },
    tabTitle: {
      position: 'relative',
      top: 5,
      marginLeft: 10,
      display: 'inline-block',
      fontWeight: 'bold',
      color: 'white',
    },
    menuIcon: {
      position: 'relative',
      display: 'inline-block',
      float: 'right',
      transform: 'translateY(-20%)', // vertically center
      zIndex: 100000,
      padding: 0,
    },
    menu: {
      backgroundColor: color,
    },
    leftFin: {
      borderColor: `transparent ${color} transparent transparent`,
      borderStyle: 'solid',
      borderWidth: `0 ${finWidth}px ${finHeight}px 0`,
      height: 0,
      width: 0,
      float: 'left',
      display: 'inline-block',
      position: 'relative',
      marginLeft: -1 * finWidth + 1,
      zIndex,
    },
    rightFin: {
      borderColor: `${color} transparent transparent transparent`,
      borderStyle: 'solid',
      borderWidth: `${finHeight}px ${finWidth}px 0 0`,
      height: 0,
      width: 0,
      float: 'right',
      display: 'inline-block',
      position: 'relative',
      marginRight: -1 * finWidth + 1,
      zIndex,
    },
  };
}

const colors = {
  hover: '#9E9E9E',
  active: '#9E9E9E',
  inactive: '#616161',
};

export default Tab;
