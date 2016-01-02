/* @flow */

import React from 'react';

export type Font = { 
  name: string; 
  family: string; 
};

export type ToolbarControlProps = {
  displayValue?: string; 
  tooltip: string; 
  showTooltip: boolean; 
  width: number; 
  iconName?: string; 
  includeDropdownArrow?: boolean; 
};

export type MenuProps = MenuItemProps | MenuDividerProps;

export type MenuItemProps = {
  tag: "MenuItem"; 
  primaryText: string; 
  value: string; 
  onTouchTap?: (e: SyntheticMouseEvent, index: number) => void; 
  leftFontIcon?: React.Element; 
};

export type MenuDividerProps = {
  tag: "Divider"; 
  style: any;
};

