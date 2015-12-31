/* @flow */

import React from 'react';

export type Font = { 
  name: string; 
  family: string; 
};

export type LanguageMenuOption = { 
  name: string; 
  shortcut: string; 
};

export type ToolbarControlProps = {
  displayValue?: string; 
  tooltip: string; 
  showTooltip: boolean; 
  width: number; 
  iconName?: string; 
  includeDropdownArrow?: boolean; 
};

export type MenuProps = {
  tag: string; 
  leftFontIcon?: React.Element; 
  primaryText: string; 
  value: string; 
  onTouchTap?: (e: SyntheticMouseEvent, index: number) => void; 
};