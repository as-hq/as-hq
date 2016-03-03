/* @flow */

export type EditorSelection = {
  range: AEWordRange;
  backwards: boolean;
};

export type EditorIdentifier = 'TOP_EDITOR' | 'GRID_EDITOR';

export type EditorSelectionSource = 'text' | 'nav';
