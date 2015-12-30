/* @flow */

import type { 
  ASCell, 
  ASLocation, 
  ASCellProp
} from'./Eval';


export type Bar = {
  tag: 'Bar';
  barIndex: BarIndex;
  barProps: Array<BarProp>;
};

export type BarIndex = {
  tag: 'BarIndex';
  barSheetId: string;
  barType: BarType;
  barNumber: number;
}

export type BarType = 'ColumnType' | 'RowType';

export type BarProp = Dimension | FromCellProp;

export type Dimension = {
  tag: 'Dimension';
  contents: number;
};

export type FromCellProp = {
  tag: 'FromCellProp';
  contents: ASCellProp;
};
