/* @flow */

export type FChunk = { key?: string; element: ReactElement; };

export type FComposer = (vals: Array<FChunk>) => ReactElement;
