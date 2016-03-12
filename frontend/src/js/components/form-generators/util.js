/* @flow */

import type {FChunk, FComposer} from './types';

import React from 'react';

export const defaultComposer: FComposer =
  (vals: Array<FChunk>): React.Element => {
    return <div>{
      vals.map(({element}) => element)
    }</div>;
  };

export function get(vals: Array<FChunk>, key: string): React.Element {
  const matchingVals = vals.filter(({key: k}) => key === k)

  if (matchingVals.length > 0) {
    return matchingVals[0].element;
  } else {
    throw new Error('Tried to get an invalid element');
  }
}
