/* @flow */

import React from 'react';

export type FChunk = { key?: string; element: React.Element; };

export type FComposer = (vals: Array<FChunk>) => React.Element;
