/* @flow */

import React from 'react';

export default function HOPropInsert(
  Component: ReactClass,
  props: any
): ReactClass {
  return (otherProps: any) => {
    return <Component {...otherProps} {...props} />;
  };
}
