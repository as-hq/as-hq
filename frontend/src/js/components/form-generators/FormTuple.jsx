/* @flow */

import type {
  FComposer
} from './types';

import React from 'react';
import {defaultComposer} from './util';

type FTupTerm = {
  form: ReactClass;
};

export default function FormTuple(
  {tag, format}: {
    tag?: string;
    format: Array<FTupTerm>;
  },
  composer?: FComposer = defaultComposer
): ReactClass {
  const defaultValue = () => {
    let ret = {tag, contents: []};
    format.forEach(({form}, idx) => {
      if (form.defaultValue) {
        ret.contents[idx] = form.defaultValue();
      }
    });
    return ret;
  };

  let ret = React.createClass({
    getValueLink(idx: number): ReactLink {
      const self = this;
      const {
        valueLink: {
          value: extValue,
          requestChange: extRequestChange
        }
      } = this.props;

      const subValue =
        (extValue && extValue.contents) ? extValue.contents[idx] : null;

      return ({
        value: subValue,
        requestChange(newVal: any) {
          let nextValue = extValue || defaultValue();
          nextValue.contents[idx] = newVal;
          extRequestChange({ ...nextValue, tag: tag });
        }
      });
    },

    render(): React.Element {
      return composer(
        format.map(({form: FormElement}, idx) => ({
          element: <FormElement key={idx} valueLink={this.getValueLink(idx)} />
        }))
      );
    }
  });

  ret.defaultValue = defaultValue;
  return ret;
}
