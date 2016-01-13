/* @flow */

import type {
  FComposer
} from './types';

import React from 'react';
import {defaultComposer} from './util';

type FObjTerm = {
  key: string;
  form: ReactClass;
};

export default function FormObject(
  {tag, format}: {
    tag?: string;
    format: Array<FObjTerm>;
  },
  composer?: FComposer = defaultComposer
): ReactClass {
  const defaultValue = () => {
    let ret = {tag};
    format.forEach(({key, form}) => {
      if (form.defaultValue) {
        ret[key] = form.defaultValue();
      }
    });
    return ret;
  };

  let ret = React.createClass({
    getValueLink(name: string): ReactLink {
      const self = this;
      const {
        valueLink: {
          value: extValue,
          requestChange: extRequestChange
        }
      } = this.props;

      const subValue = extValue ? extValue[name] : null;

      return ({
        value: subValue,
        requestChange(newVal: any) {
          extRequestChange({
            ...defaultValue(), ...extValue, tag: tag, [name]: newVal
          });
        }
      });
    },

    render(): React.Element {
      return composer(
        format.map(({key, form: FormElement}) => ({
          key,
          element:
            <FormElement key={key} valueLink={this.getValueLink(key)} />
        }))
      );
    }
  });

  ret.defaultValue = defaultValue;
  return ret;
}
