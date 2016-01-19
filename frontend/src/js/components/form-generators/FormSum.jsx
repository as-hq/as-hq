/* @flow */

import type {
  Dict
} from '../../types/Base';

import type {
  FComposer
} from './types';

import React from 'react';

import ASDropdownMenu from '../basic-controls/ASDropdownMenu.jsx';

import {defaultComposer} from './util';

type FAddend = {
  tag: string;
  text: string;
  form: ReactClass;
};

function hiddenStyle(hidden: boolean) {
  return hidden ? { visibility: 'hidden' } : {};
}

export default function FSum<T>(
  {formats, menuComponent}: {
    formats: Array<FAddend>;
    menuComponent?: ReactClass;
  },
  composer?: FComposer = defaultComposer
): ReactClass {
  const defaultTag = formats[0].tag;
  const Menu = menuComponent ? menuComponent : ASDropdownMenu;

  const getFormByTag = (tag) => {
    return formats.filter(({tag: t}) => tag === t)[0].form;
  };

  const defaultValue = () => {
    let ret = {tag: defaultTag};
    const {form} = formats[0];
    if (form.defaultValue) {
      ret = {...form.defaultValue(), ...ret};
    }

    return ret;
  };

  let ret = React.createClass({
    getValueLink(tag: string): ReactLink {
      const {
        valueLink: {
          value: extValue,
          requestChange: extRequestChange
        }
      } = this.props;
      const currentSelectionTag = extValue ? extValue.tag : defaultTag;

      if (currentSelectionTag !== tag) {
        throw new Error('This form should not be shown');
      }

      return ({
        value: extValue,
        requestChange(newVal: any) {
          extRequestChange({
            ...getFormByTag(tag).defaultValue(),
            ...newVal,
            tag: tag
          });
        }
      });
    },

    getMenuValueLink(): ReactLink {
      const {
        valueLink: {
          value: extValue,
          requestChange: extRequestChange
        }
      } = this.props;
      const tag = extValue ? extValue.tag : defaultTag;

      return ({
        value: tag,
        requestChange(newVal: any) {
          extRequestChange({
            ...getFormByTag(newVal).defaultValue(),
            tag: newVal
          });
        }
      })
    },

    render(): React.Element {
      const {
        valueLink: {
          value: extValue,
          requestChange: extRequestChange
        }
      } = this.props;

      const mappedItems = formats.map(
        ({tag, text}) => ({ payload: tag, text: text })
      );

      return composer([
        {
          key: '$menu',
          element: <Menu menuItems={mappedItems} valueLink={this.getMenuValueLink()} />
        },
        ...formats
          .filter(({tag}) => extValue ? (tag === extValue.tag) : (tag === defaultTag))
          .map(({tag, form: FormElement}) =>
            ({
              key: tag,
              element: <FormElement key={tag} valueLink={this.getValueLink(tag)} />
            })
          )
      ]);
    }
  });

  ret.defaultValue = defaultValue;

  return ret;
}
