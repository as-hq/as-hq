/* @flow */

import React from 'react';

import FObject from './FormObject.jsx';

export default function FormAddendLift({tag, text, form}: {
  tag: string;
  text: string;
  form: ReactClass;
}): {tag: string; text: string; form: ReactClass} {
  return {
    tag: tag,
    text: text,
    form: FObject({
      tag: tag,
      format: [
        { key: 'contents', form: form }
      ]
    })
  };
}
