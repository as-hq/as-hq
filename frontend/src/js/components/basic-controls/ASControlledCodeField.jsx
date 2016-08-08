/* @flow */

import type {
  Callback
} from '../../types/Base';

import React from 'react';
import U from '../../AS/Util';

import ASCodeField from './ASCodeField.jsx';
import HOPurify from '../transforms/HOPurify.jsx';

/**
 * A purified codefield component.
 * will have ReactLinks for its 'selection' and 'text' props
 */
const ASControlledCodeField = HOPurify({
  component: ASCodeField,
  purifiers: {
    selection: {
      manuallySilenced: true,

      addChangeListener({component, listener, locker}) {

        component.editor.selection.lead.on('alphasheets-selection-change', () => {

          /**
           * The event timeline for Ace is:
           *
           * keypress --> selection changed --> text changed
           * 							 |           |        |          |
           * 							 --> store -->        --> store ->
           *        						 			 ^                   ^
           *  do not modify editor's text		     modify text
           *
           * So, lock the internal text state until our listener is finished.
           */

          locker.lock('text');
          listener({eventSource: 'textchange'});
          locker.unlock('text');
        });

        component.editor.selection.anchor.on('alphasheets-selection-change', () => {
          // see diagram above.
          locker.lock('text');
          listener({eventSource: 'textchange'});
          locker.unlock('text');
        });

        component.editor.selection.on('alphasheets-selection-change', () => {
          // see diagram above.
          locker.lock('text');

          if (component.editor.$mouseHandler.isMousePressed) {
            listener({eventSource: 'mousenav'});
          } else {
            const key = component.editor.textInput.$lastKeyDown;

            if (U.Key.isNavKey(key)) {
              listener({eventSource: 'keynav'})
            } else {
              listener({eventSource: 'textchange'});
            }
          }

          locker.unlock('text');
        });
      },

      getValue(component: any) {
        return component.editor.getSelectionObject();
      },

      setValue({component, value: selection}) {
        component.editor.setSelectionObject(selection);
      },
    },

    text: {
      manuallySilenced: true,

      addChangeListener({component, listener}) {
        component.editor.on('alphasheets-text-change', (e) => listener());
        component.editor.on('alphasheets-paste', () => listener());
        component.editor.on('alphasheets-cut', () => listener());
      },

      getValue(component: any) {
        return component.editor.getValue();
      },

      setValue({component, value: text}) {
        component.editor.setValueSilently(text);
      },
    }
  }
});

export default ASControlledCodeField;
