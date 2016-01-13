/* @flow */

/* NOTE:

There has been a problem in AlphaSheets code where views have too much state
that simply comes from stores. The point of this file is to ensure that we can
just do:

    componentDidMount() {
      Util.React.addStoreLinks(this, [
        { store: SelectionStore }
      ]);
    }

    componentWillUnmount() {
      Util.React.removeStoreLinks(this);
    }

and then be able to forceUpdate on render,

 */

import type {
  Callback,
  Dict
} from '../../types/Base';

import type {
  StoreLink
} from '../../types/React';

import React from 'react';

import BaseStore from '../../stores/BaseStore';

type ExtReactComponent = ReactComponent & {
  $storeLinks: Array<StoreLink>
};

const ASReactUtils = {
  addStoreLinks(
    component: ExtReactComponent,
    storeLinks: Array<{
      listener?: Callback;
      store: typeof BaseStore;
    }>
  ) {
    component.$storeLinks = [];
    storeLinks.forEach(({listener, store}) => {
      const fn = () => {
        component.forceUpdate();
        if (listener) { listener(); }
      };
      component.$storeLinks.push({
        store, listener: fn
      });
      store.addChangeListener(fn);
    });
  },

  removeStoreLinks(component: ExtReactComponent) {
    component.$storeLinks.forEach(({store, listener}) => {
      store.removeChangeListener(listener);
    });
    component.$storeLinks = [];
  }
};

export default ASReactUtils;
