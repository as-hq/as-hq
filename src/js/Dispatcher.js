/* @flow */

import {Dispatcher} from 'flux';
import type {ASAction} from './types/Actions';

let _dispatcherInstance: Dispatcher<ASAction> = new Dispatcher();
export default _dispatcherInstance;
