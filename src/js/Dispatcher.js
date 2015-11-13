/* @flow */

import {Dispatcher} from 'flux';
import type {ASAction} from './Types/Actions';

let _dispatcherInstance: Dispatcher<ASAction> = new Dispatcher();
export default _dispatcherInstance;
