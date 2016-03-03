/* @flow */

import type { PXRectangle } from '../../types/Render';

export default {
  defaultRectangle(): PXRectangle {
    return {
      origin: {x: 0, y: 0},
      extent: {x: 0, y: 0},
    }
  }
}
