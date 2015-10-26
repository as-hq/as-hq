import Util from '../AS/Util.js'

export default {

/**************************************************************************************************************************/
  /* Type specs */

  // ASLocation
  // {tag: index/range, index/range: <simpleLocation>, sheetId: ?}

  // ALL SIMPLELOCATIONS ARE RANGES
  // simpleLocation
  // {tl: {row: ?, col: ?}, br: {row: ?, col: ?}}


/**************************************************************************************************************************/
  /* Manual type checks */

  isIndex(simpleLocation) {
    return (simpleLocation.tl.row === simpleLocation.br.row) &&
           (simpleLocation.tl.col === simpleLocation.br.col);
  }



}
