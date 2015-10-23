import Util from '../AS/Util.js'

export default {
  //Types:

  // ASLocation
  // {tag: index/range, index/range: <simpleLocation>, sheetId: ?}

  // ALL SIMPLELOCATIONS ARE RANGES
  // simpleLocation
  // {tl: {row: ?, col: ?}, br: {row: ?, col: ?}}

  isIndex(simpleLocation) {
    return (simpleLocation.tl.row === simpleLocation.br.row) &&
           (simpleLocation.tl.col === simpleLocation.br.col);
  }
}
