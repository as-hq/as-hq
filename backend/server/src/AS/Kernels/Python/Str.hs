module AS.Kernels.Python.Str(str) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

str = QuasiQuoter { quoteExp  = stringE
                  , quotePat  = error "quotePat referenced in AS.Kernels.Python.Str" 
                  , quoteType = error "quoteType referenced in AS.Kernels.Python.Str"
                  , quoteDec  = error "quoteDec referenced in AS.Kernels.Python.Str" }