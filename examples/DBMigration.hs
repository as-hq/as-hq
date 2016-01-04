-- example for migration of ASCell
-- (will not compile b/c of imports)

import Data.SafeCopy

-- Here, we're going to migrate the ASCell type. The general idea is that
-- you retain the old type, appending a version number to avoid name conflicts.
-- the version number can be anything, so long as it's unique. 

-- declare the old ASCell type
data ASCell0 = Cell0 { _cellLocation :: ASIndex
                   , _cellExpression :: ASExpression
                   , _cellValue :: ASValue
                   , _cellProps :: ASCellProps } 

-- generate a serialization instance for the old type, specifying:
-- (1) the type version
-- (2) whether the type is a base or an extension of another type
-- (3) the type name
deriveSafeCopy 1 'base ''ASCell0

-- declare the new ASCell type
data ASCell = Cell { _cellLocation :: ASIndex
                   , _cellExpression :: ASExpression
                   , _cellValue :: ASValue
                   , _cellProps :: ASCellProps
                   , _cellRangeKey :: Maybe RangeKey } 

-- generate a serialization instance for the new type, specifying 
-- that it is an extension of another type, and therefore might need to migrate during deserialization
deriveSafeCopy 2 'extension ''ASCell

-- Now, specify which types the new type can be migrated from, 
-- as well as how to migrate them.
-- Note that you never call the "migrate" function manually. 
instance Migrate ASCell where
  type MigrateFrom ASCell
  migrate (Cell0 l e v p) = Cell l e v Nothing -- "Nothing" might not actually be right, depending on the version of ASExpression :)
-- conceptually, this specifies a surjective function from all old 
-- type version to the new one. So during serialization of _either_ 
-- ASCell or ASCell0, "migrate" is called if needed, and the end 
-- result is a deserializaed ASCell. 
-- In a sense, this is lazy migration -- old types get migrated to the
-- new one as and when "decode" is called on them.
