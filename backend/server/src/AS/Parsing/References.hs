
module AS.Parsing.References where

import AS.Prelude

import AS.Types.User
import AS.Types.Sheets
import AS.Types.Excel
import AS.Types.Locations
import AS.Types.Cell
import AS.Types.Commits

import AS.Parsing.Substitutions 

import AS.DB.Users (getOpenedSheets)

import Database.Redis (Connection)

----------------------------------------------------------------------------------------------------------------------
-- Reference extraction

-- | Returns the list of dependencies in ASExpression. 
-- #needsrefactor not all ASReferences are valid references for the graph.
getDependencies :: SheetID -> [Sheet] -> ASExpression -> [ASReference]
getDependencies sid sheets = map (convertInvalidRef . exRefToASRef sid sheets) . getExcelReferences
  where 
    convertInvalidRef r = case r of 
      TemplateRef t -> case t of 
        SampleExpr _ idx -> IndexRef idx
      _ -> r

----------------------------------------------------------------------------------------------------------------------
-- Reference conversions

-- | Turns an Excel reference to an AlphaSheets reference. (first arg is the sheet of the
-- ref, unless it's a part of the ExRef)
exRefToASRef :: SheetID -> 
                [Sheet] ->     -- list of all the user's sheets (used for matching sheet names <=> sheetIDs)
                ExRef -> 
                ASReference      -- #needsrefactor should return (Either RefError ASReference)
exRefToASRef sid sheets exRef = case exRef of
  ExOutOfBounds -> OutOfBounds

  ExIndexRef exIndex sn wn -> case sn of 
    Nothing -> IndexRef $ Index sid (exIndexToCoord exIndex)
    Just s -> case lookupSheetId sheets s wn of 
      Nothing -> OutOfBounds -- #needsrefactor better error message than this
      Just sid' -> IndexRef $ Index sid' (exIndexToCoord exIndex)

  ExRangeRef (ExRange f s) sn wn -> case sn of 
    Nothing -> RangeRef $ Range sid (tl, br)
    Just s -> case lookupSheetId sheets s wn of 
      Nothing -> OutOfBounds
      Just sid' -> RangeRef $ Range sid' (tl, br)
    where
      tl = exIndexToCoord f
      br = extExIndexToExtCoord s

  ExPointerRef exIndex sn wn -> case sn of 
    Nothing -> PointerRef . Pointer $ Index sid (exIndexToCoord exIndex)
    Just s -> case lookupSheetId sheets s wn of 
      Nothing -> OutOfBounds
      Just sid' -> PointerRef . Pointer $ Index sid' (exIndexToCoord exIndex)

  ExTemplateRef t sn wn -> case t of 
    ExSampleExpr n coord -> case sn of 
      Nothing -> TemplateRef . SampleExpr n $ Index sid (exIndexToCoord coord)
      Just s -> case lookupSheetId sheets s wn of 
        Nothing -> OutOfBounds
        Just sid' -> TemplateRef . SampleExpr n $ Index sid' (exIndexToCoord coord)

-- #TODO (incomplete) does not consider workbookname.
lookupSheetId :: [Sheet] -> SheetName -> Maybe WorkbookName -> Maybe SheetID
lookupSheetId sheets sn _ = view sheetId <$> find ((== sn) . view sheetName) sheets
