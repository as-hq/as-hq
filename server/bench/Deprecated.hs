module Deprecated where

----------------------------------------------------------------------------------------------------------------------
-- FFI

foreign import ccall unsafe "hiredis/redis_db.c getCells" c_getCells :: CString -> CInt -> IO (Ptr CString)
foreign import ccall unsafe "hiredis/redis_db.c setCells" c_setCells :: CString -> CInt -> IO ()
foreign import ccall unsafe "hiredis/redis_db.c clearSheet" c_clearSheet :: CString -> IO ()


getCellsInSheet :: Connection -> ASSheetId -> IO [ASCell]
getCellsInSheet conn sid = DI.getCellsByKeyPattern conn $ "I/" ++ (T.unpack sid) ++ "/(*,*)"

getAllCells :: Connection -> IO [ASCell]
getAllCells conn = DI.getCellsByKeyPattern conn "I/*/(*,*)"

getCellsByKeys :: [B.ByteString] -> IO [Maybe ASCell]
getCellsByKeys keys = getCellsByMessage msg num
  where
    msg      = B.concat $ [BC.pack "\"", internal, BC.pack "\"\NUL"]
    internal = B.intercalate (BC.pack msgPartDelimiter) keys
    num      = length keys

-- takes a message and number of locations queried
getCellsByMessage :: B.ByteString -> Int -> IO [Maybe ASCell]
getCellsByMessage msg num = do
  ptrCells <- BU.unsafeUseAsCString msg $ \str -> c_getCells str (fromIntegral num)
  cCells   <- peekArray (fromIntegral num) ptrCells
  res      <- mapM cToASCell cCells
  --free ptrCells
  return res

getCellsByKeyPattern :: Connection -> String -> IO [ASCell]
getCellsByKeyPattern conn pattern = runRedis conn $ do
  Right locKeys <- keys . BC.pack $ pattern
  catMaybes <$> (liftIO $ getCellsByKeys locKeys)

setCellsByMessage :: B.ByteString -> Int -> IO ()
setCellsByMessage msg num = BU.unsafeUseAsCString msg $ \lstr -> c_setCells lstr (fromIntegral num)

cToASCell :: CString -> IO (Maybe ASCell)
cToASCell str = do
  str' <- peekCString str
  let str'' = read ('"':str' ++ ['"']) -- Need to unescape the string (what's passed to Redis was escaped by Bytestring.show)
  return $ case str'' of
    "Nothing" -> Nothing
    otherwise -> Just (read2 str'' :: ASCell)

getSheetLocsRedis :: ASSheetId -> Redis [B.ByteString]
getSheetLocsRedis sheetid = do
  Right keys <- smembers $ makeSheetSetKey sheetid
  return keys

-- deletes the sheet only, does not remove from any containing workbooks
deleteSheetUnsafe :: Connection -> ASSheetId -> IO ()
deleteSheetUnsafe conn sid = do
    runRedis conn $ do
        let setKey = makeSheetSetKey sid
            sheetKey = makeSheetKey sid

        mlocKeys <- smembers setKey
        TxSuccess _ <- multiExec $ do
            case mlocKeys of
                Right []      -> return () -- hedis can't delete empty list
                Right locKeys -> del locKeys >> return ()
                Left _        -> return ()
            del [setKey]      -- delete the loc set
            del [sheetKey]    -- delete the sheet
            srem "sheetKeys" [sheetKey] -- remove the sheet key from the set of sheets
        return ()

deleteWorkbookSheet :: Connection -> WorkbookSheet -> IO ()
deleteWorkbookSheet conn wbs = do
  let delSheets = map sheetId $ wsSheets wbs
  mapM_ (deleteSheetUnsafe conn) delSheets
  wbResult <- getWorkbook conn $ wsName wbs
  case wbResult of
    (Just wb) -> modifyWorkbookSheets conn deleteSheets (workbookName wb)
      where deleteSheets = filter $ \s -> not $ s `elem` delSheets
    Nothing -> return ()

-- note: this is an expensive operation
deleteWorkbookAndSheets :: Connection -> String -> IO ()
deleteWorkbookAndSheets conn name = do
    mwb <- getWorkbook conn name
    case mwb of
        Nothing -> return ()
        Just wb -> do
            mapM_ (deleteSheetUnsafe conn) (workbookSheets wb) -- remove sheets
            runRedis conn $ do
                let workbookKey = makeWorkbookKey name
                TxSuccess _ <- multiExec $ do
                    del [workbookKey]   -- remove workbook from key-value
                    srem "workbookKeys" [workbookKey] -- remove workbook from set
                return ()
