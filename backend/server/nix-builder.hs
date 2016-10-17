{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Main where

import Data.Maybe

import qualified Data.Text as T

import Prelude hiding (FilePath)

import Shelly

default (T.Text)

getOut :: Sh FilePath
getOut = ( fromText
         . fromMaybe (error "$out not defined; not in a nix env")
         ) <$> get_env "out"

stackClean :: Sh ()
stackClean = run_ "stack" ["clean", "--full"]

stackBuild :: Sh ()
stackBuild = get_env "doCheck" >>= maybe withoutTest (const withTest)
    where withoutTest = run_ "stack" ["build", "alphasheets"]
          withTest    = run_ "stack" ["build", "alphasheets", "--test"]

installBin :: T.Text -> T.Text -> Sh ()
installBin opt comp = run_ "stack" [ "install"
                                   , "alphasheets:" `T.append` comp
                                   , "--local-bin-path"
                                   , opt
                                   ]

patchElf :: FilePath -> Sh ()
patchElf p = run_ "patchelf" [ "--shrink-rpath", toTextIgnore p]

main = shelly $ do
    sr <- toTextIgnore <$> absPath "./.stack-root"
    setenv "STACK_ROOT" sr
    stackClean
    stackBuild
    ob <- (</> "bin") <$> getOut
    let obt = toTextIgnore ob
    mkdir_p ob
    installBin obt "alphasheets-exe"
    installBin obt "rkernel-exe"
    patchElf $ ob </> "alphasheets-exe"
    patchElf $ ob </> "rkernel-exe"
