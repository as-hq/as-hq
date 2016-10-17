#!/usr/bin/env nix-shell
#!nix-shell --pure -i "runhaskell -no-user-package-db" -p nix which coreutils acbuild "haskell.packages.ghc801.ghcWithPackages (pkgs: [pkgs.async pkgs.shelly pkgs.text])"

{-# LANGUAGE ExtendedDefaultRules
           , OverloadedStrings
           #-}

module Main where

import Control.Concurrent.Async

import Control.Monad

import Data.Monoid

import qualified Data.Text as T

import GHC.IO.Encoding

import Prelude hiding (FilePath)

import Shelly

import System.Environment
import System.Exit

default (T.Text)

nixBuildAttr :: T.Text   -- ^ Nix attribute to derive.
             -> FilePath -- ^ Result link path.
             -> Sh ()
nixBuildAttr at lp = run_ "nix-build" ["<nixpkgs>", "-o", lpt, "-A", at]
    where lpt = toTextIgnore lp

nixBuildPackageFile :: FilePath -- ^ Nix expression path.
                    -> FilePath -- ^ Result link path.
                    -> Sh ()
nixBuildPackageFile ep lp = run_ "nix-build" ["-o", lpt, "-E", nxp]
    where lpt = toTextIgnore lp
          nxp = "with import <nixpkgs> {}; callPackage (import "
             <> (toTextIgnore ep)
             <> ") {}"

buildASBackend :: Sh ()
buildASBackend = nixBuildPackageFile
    "alphasheets-backend/alphasheets-backend.nix"
    "alphasheets-backend-out"

buildASLibs :: Sh ()
buildASLibs = nixBuildPackageFile "as-libs/as-libs.nix" "as-libs-out"

buildFileInputHandler :: Sh ()
buildFileInputHandler = nixBuildPackageFile
    "file-input-handler/file-input-handler.nix"
    "file-input-handler-out"

buildRedis :: Sh ()
buildRedis = nixBuildAttr "redis" "redis-out"

buildNixThings :: Sh ()
buildNixThings = do
    asb <- asyncSh buildASBackend
    asl <- asyncSh buildASLibs
    fih <- asyncSh buildFileInputHandler
    rds <- asyncSh buildRedis
    liftIO $ do wait asb
                wait asl
                wait fih
                wait rds

setupNixEnv :: IO ()
setupNixEnv = do
    setLocaleEncoding utf8
    as <- getArgs
    case as of [np] -> setEnv "NIX_PATH" np
               _    -> die "usage:   ./build-conainers nix-path\n\
                           \example: ./build-containers $NIX_PATH"

acbuild :: T.Text   -- ^ Container name.
        -> FilePath -- ^ Container image.
        -> FilePath -- ^ Nix root.
        -> FilePath -- ^ Entry point.
        -> Sh ()
acbuild n p r e = do
    run_ "acbuild" ["begin"]
    run_ "acbuild" ["label", "add", "arch", "amd64"]
    run_ "acbuild" ["label", "add", "os", "linux"]
    run_ "acbuild" ["set-name", n]
    (T.lines <$> run "nix-store" ["-qR", toTextIgnore r]) >>= mapM accopy
    ep <- run "realpath" [toTextIgnore e]
    run_ "acbuild" ["set-exec", ep]
    run_ "acbuild" ["write", toTextIgnore p]
    where accopy a = run_ "acbuild" ["copy", a, a]

-- | Can't do this concurrently because acbuild uses global state :(
buildContainers :: Sh ()
buildContainers = sequence_ [ acbuild "alphasheets-backend"
                                      "alphasheets-backend.aci"
                                      "alphasheets-backend-out"
                                      "alphasheets-backend-out/bin/alphasheets-exe"
                            , acbuild "rkernel"
                                      "rkernel.aci"
                                      "alphasheets-backend-out"
                                      "alphasheets-backend-out/bin/rkernel-exe"
                            , acbuild "pykernel"
                                      "pykernel.aci"
                                      "as-libs-out"
                                      "as-libs-out/lib/python2.7/site-packages/AS/kernel/server.py"
                            , acbuild "file-input-handler"
                                      "file-input-handler.aci"
                                      "file-input-handler-out"
                                      "file-input-handler-out/bin/file-input-handler.py"
                            , acbuild "redis"
                                      "redis.aci"
                                      "redis-out"
                                      "redis-out/bin/redis-server"
                            ]

main :: IO ()
main = (setupNixEnv >>) $ shelly $ do
    buildNixThings
    run_ "rm" ["-rf", ".acbuild"]
    finally_sh (run_ "rm" ["-rf", ".acbuild"]) buildContainers
