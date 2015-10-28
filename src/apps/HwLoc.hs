{-# LANGUAGE LambdaCase #-}

module Main where

import System.Hwloc
import Numeric (showHex)

main :: IO ()
main = do
   putStrLn $ "Using HWLOC version " ++ show (showHex getApiVersion "")

   Just topo <- initTopology

   _ <- loadTopology topo

   root <- getObject topo 0 0

   putStrLn (show root)
