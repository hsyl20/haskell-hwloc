{-# LANGUAGE LambdaCase #-}

module Main where

import System.Hwloc
import Numeric (showHex)
import Data.Tree
import Control.Monad

main :: IO ()
main = do
   putStrLn $ "Using HWLOC version " ++ showHex getApiVersion ""

   Just topo <- initTopology

   _ <- loadTopology topo

   root <- getObject topo 0 0
   tree <- buildTree root

   putStrLn . drawTree . fmap (show . objectType) $ tree

   putStrLn . drawTree . fmap show $ tree

buildTree :: Object -> IO (Tree Object)
buildTree root = do
   Node root <$> forM (objectChildren root) (buildTree <=< peekObject)
