----
-- Licence: GPLv3
-- Author: Willem Obbens
-- Status: hardly tested, use at your own risk
-- Description: wrapper around gcc haskell-style literate programming
--              in c and c++.
----
module Main where

import Data.List (intercalate, isSuffixOf, isPrefixOf, delete)
import Control.Applicative ((<$>))
import Control.Monad (when, void)
import Control.Arrow ((&&&), (>>>), first)
import Data.Traversable (sequenceA, forM)
import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.Cmd (rawSystem)
import System.IO (hPutStrLn, stderr)

unliterate :: String -> String
unliterate = unlines . map (drop 2) . filter (isPrefixOf "> ") . lines

isLiterate :: String -> Bool
isLiterate = or . sequenceA (isSuffixOf <$>
  [".lcc", ".lcpp", ".lC", ".lhh", ".lhpp", ".lc", ".lh"])

main :: IO ()
main = do
  args <- getArgs
  when (null args) help
  let (comp, args') = (head &&& tail >>> first matchComp) args
      matchComp s
        | s == "c" || s == "cpp" = s
        | otherwise = ""
  when (null comp) help
  newArgs <- forM args'
    (\arg -> do
        if isLiterate arg
          then do
            e <- doesFileExist arg
            when (not e) (reportError (arg ++ " does not exist."))
            let (front, suffix) = break (== '.') arg
                newArg = front ++ delete 'l' suffix
            e' <- doesFileExist newArg
            when e' (reportError (newArg ++ " already exists."))
            unliterate <$> readFile arg >>= writeFile newArg
            return newArg
          else return arg)
  putStrLn (comp ++ intercalate " " newArgs)
  void (rawSystem comp newArgs)
  where reportError msg = hPutStrLn stderr msg >> exitFailure
        help = do
          pn <- getProgName
          putStrLn (pn ++ " <c|cpp> [compiler args]")
          exitFailure
                                      
