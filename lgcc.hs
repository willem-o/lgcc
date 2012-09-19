----
-- Licence: GPLv3
-- Author: Willem Obbens
-- Status: hardly tested, use at your own risk
-- Description: wrapper around gcc haskell-style literate programming
--              in c and c++.
----
module Main where

import Control.Applicative ((<$>), liftA2)
import Control.Arrow ((&&&), (>>>), first)
import Control.Monad (when, void, sequence, forM, forM_)
import Data.IORef (newIORef, modifyIORef, readIORef)
import Data.List (intercalate, isSuffixOf, isPrefixOf, delete)
import System.Cmd (rawSystem)
import System.Directory (doesFileExist, removeFile)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

unliterate :: String -> String
unliterate = unlines . map (drop 2) . filter (isPrefixOf "> ") . lines

isLiterate :: String -> Bool
isLiterate = or . sequence (isSuffixOf <$>
  [".lcc", ".lcpp", ".lC", ".lhh", ".lhpp", ".lc", ".lh"])

main :: IO ()
main = do
  args <- getArgs
  when (null args) help
  let (compiler, args') = (head &&& tail >>> first matchCompiler) args
      compilers = [("c", "gcc"), ("cpp", "g++")]
      matchCompiler = maybe "" id . flip lookup compilers
  when (null compiler) help
  tempFiles <- newIORef []
  newArgs <- forM args'
    (\arg ->
      if isLiterate arg
        then do
          e <- doesFileExist arg
          when (not e) (reportError (arg ++ " does not exist."))
          let (front, suffix) = break (== '.') arg
              newArg = front ++ delete 'l' suffix
          e' <- doesFileExist newArg
          when e' (reportError (newArg ++ " already exists."))
          unliterate <$> readFile arg >>= writeFile newArg
          modifyIORef tempFiles (newArg:)
          return newArg
        else return arg)
  putStrLn (compiler ++ intercalate " " newArgs)
  void (rawSystem compiler newArgs)
  readIORef tempFiles >>=
    mapM_ (liftA2 (>>=) doesFileExist (flip when . removeFile))
  where reportError msg = hPutStrLn stderr msg >> exitFailure
        help = do
          pn <- getProgName
          putStrLn (pn ++ " <c|cpp> [compiler args]")
          exitFailure