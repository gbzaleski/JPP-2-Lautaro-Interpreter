
module Main where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import System.IO          ( hPutStrLn, stderr )
import System.Environment ( getArgs, getProgName )
import System.Exit        ( exitFailure, exitSuccess )

import Lautaro.Lex     ( Token )
import Lautaro.Par     ( pProgramme, myLexer )
import Lautaro.Print   ( Print, printTree )
import TypeChecker     ( runTypeCheck )
import Interpreter     ( runProgramme )

runFile :: FilePath -> IO()
runFile f = readFile f >>= runProgram

runProgram :: String -> IO ()
runProgram s = case pProgramme tokenised of
    Left parseError -> do
      hPutStrLn stderr $ "Programme parsing failure!\n" ++ parseError
      exitFailure
    Right programmeTree -> do
      let staticAnalysisResult = runTypeCheck programmeTree
      case staticAnalysisResult of
        Left staticError -> do 
          hPutStrLn stderr $ "Static analysis failure!\n" ++ staticError
          exitFailure
        Right _ -> runProgramme programmeTree
  where
    tokenised = myLexer s

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= runProgram
    fs -> mapM_ runFile fs
