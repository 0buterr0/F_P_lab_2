{-# LANGUAGE CPP #-}

-- | Лабораторна робота №2. Багатопотокове стиснення множини файлів
--   в один архів алгоритмом Хаффмана (GHC -threaded).
module Main (main) where

import Control.Exception (SomeException, catch, displayException, fromException)
import GHC.Conc (getNumProcessors, setNumCapabilities)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith)
import System.FilePath (takeBaseName)
import System.IO
import Text.Read (readMaybe)

import Archive
import Bench

#if defined(mingw32_HOST_OS)
import System.Win32.Console (setConsoleCP, setConsoleOutputCP)
#endif

setupConsole :: IO ()
setupConsole = do
#if defined(mingw32_HOST_OS)
  setConsoleOutputCP 65001
  setConsoleCP 65001
#endif
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  hSetBuffering stdout LineBuffering

data Opts = Opts
  { optThreads :: Maybe Int
  , optChunkKB :: Int
  , optRepeats :: Int
  , optRest    :: [String]
  }

parseOpts :: [String] -> Either String Opts
parseOpts = go (Opts Nothing 1024 3 [])
  where
    go o [] = Right o { optRest = reverse (optRest o) }
    go o (flag : v : rest)
      | flag == "-j" = num v >>= \n -> go o { optThreads = Just n } rest
      | flag == "-c" = num v >>= \n -> go o { optChunkKB = n } rest
      | flag == "-r" = num v >>= \n -> go o { optRepeats = n } rest
    go o (x : rest) = go o { optRest = x : optRest o } rest

    num v = case readMaybe v of
      Just n | n > 0 -> Right n
      _              -> Left ("Некоректне число: " ++ v)

usage :: IO a
usage = do
  mapM_ putStrLn
    [ "Багатопотоковий архіватор Хаффмана"
    , ""
    , "Використання:"
    , "  huff compress   <архів.huf> <файли або папки...> [-j потоки] [-c КБ]"
    , "  huff decompress <архів.huf> [папка] [-j потоки]"
    , "  huff list       <архів.huf>"
    , "  huff bench      <файли або папки...> [-j макс. потоків] [-r повторів] [-c КБ]"
    , "  huff gen        <папка> <кількість файлів> <розмір файлу, МБ>"
    , ""
    , "  -j  кількість потоків (за замовчуванням — кількість логічних процесорів)"
    , "  -c  розмір блоку в КБ (за замовчуванням 1024)"
    , "  -r  кількість повторів у bench, береться найкращий час (за замовчуванням 3)"
    , ""
    , "Приклад:"
    , "  cabal run huff -- gen testdata 8 16"
    , "  cabal run huff -- bench testdata"
    , "  cabal run huff -- compress test.huf testdata -j 8"
    , "  cabal run huff -- decompress test.huf out"
    ]
  exitFailure

runCommand :: String -> [String] -> IO ()
runCommand cmd args = do
  opts <- either (\e -> hPutStrLn stderr e >> usage) pure (parseOpts args)
  cores <- getNumProcessors
  let threads = maybe cores id (optThreads opts)
      chunk   = optChunkKB opts * 1024
  case (cmd, optRest opts) of
    ("compress", archive : inputs@(_ : _)) -> do
      setNumCapabilities threads
      compressFiles threads chunk archive inputs
    ("decompress", [archive]) -> do
      setNumCapabilities threads
      decompressFiles threads archive (takeBaseName archive ++ "_unpacked")
    ("decompress", [archive, outDir]) -> do
      setNumCapabilities threads
      decompressFiles threads archive outDir
    ("list", [archive]) -> listArchive archive
    ("bench", inputs@(_ : _)) -> runBenchmark threads (optRepeats opts) chunk inputs
    ("gen", [dir, cnt, size]) -> case (readMaybe cnt, readMaybe size) of
      (Just c, Just s) | c > 0 && s > 0 -> generateTestFiles dir c s
      _                                 -> usage
    _ -> usage

main :: IO ()
main = do
  setupConsole
  args <- getArgs
  case args of
    (cmd : rest) -> runCommand cmd rest `catch` handler
    []           -> usage
  where
    handler :: SomeException -> IO ()
    handler e = case fromException e of
      Just code -> exitWith code
      Nothing   -> do
        hPutStrLn stderr ("Помилка: " ++ displayException e)
        exitFailure
