{-# LANGUAGE BangPatterns #-}

-- | Вимірювання ефективності (час / прискорення / ефективність) і генерація тестових даних.
module Bench
  ( runBenchmark
  , generateTestFiles
  ) where

import Control.Monad (forM, forM_, unless, when)
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed (UArray, listArray)
import Data.Bits (shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import Data.Word (Word64, Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)
import GHC.Clock (getMonotonicTime)
import GHC.Conc (getNumProcessors, setNumCapabilities)
import Numeric (showFFloat)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Mem (performGC)

import Archive

-- | Найкращий час з r повторів.
bestOf :: Int -> IO () -> IO Double
bestOf r act = minimum <$> forM [1 .. max 1 r] (\_ -> do
  performGC
  t0 <- getMonotonicTime
  act
  t1 <- getMonotonicTime
  pure (t1 - t0))

fmt :: Int -> Double -> String
fmt d x = showFFloat (Just d) x ""

-- | Стиснення й розпакування тих самих даних на 1..maxThreads потоках.
runBenchmark :: Int -> Int -> Int -> [FilePath] -> IO ()
runBenchmark maxThreads repeats chunkSize inputs = do
  files <- collectFiles inputs
  when (null files) (ioError (userError "не знайдено файлів для тесту"))
  contents <- forM files (\(name, path) -> (,) name <$> BS.readFile path)
  cores <- getNumProcessors
  let total = sum (map (BS.length . snd) contents)
  putStrLn "=== Вимірювання ефективності багатопотокового стиснення Хаффмана ==="
  putStrLn ("Логічних процесорів у системі: " ++ show cores)
  putStrLn ("Файлів: " ++ show (length files) ++ ", обсяг: " ++ fmtMB total
            ++ ", блок: " ++ show (chunkSize `div` 1024) ++ " КБ, повторів: " ++ show repeats)

  -- Перевірка коректності: розпаковане має збігатися з початковим.
  setNumCapabilities maxThreads
  (entries, st) <- compressData maxThreads chunkSize contents
  restored <- decompressData maxThreads entries
  let ok       = map snd restored == map snd contents
      compSize = sum [ BS.length (ciData c) | fe <- entries, c <- feChunks fe ]
  putStrLn ("Блоків (паралельних завдань): " ++ show (csChunks st))
  putStrLn ("Ступінь стиснення: " ++ fmt 1 (100 * fromIntegral compSize / fromIntegral (max 1 total))
            ++ " % від початкового розміру")
  putStrLn ("Перевірка (розпаковане == початкове): " ++ if ok then "OK" else "ПОМИЛКА")
  unless ok (ioError (userError "розпаковані дані не збігаються з початковими"))
  putStrLn ""

  results <- forM [1 .. maxThreads] $ \j -> do
    setNumCapabilities j
    tc <- bestOf repeats (compressData j chunkSize contents >> pure ())
    td <- bestOf repeats (decompressData j entries >> pure ())
    putStrLn ("  " ++ show j ++ " потік(и): стиснення " ++ fmtSec tc ++ ", розпакування " ++ fmtSec td)
    pure (j, tc, td)
  setNumCapabilities maxThreads

  case results of
    [] -> pure ()
    ((_, tc1, td1) : _) -> do
      let line j tc td =
            [ show j, fmt 3 tc, fmt 2 (tc1 / tc), fmt 2 (tc1 / tc / fromIntegral j)
            , fmt 3 td, fmt 2 (td1 / td), fmt 2 (td1 / td / fromIntegral j) ]
          header = [ "Потоки", "Стисн., с", "Приск.", "Ефект.", "Розп., с", "Приск.", "Ефект." ]
          rows   = [ line j tc td | (j, tc, td) <- results ]
          widths = [8, 11, 8, 8, 11, 8, 8]
          render cs = concat (zipWith pad widths cs)
      putStrLn ""
      putStrLn (render header)
      putStrLn (replicate (sum widths) '-')
      mapM_ (putStrLn . render) rows
      let csv = "threads,compress_s,compress_speedup,compress_efficiency,decompress_s,decompress_speedup,decompress_efficiency"
                : [ concatMap (++ ",") (init r) ++ last r | r <- rows ]
      writeFile "bench.csv" (unlines csv)
      putStrLn ""
      putStrLn "Прискорення S(p) = T(1) / T(p), ефективність E(p) = S(p) / p."
      putStrLn "Результати збережено у bench.csv."

---------------------------------------------------------------------
-- Генерація тестових файлів (текст із частотами букв англійської мови)
---------------------------------------------------------------------

letterWeights :: [(Char, Int)]
letterWeights =
  [ (' ', 180), ('e', 102), ('t', 73), ('a', 65), ('o', 61), ('i', 56), ('n', 55)
  , ('s', 51), ('h', 49), ('r', 48), ('d', 34), ('l', 32), ('c', 22), ('u', 22)
  , ('m', 19), ('w', 19), ('f', 18), ('g', 16), ('y', 16), ('p', 15), ('b', 12)
  , ('\n', 10), ('v', 8), (',', 8), ('.', 8), ('k', 6), ('x', 2), ('j', 1)
  , ('q', 1), ('z', 1) ]

-- | 4096 позицій, кожен символ займає частку, пропорційну його вазі.
letterTable :: UArray Int Word8
letterTable = listArray (0, 4095) (take 4096 (expanded ++ repeat 32))
  where
    totalW   = sum (map snd letterWeights)
    expanded = concat [ replicate (w * 4096 `div` totalW) (fromIntegral (fromEnum c))
                      | (c, w) <- letterWeights ]

-- | Псевдовипадковий текст заданого розміру (лінійний конгруентний генератор).
genText :: Word64 -> Int -> BS.ByteString
genText seed size = BI.unsafeCreate size (\p -> go p 0 seed)
  where
    go :: Ptr Word8 -> Int -> Word64 -> IO ()
    go !p !i !s
      | i >= size = pure ()
      | otherwise = do
          let s'  = s * 6364136223846793005 + 1442695040888963407
              idx = fromIntegral (s' `shiftR` 52) :: Int
          pokeByteOff p i (unsafeAt letterTable idx)
          go p (i + 1) s'

generateTestFiles :: FilePath -> Int -> Int -> IO ()
generateTestFiles dir count sizeMB = do
  createDirectoryIfMissing True dir
  forM_ [1 .. count] $ \i -> do
    let path = dir </> ("file" ++ show i ++ ".txt")
    BS.writeFile path (genText (fromIntegral i * 7919) (sizeMB * 1024 * 1024))
    putStrLn ("  створено " ++ path ++ " (" ++ show sizeMB ++ " МБ)")
