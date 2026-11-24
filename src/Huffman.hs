{-# LANGUAGE BangPatterns #-}

-- | Алгоритм Хаффмана: частоти байтів, довжини кодів, канонічні коди,
--   кодування та декодування блоку даних.
module Huffman
  ( Histogram
  , histogram
  , mergeHistograms
  , CodeLengths
  , maxCodeLen
  , buildCodeLengths
  , Encoder
  , mkEncoder
  , encodedBits
  , encodeChunk
  , Decoder
  , mkDecoder
  , decodeChunk
  ) where

import Control.Monad (when)
import Data.Array.Base (unsafeAt, unsafeRead, unsafeWrite)
import Data.Array.ST (newArray, runSTUArray)
import Data.Array.Unboxed (UArray, accumArray, elems, listArray)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Data.Word (Word64, Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)

---------------------------------------------------------------------
-- Частоти
---------------------------------------------------------------------

-- | Кількість входжень кожного байта 0..255.
type Histogram = UArray Int Int

histogram :: BS.ByteString -> Histogram
histogram bs = runSTUArray $ do
  arr <- newArray (0, 255) 0
  let n = BS.length bs
      go !i
        | i >= n = pure ()
        | otherwise = do
            let b = fromIntegral (BU.unsafeIndex bs i) :: Int
            c <- unsafeRead arr b
            unsafeWrite arr b (c + 1)
            go (i + 1)
  go 0
  pure arr

mergeHistograms :: [Histogram] -> Histogram
mergeHistograms hs =
  accumArray (+) 0 (0, 255) [ (i, c) | h <- hs, (i, c) <- zip [0 ..] (elems h) ]

---------------------------------------------------------------------
-- Довжини кодів (дерево Хаффмана)
---------------------------------------------------------------------

-- | Довжина коду кожного байта; 0 — байт не зустрічається.
type CodeLengths = UArray Int Int

-- | Обмеження довжини коду (щоб код вміщався в машинне слово).
maxCodeLen :: Int
maxCodeLen = 32

data Tree = Leaf !Int | Node Tree Tree

-- | Довжини кодів за частотами. Якщо дерево вийшло глибшим за maxCodeLen,
--   частоти зменшуються вдвічі й дерево будується заново.
buildCodeLengths :: Histogram -> CodeLengths
buildCodeLengths h = go (elems h)
  where
    go freqs =
      let ls = huffmanLengths freqs
      in  if maximum (0 : ls) <= maxCodeLen
            then listArray (0, 255) ls
            else go (map (\f -> if f == 0 then 0 else max 1 (f `div` 2)) freqs)

huffmanLengths :: [Int] -> [Int]
huffmanLengths freqs =
  case [ (f, s) | (s, f) <- zip [0 :: Int ..] freqs, f > 0 ] of
    []       -> replicate 256 0
    [(_, s)] -> [ if i == s then 1 else 0 | i <- [0 .. 255] ]
    syms     ->
      let queue  = M.fromList [ ((f, uid), Leaf s) | (uid, (f, s)) <- zip [0 ..] syms ]
          tree   = build (length syms) queue
          depths = M.fromList (leafDepths 0 tree)
      in  [ M.findWithDefault 0 i depths | i <- [0 .. 255] ]
  where
    -- Черга з пріоритетом: ключ (вага, унікальний номер).
    build :: Int -> M.Map (Int, Int) Tree -> Tree
    build next q = case M.minViewWithKey q of
      Nothing -> error "huffmanLengths: порожня черга"
      Just (((w1, _), t1), q1) -> case M.minViewWithKey q1 of
        Nothing -> t1
        Just (((w2, _), t2), q2) ->
          build (next + 1) (M.insert (w1 + w2, next) (Node t1 t2) q2)

    leafDepths :: Int -> Tree -> [(Int, Int)]
    leafDepths d (Leaf s)   = [(s, d)]
    leafDepths d (Node l r) = leafDepths (d + 1) l ++ leafDepths (d + 1) r

---------------------------------------------------------------------
-- Канонічні коди
---------------------------------------------------------------------

-- | Символи, впорядковані за (довжина, символ) — основа канонічного коду.
sortedSymbols :: CodeLengths -> [(Int, Int)]
sortedSymbols ls =
  sortBy (comparing (\(s, l) -> (l, s))) [ (s, l) | (s, l) <- zip [0 ..] (elems ls), l > 0 ]

-- | (символ, код, довжина). Коди однакової довжини йдуть підряд.
canonicalCodes :: CodeLengths -> [(Int, Int, Int)]
canonicalCodes ls = assign 0 0 (sortedSymbols ls)
  where
    assign :: Int -> Int -> [(Int, Int)] -> [(Int, Int, Int)]
    assign _ _ [] = []
    assign !code !prevLen ((s, l) : rest) =
      let code' = code `shiftL` (l - prevLen)
      in  (s, code', l) : assign (code' + 1) l rest

---------------------------------------------------------------------
-- Кодування
---------------------------------------------------------------------

data Encoder = Encoder
  { encCodes :: !(UArray Int Word64)
  , encLens  :: !(UArray Int Int)
  }

mkEncoder :: CodeLengths -> Encoder
mkEncoder ls = Encoder codes ls
  where
    codes = accumArray (\_ c -> c) 0 (0, 255)
              [ (s, fromIntegral c) | (s, c, _) <- canonicalCodes ls ]

-- | Скільки біт займе блок із заданою гістограмою.
encodedBits :: Encoder -> Histogram -> Int
encodedBits enc h = sum (zipWith (*) (elems h) (elems (encLens enc)))

-- | Закодувати блок. nBits — точна кількість біт (з encodedBits).
encodeChunk :: Encoder -> Int -> BS.ByteString -> BS.ByteString
encodeChunk (Encoder codes lens) nBits input = BI.unsafeCreate nBytes (\p -> go p 0 0 0 0)
  where
    nBytes = (nBits + 7) `div` 8
    n      = BS.length input

    -- acc містить accLen ще не записаних біт (молодші біти).
    go :: Ptr Word8 -> Int -> Int -> Word64 -> Int -> IO ()
    go !p !i !out !acc !accLen
      | accLen >= 8 = do
          pokeByteOff p out (fromIntegral (acc `shiftR` (accLen - 8)) :: Word8)
          go p i (out + 1) acc (accLen - 8)
      | i >= n =
          when (accLen > 0) $
            pokeByteOff p out (fromIntegral (acc `shiftL` (8 - accLen)) :: Word8)
      | otherwise = do
          let b = fromIntegral (BU.unsafeIndex input i) :: Int
              l = unsafeAt lens b
          go p (i + 1) out ((acc `shiftL` l) .|. unsafeAt codes b) (accLen + l)

---------------------------------------------------------------------
-- Декодування
---------------------------------------------------------------------

data Decoder = Decoder
  { decFirst  :: !(UArray Int Int)    -- перший код довжини L (індекс L-1)
  , decCount  :: !(UArray Int Int)    -- кількість кодів довжини L
  , decOffset :: !(UArray Int Int)    -- позиція першого символу довжини L у decSyms
  , decSyms   :: !(UArray Int Word8)  -- символи в канонічному порядку
  }

mkDecoder :: CodeLengths -> Decoder
mkDecoder ls = Decoder firstA countA offsetA symsA
  where
    lensList = elems ls
    counts   = [ length (filter (== l) lensList) | l <- [1 .. maxCodeLen] ]
    countA   = listArray (1, maxCodeLen) counts
    firstA   = listArray (1, maxCodeLen) (take maxCodeLen (scanl (\f c -> (f + c) * 2) 0 counts))
    offsetA  = listArray (1, maxCodeLen) (take maxCodeLen (scanl (+) 0 counts))
    syms     = map (fromIntegral . fst) (sortedSymbols ls)
    symsA    = if null syms
                 then listArray (0, 0) [0]
                 else listArray (0, length syms - 1) syms

-- | Розкодувати блок, що містить origLen байтів.
decodeChunk :: Decoder -> Int -> BS.ByteString -> BS.ByteString
decodeChunk (Decoder firstA countA offsetA symsA) origLen input =
  BI.unsafeCreate origLen (\p -> go p 0 0 0 0)
  where
    totalBits = BS.length input * 8

    go :: Ptr Word8 -> Int -> Int -> Int -> Int -> IO ()
    go !p !out !bitPos !code !len
      | out >= origLen = pure ()
      | bitPos >= totalBits || len >= maxCodeLen =
          error "Пошкоджені дані: некоректний код Хаффмана"
      | otherwise = do
          let byte  = BU.unsafeIndex input (bitPos `shiftR` 3)
              bit   = fromIntegral ((byte `shiftR` (7 - (bitPos .&. 7))) .&. 1) :: Int
              code' = code * 2 + bit
              k     = len          -- індекс для довжини len+1 (масиви з 1, unsafeAt з 0)
              idx   = code' - unsafeAt firstA k
          if idx >= 0 && idx < unsafeAt countA k
            then do
              pokeByteOff p out (unsafeAt symsA (unsafeAt offsetA k + idx))
              go p (out + 1) (bitPos + 1) 0 0
            else go p out (bitPos + 1) code' (len + 1)
