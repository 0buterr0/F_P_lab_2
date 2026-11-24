-- | Формат архіву, багатопотокове стиснення / розпакування множини файлів.
module Archive
  ( FileEntry (..)
  , ChunkInfo (..)
  , CompressStats (..)
  , collectFiles
  , compressData
  , decompressData
  , serialize
  , parseArchive
  , compressFiles
  , decompressFiles
  , listArchive
  , fmtSec
  , fmtMB
  , pad
  ) where

import Control.Exception (evaluate)
import Control.Monad (ap, forM, forM_, replicateM, when)
import Data.Array.Unboxed (elems, listArray)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Clock (getMonotonicTime)
import Numeric (showFFloat)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , getFileSize
  , listDirectory
  )
import System.FilePath (dropTrailingPathSeparator, takeDirectory, takeFileName, (</>))
import System.IO (IOMode (..), withBinaryFile)

import Huffman
import Parallel

---------------------------------------------------------------------
-- Типи
---------------------------------------------------------------------

-- | Закодований блок файлу.
data ChunkInfo = ChunkInfo
  { ciOrigLen :: !Int            -- розмір блоку до стиснення
  , ciData    :: !BS.ByteString  -- закодовані біти
  }

-- | Файл в архіві: один канонічний код Хаффмана на файл, дані — блоками.
data FileEntry = FileEntry
  { feName    :: !String
  , feSize    :: !Int
  , feLengths :: !CodeLengths
  , feChunks  :: ![ChunkInfo]
  }

-- | Час етапів паралельного стиснення.
data CompressStats = CompressStats
  { csChunks :: !Int
  , csHist   :: !Double
  , csCodes  :: !Double
  , csEncode :: !Double
  }

data FileCode = FileCode !CodeLengths !Encoder

---------------------------------------------------------------------
-- Паралельне стиснення / розпакування в пам'яті
---------------------------------------------------------------------

splitChunks :: Int -> BS.ByteString -> [BS.ByteString]
splitChunks size bs
  | BS.null bs = []
  | otherwise  = let (a, b) = BS.splitAt size bs in a : splitChunks size b

-- | Розбити плаский список на групи заданих розмірів.
regroup :: [Int] -> [a] -> [[a]]
regroup [] _ = []
regroup (n : ns) xs = let (a, b) = splitAt n xs in a : regroup ns b

-- | Стиснення множини файлів. Три етапи, кожен виконується пулом з nThreads потоків:
--   1) частоти байтів для кожного блоку всіх файлів;
--   2) об'єднання частот і побудова канонічного коду Хаффмана для кожного файлу;
--   3) кодування кожного блоку кодом свого файлу.
compressData :: Int -> Int -> [(String, BS.ByteString)] -> IO ([FileEntry], CompressStats)
compressData nThreads chunkSize files = do
  let chunksPerFile = map (splitChunks chunkSize . snd) files
      counts        = map length chunksPerFile
      flat          = concat chunksPerFile
  t0 <- getMonotonicTime
  hists <- parMap nThreads histogram flat
  t1 <- getMonotonicTime
  let histsPerFile = regroup counts hists
  codes <- parMap nThreads
             (\hs -> let ls = buildCodeLengths (mergeHistograms hs) in FileCode ls (mkEncoder ls))
             histsPerFile
  t2 <- getMonotonicTime
  let encTasks = concat
        [ [ (enc, h, c) | (h, c) <- zip hs cs ]
        | (FileCode _ enc, hs, cs) <- zip3 codes histsPerFile chunksPerFile ]
  encoded <- parMap nThreads
               (\(enc, h, c) -> ChunkInfo (BS.length c) (encodeChunk enc (encodedBits enc h) c))
               encTasks
  t3 <- getMonotonicTime
  let entries =
        [ FileEntry name (BS.length content) ls cs
        | ((name, content), FileCode ls _, cs) <- zip3 files codes (regroup counts encoded) ]
  pure (entries, CompressStats (length flat) (t1 - t0) (t2 - t1) (t3 - t2))

-- | Паралельне розпакування: кожен блок декодується незалежно.
decompressData :: Int -> [FileEntry] -> IO [(String, BS.ByteString)]
decompressData nThreads entries = do
  decs <- mapM (evaluate . mkDecoder . feLengths) entries
  let tasks = concat [ [ (d, c) | c <- feChunks fe ] | (fe, d) <- zip entries decs ]
  parts <- parMap nThreads (\(d, ChunkInfo o c) -> decodeChunk d o c) tasks
  forM (zip entries (regroup (map (length . feChunks) entries) parts)) $ \(fe, ps) -> do
    content <- evaluate (BS.concat ps)
    pure (feName fe, content)

---------------------------------------------------------------------
-- Формат файлу архіву
--
--   "HUF1" | кількість файлів (u32)
--   для кожного файлу:
--     довжина імені (u16) | ім'я (UTF-8) | розмір (u64)
--     довжини кодів 256 x u8 | кількість блоків (u32)
--     для кожного блоку: розмір до стиснення (u32), розмір після (u32)
--     дані блоків
--   усі числа little-endian
---------------------------------------------------------------------

serialize :: [FileEntry] -> BB.Builder
serialize entries =
  BB.string7 "HUF1" <> BB.word32LE (fromIntegral (length entries)) <> foldMap entry entries
  where
    entry fe =
      let nameBytes = TE.encodeUtf8 (T.pack (feName fe))
          chunks    = feChunks fe
      in  BB.word16LE (fromIntegral (BS.length nameBytes))
          <> BB.byteString nameBytes
          <> BB.word64LE (fromIntegral (feSize fe))
          <> foldMap (BB.word8 . fromIntegral) (elems (feLengths fe))
          <> BB.word32LE (fromIntegral (length chunks))
          <> foldMap (\c -> BB.word32LE (fromIntegral (ciOrigLen c))
                         <> BB.word32LE (fromIntegral (BS.length (ciData c)))) chunks
          <> foldMap (BB.byteString . ciData) chunks

-- Мінімальний парсер бінарних даних.
newtype Parser a = Parser { runParser :: BS.ByteString -> Either String (a, BS.ByteString) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> case p s of
    Left e        -> Left e
    Right (a, r)  -> Right (f a, r)

instance Applicative Parser where
  pure x = Parser $ \s -> Right (x, s)
  (<*>)  = ap

instance Monad Parser where
  Parser p >>= k = Parser $ \s -> case p s of
    Left e       -> Left e
    Right (a, r) -> runParser (k a) r

failP :: String -> Parser a
failP msg = Parser (const (Left msg))

takeBytes :: Int -> Parser BS.ByteString
takeBytes n = Parser $ \s ->
  if BS.length s < n then Left "Архів пошкоджено: неочікуваний кінець файлу" else Right (BS.splitAt n s)

-- | Ціле число little-endian з n байтів.
leWord :: Int -> Parser Int
leWord n = foldr (\b acc -> acc * 256 + fromIntegral b) 0 . BS.unpack <$> takeBytes n

archiveP :: Parser [FileEntry]
archiveP = do
  magic <- takeBytes 4
  when (magic /= BC.pack "HUF1") (failP "Це не архів формату HUF1")
  n <- leWord 4
  replicateM n entryP

entryP :: Parser FileEntry
entryP = do
  nameLen   <- leWord 2
  name      <- T.unpack . TE.decodeUtf8 <$> takeBytes nameLen
  size      <- leWord 8
  lensBytes <- takeBytes 256
  let lensList = map fromIntegral (BS.unpack lensBytes)
  when (any (> maxCodeLen) lensList) (failP "Архів пошкоджено: некоректні довжини кодів")
  k      <- leWord 4
  sizes  <- replicateM k ((,) <$> leWord 4 <*> leWord 4)
  chunks <- mapM (\(o, c) -> ChunkInfo o <$> takeBytes c) sizes
  pure (FileEntry name size (listArray (0, 255) lensList) chunks)

parseArchive :: BS.ByteString -> Either String [FileEntry]
parseArchive s = case runParser archiveP s of
  Left e -> Left e
  Right (es, rest)
    | BS.null rest -> Right es
    | otherwise    -> Left "Архів пошкоджено: зайві дані в кінці"

---------------------------------------------------------------------
-- Робота з файловою системою
---------------------------------------------------------------------

-- | Розгорнути список файлів і папок у пари (ім'я в архіві, шлях на диску).
collectFiles :: [FilePath] -> IO [(String, FilePath)]
collectFiles = fmap concat . mapM one
  where
    one p = do
      isDir <- doesDirectoryExist p
      if isDir
        then do
          let base = takeFileName (dropTrailingPathSeparator p)
          rels <- listRecursive p
          pure [ (toArchiveName (if null base || base == "." then rel else base </> rel), p </> rel)
               | rel <- rels ]
        else pure [(takeFileName p, p)]

    listRecursive dir = do
      names <- sort <$> listDirectory dir
      fmap concat . forM names $ \nm -> do
        let full = dir </> nm
        isDir <- doesDirectoryExist full
        if isDir then map (nm </>) <$> listRecursive full else pure [nm]

    toArchiveName = map (\c -> if c == '\\' then '/' else c)

-- | Шлях для розпакування з перевіркою, що файл не вийде за межі папки.
safeOutputPath :: FilePath -> String -> Either String FilePath
safeOutputPath outDir name
  | null parts || any bad parts = Left ("Небезпечне або некоректне ім'я файлу в архіві: " ++ name)
  | otherwise                   = Right (foldl (</>) outDir parts)
  where
    parts = filter (not . null) (splitName name)
    bad p = p == "." || p == ".." || ':' `elem` p
    splitName s = case break (`elem` "/\\") s of
      (a, [])       -> [a]
      (a, _ : rest) -> a : splitName rest

---------------------------------------------------------------------
-- Команди
---------------------------------------------------------------------

fmtSec :: Double -> String
fmtSec x = showFFloat (Just 3) x " с"

fmtMB :: Integral a => a -> String
fmtMB b = showFFloat (Just 2) (fromIntegral b / 1048576 :: Double) " МБ"

pad :: Int -> String -> String
pad n s = s ++ replicate (n - length s) ' '

row :: String -> String -> IO ()
row label value = putStrLn ("  " ++ pad 24 label ++ value)

compressFiles :: Int -> Int -> FilePath -> [FilePath] -> IO ()
compressFiles nThreads chunkSize archivePath inputs = do
  files <- collectFiles inputs
  when (null files) (ioError (userError "не знайдено жодного файлу для стиснення"))
  t0 <- getMonotonicTime
  contents <- parMapIO nThreads (\(name, path) -> (,) name <$> BS.readFile path) files
  t1 <- getMonotonicTime
  (entries, st) <- compressData nThreads chunkSize contents
  t2 <- getMonotonicTime
  withBinaryFile archivePath WriteMode (\h -> BB.hPutBuilder h (serialize entries))
  t3 <- getMonotonicTime
  archSize <- getFileSize archivePath
  let total = sum (map (BS.length . snd) contents)
      ratio = if total == 0 then 0 else 100 * fromIntegral archSize / fromIntegral total :: Double
  putStrLn ("Стиснення в " ++ archivePath)
  row "Файлів:" (show (length files))
  row "Загальний розмір:" (fmtMB total)
  row "Потоків:" (show nThreads)
  row "Блоків:" (show (csChunks st) ++ " по " ++ show (chunkSize `div` 1024) ++ " КБ")
  putStrLn "  Час етапів:"
  row "  читання файлів" (fmtSec (t1 - t0))
  row "  підрахунок частот" (fmtSec (csHist st))
  row "  побудова кодів" (fmtSec (csCodes st))
  row "  кодування" (fmtSec (csEncode st))
  row "  запис архіву" (fmtSec (t3 - t2))
  row "  разом" (fmtSec (t3 - t0))
  row "Розмір архіву:" (fmtMB archSize ++ " (" ++ showFFloat (Just 1) ratio " % від початкового)")

decompressFiles :: Int -> FilePath -> FilePath -> IO ()
decompressFiles nThreads archivePath outDir = do
  t0 <- getMonotonicTime
  raw <- BS.readFile archivePath
  entries <- either (ioError . userError) pure (parseArchive raw)
  targets <- either (ioError . userError) pure (mapM (safeOutputPath outDir . feName) entries)
  t1 <- getMonotonicTime
  files <- decompressData nThreads entries
  t2 <- getMonotonicTime
  forM_ (zip targets files) $ \(path, (_, content)) -> do
    createDirectoryIfMissing True (takeDirectory path)
    BS.writeFile path content
  t3 <- getMonotonicTime
  putStrLn ("Розпаковано " ++ show (length files) ++ " файл(ів) у " ++ outDir)
  forM_ (zip entries files) $ \(fe, (_, c)) ->
    when (BS.length c /= feSize fe) $
      putStrLn ("  ! Розмір не збігається: " ++ feName fe)
  row "Потоків:" (show nThreads)
  row "Читання архіву:" (fmtSec (t1 - t0))
  row "Декодування:" (fmtSec (t2 - t1))
  row "Запис файлів:" (fmtSec (t3 - t2))
  row "Разом:" (fmtSec (t3 - t0))

listArchive :: FilePath -> IO ()
listArchive archivePath = do
  raw <- BS.readFile archivePath
  entries <- either (ioError . userError) pure (parseArchive raw)
  putStrLn (pad 40 "Файл" ++ pad 14 "Розмір" ++ pad 14 "Стиснутий" ++ pad 9 "%" ++ "Блоків")
  putStrLn (replicate 84 '-')
  forM_ entries $ \fe -> do
    let comp  = sum (map (BS.length . ciData) (feChunks fe))
        ratio = if feSize fe == 0 then 0 else 100 * fromIntegral comp / fromIntegral (feSize fe) :: Double
    putStrLn (pad 40 (feName fe) ++ pad 14 (fmtMB (feSize fe)) ++ pad 14 (fmtMB comp)
              ++ pad 9 (showFFloat (Just 1) ratio "") ++ show (length (feChunks fe)))
  putStrLn ("Усього файлів: " ++ show (length entries) ++ ", розмір архіву: " ++ fmtMB (BS.length raw))
