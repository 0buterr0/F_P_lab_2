-- | Пул потоків (forkIO + MVar) з динамічним розподілом завдань.
module Parallel
  ( parMapIO
  , parMap
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (modifyMVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Exception (SomeException, evaluate, throwIO, try)
import Control.Monad (forM, replicateM_)

tryAny :: IO a -> IO (Either SomeException a)
tryAny = try

-- | Виконати дію для кожного елемента списку на nThreads потоках.
--   Потоки беруть завдання зі спільної черги (хто звільнився — той бере наступне),
--   тому навантаження розподіляється рівномірно навіть для блоків різного розміру.
--   Результати повертаються в початковому порядку.
parMapIO :: Int -> (a -> IO b) -> [a] -> IO [b]
parMapIO nThreads f xs = do
  slots <- forM xs $ \x -> do
    v <- newEmptyMVar
    pure (x, v)
  queue <- newMVar slots
  let worker = do
        next <- modifyMVar queue $ \q -> pure $ case q of
          []       -> ([], Nothing)
          (t : ts) -> (ts, Just t)
        case next of
          Nothing -> pure ()
          Just (x, v) -> do
            r <- tryAny (f x >>= evaluate)
            putMVar v r
            worker
  replicateM_ (max 1 (min nThreads (length slots))) (forkIO worker)
  forM slots $ \(_, v) -> do
    r <- takeMVar v
    either throwIO pure r

-- | Паралельне обчислення чистої функції (результат обчислюється до WHNF у потоці-виконавці).
parMap :: Int -> (a -> b) -> [a] -> IO [b]
parMap n f = parMapIO n (evaluate . f)
