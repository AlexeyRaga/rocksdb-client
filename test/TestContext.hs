module TestContext where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.ByteString            (ByteString)
import           Data.ByteString.Char8      (singleton)
import           Data.Monoid
import           RocksDB
import           RocksDB.Options
import           RocksDB.WriteOptions
import           System.Directory
import           System.FilePath
import           System.IO.Temp
import           Test.Hspec

ensureSuccess :: RocksDBResult () -> IO ()
ensureSuccess r = do
    res <- runExceptT r
    res `shouldBe` Right()

dbPath :: String -> IO FilePath
dbPath name = getTemporaryDirectory >>= flip createTempDirectory name

createContext :: String -> IO (RocksDB, ReadOptions, [(ByteString, ByteString)])
createContext name = createContext' name mempty

createContext' :: String -> OptionsBuilder -> IO (RocksDB, ReadOptions, [(ByteString, ByteString)])
createContext' name opts = do
  path <- dbPath name
  print $ "DB in: " ++ path
  res <- runExceptT create
  case res of
    Right x -> return x
    Left _ -> undefined
  where
    create = do
      path  <- liftIO $ dbPath name
      db    <- open path (setCreateIfMissing True <> opts)
      wOpts <- defaultWriteOptions
      rOpts <- defaultReadOptions
      let input =  [(bs, bs) | x <- ['a' .. 'z'], let bs = singleton x]
      mapM_ (uncurry $ put db wOpts) input

      let input =  [(bs, bs) | x <- ['a' .. 'z'], let bs = singleton x]
      mapM_ (uncurry $ put db wOpts) input

      let input =  [(bs, bs) | x <- ['a' .. 'z'], let bs = singleton x]
      mapM_ (uncurry $ put db wOpts) input

      let input =  [(bs, bs) | x <- ['a' .. 'z'], let bs = singleton x]
      mapM_ (uncurry $ put db wOpts) input
      return (db, rOpts, input)
