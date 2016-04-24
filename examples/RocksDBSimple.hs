{-# LANGUAGE OverloadedStrings #-}
module RocksDBSimple

where
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           RocksDB
import           RocksDB.Options
import           RocksDB.ReadOptions
import           RocksDB.Types
import           RocksDB.WriteOptions
import           System.Directory
import           System.FilePath

dbPath :: IO FilePath
dbPath  = (</> "rocksdb_simple_example")       <$> getTemporaryDirectory

--main :: IO ()
main = do
    res <- runExample
    print $ show res


runExample = runExceptT $ do
    path <- liftIO dbPath
    liftIO $ print $ "start: " ++ path

    db <- open path (setCreateIfMissing True)
    liftIO $ print "1"

    wOpts <- defaultWriteOptions
    rOpts <- defaultReadOptions
    liftIO $ print "2"

    put db wOpts "MyKey" "MyValue"
    liftIO $ print "3"

    res <- get db rOpts "MyKey"
    liftIO $ print "4"

    liftIO $ print (show res)

    close db
    liftIO $ print path
