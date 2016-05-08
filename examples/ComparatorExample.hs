{-# LANGUAGE OverloadedStrings #-}
module ComparatorExample
where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.ByteString            (ByteString)
import           Data.ByteString.Char8      (pack, singleton)
import           Data.Monoid
import           RocksDB
import           RocksDB.Options
import           RocksDB.WriteOptions
import           System.Directory
import           System.FilePath

dbPath :: IO FilePath
dbPath  = (</> "rocksdb_flip_comparator_example_w") <$> getTemporaryDirectory

-- | How to compare two bytestring.
-- For simplicity of the example let's compare them in a reversed order
flipComparator :: ByteString -> ByteString -> Ordering
flipComparator = compare

main :: IO ()
main = runExample >>= (print . show)

runExample :: IO (Either RocksDBError ())
runExample = runExceptT $ do
    path  <- liftIO dbPath
    liftIO $ print path
    db    <- open path $ setCreateIfMissing True <> setComparator flipComparator
    liftIO $ print "opened"

    wOpts <- defaultWriteOptions
    rOpts <- defaultReadOptions
    runIterator db rOpts [] (flip (:)) >>= (liftIO . print . show)

    put db wOpts "1" "b"

    liftIO $ print "---------------------------------------------"

    let write = uncurry $ put db wOpts

    let input = [(bs, bs) | x <- ['a' .. 'z'], let bs = singleton x]
    mapM_ write input
    --mapM_ (liftIO.print.show) input
    --runIterator db rOpts [] (flip (:)) >>= (liftIO . print . show)
    --runIterator db rOpts [] (flip (:)) >>= (liftIO . print . show)

    --let input2 = [(bs, bs) | x <- ['a' .. 'z'], let bs = pack [x, x]]
    --mapM_ write input2

    -- res2 <- runIterator db rOpts [] (flip (:))
    -- liftIO $ print (show res2)

    close db
