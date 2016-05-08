module RocksDB.Basics
( open, openForReadOnly
, close
, put, get, delete
, multiGet, multiGet'
)
where

import           Control.Error.Util
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.ByteString            (ByteString)
import           Data.Maybe
import           RocksDB.Comparator
import           RocksDB.Internal.C
import           RocksDB.Options
import           RocksDB.ReadOptions
import           RocksDB.Types
import           RocksDB.WriteOptions
import           System.FilePath

-- | Opens a RocksDB database with a given path and options
-- Note that the database must be closed with the 'close' function to avoid leaks.
-- @
--    runExceptT $ do
--        db <- open path (setCreateIfMissing True)
--        ...
-- @
open :: FilePath              -- ^ Path where the database files are located
     -> OptionsBuilder        -- ^ Database options (see 'OptionsBuilder').
     -> RocksDBResult RocksDB -- ^ RocksDB handle. Mush be closed with the 'close' function.
open p o = do
    opt@(Options _ o') <- liftIO $ createOptions o
    res <- liftIO $ c_rocksdb_open o' p
    hoistEither $ RocksDB opt <$> res

-- | Opens RocksDB database in read only mode
openForReadOnly :: FilePath              -- ^ Path where the database files are located
                -> OptionsBuilder        -- ^ Database options (see 'OptionsBuilder').
                -> ErrorIfExists         -- ^ Error if log file exists
                -> RocksDBResult RocksDB -- ^ RocksDB handle. Mush be closed with the 'close' function.
openForReadOnly p o (ErrorIfExists e) = do
    opt@(Options _ o') <- liftIO $ createOptions o
    res <- liftIO $ c_rocksdb_open_for_read_only o' p e
    hoistEither $ RocksDB opt <$> res

-- | Closes the database handle.
close :: RocksDB -> RocksDBResult ()
close (RocksDB (Options c _) r) = do
    res <- liftIO (c_rocksdb_close r)
    liftIO $ mapM_ releaseComparator c
    (hoistEither . Right) res

-- | Puts a given key/value pair to the database.
put :: RocksDB           -- ^ RocksDB handle
    -> WriteOptions      -- ^ Write operation options
    -> ByteString        -- ^ Key
    -> ByteString        -- ^ Value
    -> RocksDBResult ()  -- ^ Result may contain error, '()' if successful
put (RocksDB _ r) (WriteOptions o) k v =
    maybeErrorResult $ c_rocksdb_put r o k v

-- | Gets a value for a given key
get :: RocksDB                          -- ^ RocksDB handle
    -> ReadOptions                      -- ^ Read operation options
    -> ByteString                       -- ^ Key
    -> RocksDBResult (Maybe ByteString) -- ^ Possibly value if exists for a given key
get (RocksDB _ r) (ReadOptions o) k =
    liftIO (c_rocksdb_get r o k) >>= hoistEither

-- | Gets values for multiple keys
multiGet :: RocksDB                          -- ^ RocksDB handle
         -> ReadOptions                      -- ^ Read operation options
         -> [ByteString]                     -- ^ A list of keys
         -> RocksDBResult [Maybe ByteString] -- ^ A list of possible values for each key, in the same order with the keys
multiGet (RocksDB _ r) (ReadOptions o) ks =
    liftIO (c_rocksdb_multi_get r o ks) >>= hoistEither

-- | Gets values for multiple keys
multiGet' :: RocksDB                                  -- ^ RocksDB handle
          -> ReadOptions                              -- ^ Read operation options
          -> [ByteString]                             -- ^ A list of keys
          -> RocksDBResult [(ByteString, ByteString)] -- ^ List of key/value pairs, only contains key/values for keys that have values
multiGet' r o ks = do
    res <- multiGet r o ks
    return [(k, fromJust v) | (k, v) <- zip ks res, isJust v]

-- | Deletes a given key from the database
delete :: RocksDB          -- ^ RocksDB handle
       -> WriteOptions     -- ^ Write operation options
       -> ByteString       -- ^ A key to delete
       -> RocksDBResult () -- ^ Result may contain error, '()' if successful
delete (RocksDB _ r) (WriteOptions o) k =
    maybeErrorResult $ c_rocksdb_delete r o k
-------------------------------------------------------------------------------
maybeErrorResult :: MonadIO m => IO (Maybe a) -> ExceptT a m ()
maybeErrorResult ma =
  liftIO ma >>= (hoistEither . maybeErrorToEither)
  where
      maybeErrorToEither x = case x of
          Just x' -> Left x'
          Nothing -> Right ()
