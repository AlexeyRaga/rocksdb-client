module RocksDB.Comparator
( Comparator
, setComparator
, releaseComparator
)
where

import           Data.ByteString    (ByteString)
import           Debug.Trace
import           Foreign
import           Foreign.C.String
import           RocksDB.Internal.C
import           RocksDB.Types

createComparator :: String -> (ByteString -> ByteString -> Ordering) -> IO Comparator
createComparator n f =
  withCString n $ \name -> do
    d' <- mkDestructor $ trace "calling destructor" (const ())
    f' <- mkCompareFunPtr (mkCompareFun f)
    n' <- mkName $ const name
    c  <- c_rocksdb_comparator_create nullPtr d' f' n'
    c' <- newForeignPtr_ c
    return $ Comparator d' n' f' c'

releaseComparator :: Comparator -> IO ()
releaseComparator (Comparator d n f c) = do
  c_rocksdb_comparator_destroy c
  freeHaskellFunPtr d
  freeHaskellFunPtr n
  freeHaskellFunPtr f
  return ()

setComparator :: (ByteString -> ByteString -> Ordering) -> OptionsBuilder
setComparator = setComparator' "custom-noname-cmp"

setComparator' :: String -> (ByteString -> ByteString -> Ordering) -> OptionsBuilder
setComparator' n f =
  OptionsBuilder $ \(Options c o) -> do
    cmp@(Comparator _ _ _ c') <- createComparator n f
    mapM_ releaseComparator c
    c_rocksdb_options_set_comparator o c'
    return $ Options (Just cmp) o
