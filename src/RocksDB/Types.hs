module RocksDB.Types
( module RocksDB.Internal.Types
, module RocksDB.Types
)
where

import           Control.Monad
import           Control.Monad.Trans.Except
import           RocksDB.Internal.C
import           RocksDB.Internal.Types

newtype ErrorIfExists = ErrorIfExists Bool

data RocksDB = RocksDB Options RocksDBFPtr

data Options = Options (Maybe Comparator) OptionsFPtr

data OptionsBuilder = OptionsBuilder { runOptionsBuilder :: Options -> IO Options }

instance Monoid OptionsBuilder where
    mempty = OptionsBuilder return
    mappend a b = OptionsBuilder (runOptionsBuilder a >=> runOptionsBuilder b)

data Comparator = Comparator DestructorFunPtr NameFunPtr CompareFunPtr ComparatorFPtr

type RocksDBResult a = ExceptT RocksDBError IO a


