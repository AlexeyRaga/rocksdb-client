{-# LANGUAGE OverloadedStrings #-}

module Integration.ComparatorSpec ( spec )
where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.ByteString            (ByteString)
import           Data.ByteString.Char8      (singleton)
import           Data.List
import           Data.Monoid
import           RocksDB
import           RocksDB.Options
import           RocksDB.ReadOptions
import           RocksDB.WriteOptions
import           System.Directory
import           System.FilePath
import           Test.Hspec
import           TestContext

values :: [(ByteString, ByteString)]
values = [ ("chatswood", "1")
         , ("namur", "2")
         , ("astana", "3")
         , ("barcelona", "4")
         ]

spec :: Spec
spec = describe "hey" $
  it "dummy" $ ensureSuccess $ do
    path <- liftIO $ dbPath "cmp_test3"
    db   <- open path (setCreateIfMissing True <> setComparator (flip compare))

    wOpts <- defaultWriteOptions
    rOpts <- defaultReadOptions

    mapM_ (uncurry $ put db wOpts) values

    res <- runIterator db rOpts [] (flip (:))
    liftIO $ reverse res `shouldBe` reverse (sortOn fst values)

    liftIO $ 1 `shouldBe` 1
    close db
