{-# LANGUAGE OverloadedStrings #-}
module Integration.IteratorSimpleSpec ( spec )
where

import           Control.Monad.IO.Class
import           RocksDB
import           Test.Hspec
import           TestContext
import           Data.ByteString (ByteString)

mkContext :: IO (RocksDB, ReadOptions, [(ByteString, ByteString)])
mkContext = createContext "rocksdb_iter_simple_test"

spec :: Spec
spec = beforeAll mkContext . afterAll (\(db, _, _) -> ensureSuccess $ close db) $
  describe "RocksDB.Integration.IteratorSimpleSpec" $ do
    context "forward iterators" $ do
      it "iterate through" $ \(db, rOpts, input) -> ensureSuccess $ do
         res <- runIterator db rOpts [] (flip (:))
         liftIO $ reverse res `shouldBe` input

      it "iterate from statring point" $ \(db, rOpts, input) -> ensureSuccess $ do
         res <- runIteratorFrom db rOpts [] "f" (flip (:))
         liftIO $ reverse res `shouldBe` drop 5 input

      it "breakable iterator" $ \(db, rOpts, input) -> ensureSuccess $ do
         res <- runIterator' db rOpts [] takeOne
         liftIO $ reverse res `shouldBe` take 1 input

      it "breakable iterator from starting point" $ \(db, rOpts, input) -> ensureSuccess $ do
         res <- runIteratorFrom' db rOpts [] "g" takeOne
         liftIO $ reverse res `shouldBe` (take 1 . drop 6) input

    context "backwards iterators" $ do
      it "iterate throug" $ \(db, rOpts, input) -> ensureSuccess $ do
         res <- runIteratorBackwards db rOpts [] (flip (:))
         liftIO $ res `shouldBe` input

      it "iterate from statring point" $ \(db, rOpts, input) -> ensureSuccess $ do
         res <- runIteratorBackwardsFrom db rOpts [] "f" (flip (:))
         liftIO $ res `shouldBe` take 6 input

      it "breakable iterator" $ \(db, rOpts, input) -> ensureSuccess $ do
         res <- runIteratorBackwards' db rOpts [] takeOne
         liftIO $ res `shouldBe` take 1 (reverse input)

      it "breakable iterator from starting point" $ \(db, rOpts, input) -> ensureSuccess $ do
         res <- runIteratorBackwardsFrom' db rOpts [] "g" takeOne
         liftIO $ res `shouldBe` (take 1 . drop 6) input

takeOne :: [(ByteString, ByteString)] -> (ByteString, ByteString) -> Iterate [(ByteString, ByteString)]
takeOne a b = IterateCompleted (b : a)
