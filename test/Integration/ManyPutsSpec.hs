{-# LANGUAGE OverloadedStrings #-}
module Integration.ManyPutsSpec ( spec )
where

import           Control.Monad.IO.Class
import           Data.ByteString.Char8  (singleton)
import           RocksDB
import           RocksDB.Options
import           RocksDB.WriteOptions
import           Test.Hspec
import           TestContext

spec :: Spec
spec = describe "RocskDB.Integration.ManyPutsSpec" $
    it "stub" $ pendingWith "uncomment to simulate RocksDB/LevelDB put error"
    -- it "put, get, delete" $ ensureSuccess $ do
    --     path <- liftIO $ dbPath "rocksdb_many_puts"
    --     db   <- open path (setCreateIfMissing True)

    --     wOpts <- defaultWriteOptions
    --     rOpts <- defaultReadOptions

    --     put db wOpts "MyKey" "MyValue"
    --     val <- get db rOpts "MyKey"
    --     liftIO $ val `shouldBe` Just "MyValue"

    --     let write = uncurry $ put db wOpts

    --     -- let input = [(bs, bs) | x <- ['a' .. 'z'], let bs = singleton x]
    --     -- mapM_ write input

    --     dval <- get db rOpts "t"
    --     liftIO $ dval `shouldBe` Just "t"
    --     close db
