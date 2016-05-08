{-# LANGUAGE OverloadedStrings #-}
module Integration.SimpleSpec ( spec )
where

import           Control.Monad.IO.Class
import           RocksDB
import           RocksDB.Options
import           RocksDB.WriteOptions
import           Test.Hspec
import           TestContext

spec :: Spec
spec = describe "RocskDB.Integration.SimpleSpec" $
    it "put, get, delete" $ ensureSuccess $ do
        path <- liftIO $ dbPath "rocksdb_simple_example"
        liftIO $ print ("DB in: " ++ path)
        db   <- open path (setCreateIfMissing True)

        wOpts <- defaultWriteOptions
        rOpts <- defaultReadOptions

        put db wOpts "MyKey" "MyValue"
        val <- get db rOpts "MyKey"
        liftIO $ val `shouldBe` Just "MyValue"

        mvals' <- multiGet' db rOpts ["MyKey", "noKey"]
        liftIO $ mvals' `shouldBe` [("MyKey", "MyValue")]

        delete db wOpts "MyKey"
        dval <- get db rOpts "MyKey"
        liftIO $ dval `shouldBe` Nothing

        close db
