module DB.Mongo.Tests

import DB.Mongo
import DB.Mongo.Bson
import DB.Mongo.Definitions
import Language.JSON
import Language.JSON.Helpers

%access export

server_uri : String
server_uri = "mongodb://127.0.0.1:27017"

-- connection_failure_path : IO ()
-- connection_failure_path = do
--     db <- DB.Mongo.client_new "mongodb://192.168.99.101:27017"
--     collection <- DB.Mongo.client_get_collection db "testdb" "testcoll"
--     Just result <- DB.Mongo.collection_insert collection "{\"name\":\"burc\",\"age\":50}" | Nothing => printLn("Failure caught!")
--     printLn("Failure not caugth")
--     DB.Mongo.collection_destroy collection
--     DB.Mongo.client_destroy db
--
-- update_flags_test : IO ()
-- update_flags_test = do
--   db <- DB.Mongo.client_new server_uri
--   collection <- DB.Mongo.client_get_collection db "testdb" "test_update"
--   DB.Mongo.collection_insert collection "{\"name\":\"burc\",\"age\":50}"
--   DB.Mongo.collection_insert collection "{\"name\":\"burc\",\"age\":35}"
--   DB.Mongo.collection_update collection "{\"name\":\"burc\"}" "{\"$set\":{\"name\": \"bora\"}}" DB.Mongo.MONGOC_UPDATE_MULTI_UPDATE
--   cursor <- DB.Mongo.collection_find collection "{\"name\":\"burc\"}" Nothing
--   value  <- DB.Mongo.cursor_next cursor
--   case value of
--     Nothing => printLn("Success: no record with name burc")
--     Just _ => printLn("Failure: not all records renamed")
--   DB.Mongo.collection_destroy collection
--   DB.Mongo.client_destroy db

initDB : DBState State DBResult
initDB = do
  init
  dbConnect server_uri
  client_set_error_api 3


insertAndFind : DBState State DBResult
insertAndFind = do
  initDB
  get_collection "testdb" "testcoll"
  collection_insert "{\"name\":\"burc1\",\"age\":50}"
  collection_insert "{\"name\":\"burc2\",\"age\":35}"
  collection_find  "{\"name\":\"burc1\"}" Nothing
  cursor_next

check_insertAndFind : (DBResult, State) -> IO ()
check_insertAndFind (DBResultJSON json, CurrentState (p, _, _, _)) = let (JString result) = value json "name" in
    if result == "burc" then printLn("Success!") else printLn(p)
check_insertAndFind (DBResultJSON json, CurrentState s) = printLn("Test1 Failed!")

dropDatabase : DBState State DBResult
dropDatabase = do
  initDB
  get_collection "testdb1" "testcoll2"
  collection_insert "{\"name\":\"sulu\",\"age\":50}"
  collection_destroy

check_dropDatabase : (DBResult, State) -> IO ()
check_dropDatabase (_, CurrentState (p, _, _, _)) = printLn(p)

test1 : IO ()
test1 = check_insertAndFind (run insertAndFind initialState)

test2 : IO ()
test2 = check_dropDatabase (run dropDatabase initialState)
