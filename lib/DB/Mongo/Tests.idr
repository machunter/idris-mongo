module DB.Mongo.Tests
import DB.Mongo
import DB.Mongo.Bson

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

-- nice_path : DBState ()
-- nice_path = do
--     pure (printLn("hello"))
--     DB.Mongo.init
    -- connect to db server
    -- DB.Mongo.client_new server_uri
    -- set the error level
    -- DB.Mongo.client_set_error_api db 2
    -- get a collection handler
    -- DB.Mongo.client_get_collection "testdb" "testcoll"
    -- insert 2 records
    -- DB.Mongo.collection_insert "{\"name\":\"burc\",\"age\":50}"
    -- DB.Mongo.collection_insert collection "{\"name\":\"burc\",\"age\":35}"
    -- -- get a cursor
    -- cursor <- DB.Mongo.collection_find collection "{\"name\":\"burc\"}" Nothing
    -- -- print the records
    -- DB.Mongo.cursor_next cursor >>= printLn
    -- DB.Mongo.cursor_next cursor >>= printLn
    -- -- no such record
    -- DB.Mongo.cursor_next cursor >>= printLn
    -- -- close cursor
    -- DB.Mongo.cursor_destroy cursor
    -- -- remove one record
    -- DB.Mongo.collection_remove collection "{\"name\":\"burc\",\"age\":50}"
    -- -- update the one record
    -- update <- DB.Mongo.collection_find_and_modify collection "{\"name\":\"burc\"}" "{\"$set\":{\"age\": 55}}"
    -- -- get a cursor
    -- cursor2 <- DB.Mongo.collection_find collection "{\"name\":\"burc\"}" Nothing
    -- -- should show the updated record
    -- DB.Mongo.cursor_next cursor2 >>= printLn
    -- -- no record
    -- DB.Mongo.cursor_next cursor2 >>= printLn
    -- -- remove anything with the following
    -- r <- DB.Mongo.collection_remove collection "{\"name\":\"burc\"}"
    -- -- close cursor
    -- DB.Mongo.cursor_destroy cursor2
    -- close collection
    -- DB.Mongo.collection_destroy
    -- close database
    -- DB.Mongo.client_destroy
    -- clean up memory




myProgram : DBState State DBResult
myProgram = do
    init
    dbConnect server_uri
    get_collection "testdb" "testcoll"
    collection_insert "{\"name\":\"burc\",\"age\":50}"
    collection_insert "{\"name\":\"burc\",\"age\":35}"
    get_collection "testdn" "newcoll"
    collection_insert "{\"name\":\"bora\",\"age\":22}"


namespace Main
  main : IO ()
  main = do
    case Prelude.Basics.fst (run myProgram initialState) of
      DBResultNothing x => x
    print("done")
