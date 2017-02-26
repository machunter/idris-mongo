module DB.Mongo.Tests
import DB.Mongo
import DB.Mongo.Bson

connection_failure_path : IO ()
connection_failure_path = do
    printLn("Starting Failure Path")
    db <- DB.Mongo.client_new "mongodb://192.168.99.101:27017"
    collection <- DB.Mongo.client_get_collection db "testdb" "testcoll"
    x <- DB.Mongo.collection_insert collection "{\"name\":\"burc\",\"age\":50}"
    case x of
      Nothing => printLn("Failure caught!")
      Just _ => printLn("Failure not caugth")
    DB.Mongo.collection_destroy collection
    DB.Mongo.client_destroy db
    printLn("Done Failure Path")

nice_path : IO ()
nice_path = do
    printLn("Starting Nice Path")
    -- connect to db server
    db <- DB.Mongo.client_new "mongodb://192.168.99.100:27017"
    -- set the error level
    -- DB.Mongo.client_set_error_api db 2
    -- get a collection handler
    collection <- DB.Mongo.client_get_collection db "testdb" "testcoll"
    -- insert 2 records
    DB.Mongo.collection_insert collection "{\"name\":\"burc\",\"age\":50}"
    DB.Mongo.collection_insert collection "{\"name\":\"burc\",\"age\":35}"
    -- get a cursor
    cursor <- DB.Mongo.collection_find collection "{\"name\":\"burc\"}" Nothing
    -- print the records
    DB.Mongo.cursor_next cursor >>= printLn
    DB.Mongo.cursor_next cursor >>= printLn
    -- no such record
    DB.Mongo.cursor_next cursor >>= printLn
    -- close cursor
    DB.Mongo.cursor_destroy cursor
    -- remove one record
    DB.Mongo.collection_remove collection "{\"name\":\"burc\",\"age\":50}"
    -- update the one record
    update <- DB.Mongo.collection_update collection "{\"name\":\"burc\"}" "{\"$set\":{\"age\": 55}}"
    -- get a cursor
    cursor2 <- DB.Mongo.collection_find collection "{\"name\":\"burc\"}" Nothing
    -- should show the updated record
    DB.Mongo.cursor_next cursor2 >>= printLn
    -- no record
    DB.Mongo.cursor_next cursor2 >>= printLn
    -- remove anything with the following
    r <- DB.Mongo.collection_remove collection "{\"name\":\"burc\"}"
    -- close cursor
    DB.Mongo.cursor_destroy cursor2
    -- close collection
    DB.Mongo.collection_destroy collection
    -- close database
    DB.Mongo.client_destroy db
    -- clean up memory
    printLn("Done Nice Path")



namespace Main
  main : IO ()
  main = do
    -- init library
    DB.Mongo.init
    nice_path
    connection_failure_path
    DB.Mongo.cleanup
    printLn("Done")
