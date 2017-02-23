module DB.Mongo.Tests
import DB.Mongo
import DB.Mongo.Bson

namespace Main
  main : IO ()
  main = do
    printLn("Starting")
    DB.Mongo.init
    handle <- DB.Mongo.client_new "mongodb://127.0.0.1:27017"
    DB.Mongo.client_set_error_api handle 2
    collection <- DB.Mongo.client_get_collection handle "testdb" "testcoll"
    DB.Mongo.collection_insert collection "{\"name\":\"burc\",\"age\":50}"
    cursor <- DB.Mongo.collection_find collection "{\"name\":\"burc\",\"age\":50}" Nothing
    doc <- DB.Mongo.cursor_next cursor
    printLn(doc)
    update <- DB.Mongo.collection_update collection "{\"name\":\"burc\"}" "{\"$set\":{\"age\": 55}}"
    cursor2 <- DB.Mongo.collection_find collection "{\"name\":\"burc\"}" Nothing
    doc2 <- DB.Mongo.cursor_next cursor2
    json_string2 <- DB.Mongo.Bson.as_json doc2
    printLn(json_string2)
    r <- DB.Mongo.collection_remove collection "{\"name\":\"burc\"}"
    DB.Mongo.Bson.destroy doc
    DB.Mongo.cursor_destroy cursor
    DB.Mongo.cursor_destroy cursor2
    DB.Mongo.collection_destroy collection
    DB.Mongo.client_destroy handle
    DB.Mongo.cleanup
    printLn("Done")
