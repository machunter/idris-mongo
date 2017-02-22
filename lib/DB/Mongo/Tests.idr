module DB.Mongo.Tests
import DB.Mongo
import DB.Mongo.Bson

namespace Main
  main : IO ()
  main = do
    printLn("Starting")
    DB.Mongo.init
    handle <- DB.Mongo.client_new "mongodb://192.168.99.100:27017"
    DB.Mongo.client_set_error_api handle 2
    collection <- DB.Mongo.client_get_collection handle "testdb" "testcoll"
    DB.Mongo.collection_insert collection "{\"name\":\"burc\",\"age\":50}"
    q <- DB.Mongo.Bson.new_from_json "{\"name\":\"burc\",\"age\":50}"
    cursor <- DB.Mongo.collection_find collection q null
    doc <- DB.Mongo.cursor_next cursor
    json_string <- DB.Mongo.Bson.as_json doc
    printLn(json_string)
    update <- DB.Mongo.collection_update collection "{\"name\":\"burc\"}" "{\"$set\":{\"age\": 55}}"
    q2 <- DB.Mongo.Bson.new_from_json "{\"name\":\"burc\"}"
    cursor2 <- DB.Mongo.collection_find collection q2 null
    doc2 <- DB.Mongo.cursor_next cursor2
    json_string2 <- DB.Mongo.Bson.as_json doc2
    printLn(json_string2)
    r <- DB.Mongo.collection_remove collection "{\"name\":\"burc\"}"
    DB.Mongo.Bson.destroy doc
    DB.Mongo.Bson.destroy q
    DB.Mongo.client_destroy handle
    DB.Mongo.cleanup
    printLn("Done")
