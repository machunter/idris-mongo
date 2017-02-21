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
    DB.Mongo.collection_insert collection "{\"name\":\"burc\",\"age\":50}" null
    q <- DB.Mongo.Bson.new_from_json "{\"name\":\"burc\",\"age\":50}"
    cursor <- DB.Mongo.collection_find collection q null
    doc <- DB.Mongo.cursor_next cursor
    json_string <- DB.Mongo.Bson.as_json doc
    printLn(json_string)
    DB.Mongo.Bson.destroy doc
    DB.Mongo.Bson.destroy q
    DB.Mongo.client_destroy handle
    DB.Mongo.cleanup
    printLn("Done")
