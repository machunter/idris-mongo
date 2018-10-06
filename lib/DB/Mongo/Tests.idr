module DB.Mongo.Tests

import DB.Mongo
import DB.Mongo.Bson
import DB.Mongo.Definitions
import Language.JSON
import Language.JSON.Helpers

%access export

server_uri : String
server_uri = "mongodb://127.0.0.1:27017"

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
check_insertAndFind (DBResultJSON json, CurrentState s) = let (JString result) = value json "name" in
    if result == "burc1" then printLn("Success insertAndFind!") else printLn("Failed insertAndFind!")

dropDatabase : DBState State DBResult
dropDatabase = do
  initDB
  get_collection "testdb1" "testcoll2"
  collection_insert "{\"name\":\"sulu\",\"age\":50}"
  collection_destroy

check_dropDatabase : (DBResult, State) -> IO ()
check_dropDatabase (_, CurrentState (p, _, _, _)) = printLn("Success dropDatabase!")

removeRecord : DBState State DBResult
removeRecord = do
    initDB
    get_collection "testdb" "testcoll"
    collection_insert "{\"name\":\"burc1\",\"age\":50}"
    collection_insert "{\"name\":\"burc1\",\"age\":35}"
    collection_remove "{\"name\":\"burc1\"}"

check_removeRecord : (DBResult, State) -> IO ()
check_removeRecord (DBResultCount count, CurrentState (Just p, _, _, _)) = if count == 1 then printLn("Success removeRecord!", count) else printLn("Failed removeRecord")
check_removeRecord (_, CurrentState (p, _, _, _)) = printLn("Failed removeRecord!")


test1 : IO ()
test1 = check_insertAndFind (run insertAndFind (initialState DebugModeOff))

test2 : IO ()
test2 = check_dropDatabase (run dropDatabase (initialState DebugModeOff))

test3 : IO ()
test3 = check_removeRecord (run removeRecord (initialState DebugModeOn))
