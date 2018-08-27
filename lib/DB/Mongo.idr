module DB.Mongo
import CFFI
import DB.Mongo.CImports as Imports
import Control.Monad.State
import DB.Mongo.Bson
import DB.Mongo.Definitions


public export
data DBState : (stateType : Type) -> (valueType : Type ) -> Type where
  GetDBState : DBState stateType stateType
  PutDBState : stateType -> DBState stateType DBResult
  BindDBState : DBState stateType valueTypeA -> (valueTypeA -> DBState stateType valueTypeB) -> DBState stateType valueTypeB
  PureDBState : valueType -> DBState stateType valueType

namespace DoBindDBState
  (>>=) : DBState stateType valueTypeA -> (valueTypeA -> DBState stateType valueTypeB) -> DBState stateType valueTypeB
  (>>=) = BindDBState

mutual
  export
  Functor (DBState stateType) where
    map func x = do
      val <- x
      PureDBState (func val)

  export
  Applicative (DBState stateType) where
    pure = PureDBState
    (<*>) f a = do
      f' <- f
      a' <- a
      pure (f' a')

  export
  Monad (DBState stateType) where
    (>>=) = BindDBState

export
run : DBState stateType a -> (st: stateType) -> (a, stateType)
run GetDBState state = (state, state)
run (PutDBState newState) st = (DBResultIO (pure ()), newState)
run (BindDBState cmd prog) state = let (val, nextState) = run cmd state in run (prog val) nextState
run (PureDBState newState) state = (newState, state)

export
initialState : State
initialState = CurrentState ("IntialState", Nothing, Nothing, Nothing)

export
initState : DBState State DBResult
initState = PutDBState initialState

||| initializes the mongo driver, must be called before starting
export
init : DBState State DBResult
init = do
    CurrentState (last_state, conn, coll, cursor) <- GetDBState
    PureDBState (Imports.init)
    PutDBState (CurrentState (last_state ++ ">>init", conn, coll, cursor))


||| cleans up the driver, must be called at the end
export
cleanup : DBState State DBResult
cleanup = do
  CurrentState (last_state, conn, coll, cursor) <- GetDBState
  PureDBState (Imports.cleanup)
  PutDBState (CurrentState (last_state ++ ">>cleanup", conn, coll, cursor))


||| creates a new client (db connection) which must be destroyed by calling client_destroy
||| @uri the mongodb compatible uri of the form mongodb://x.x.x.x:port
export
dbConnect : (uri : String) -> DBState State DBResult
dbConnect uri = do
  CurrentState (last_state, _, _, _) <- GetDBState
  case Imports.client_new uri of
    DBResultConnection (Connection connectionPtr) => PutDBState (CurrentState (last_state ++ ">>client_new", Just (Connection connectionPtr), Nothing, Nothing))


||| destroys the db client
export
client_destroy : DBState State DBResult
client_destroy = do
  CurrentState (last_state, (Just connection), _, _) <- GetDBState
  case Imports.client_destroy connection of
    _ => PutDBState (CurrentState (last_state ++ ">>client_destroy", Nothing, Nothing, Nothing))


||| returns a reference to a collection in the database
||| @db_name the name of the actual database
||| @collection_name the name of the collection
export
get_collection : (db_name : String) -> (collection_name : String) -> DBState State DBResult
get_collection db_name collection_name = do
  CurrentState (last_state, (Just connection), _, _) <- GetDBState
  case Imports.client_get_collection connection db_name collection_name of
    (DBResultCollection x) => PutDBState (CurrentState (last_state ++ ">> client_get_collection", (Just connection), Just x, Nothing))


||| inserts a stringified json object into a , return true if successful
||| @document a valid stringified json object
export
collection_insert : (document : String) -> DBState State DBResult
collection_insert document = do
  CurrentState (last_state, (Just connection), (Just collection), _) <- GetDBState
  let bsonDoc = new_from_json document
  pure (Imports.collection_insert collection bsonDoc)
  pure (destroy bsonDoc)
  PutDBState (CurrentState (last_state ++ "\n\r >>collection_insert", (Just connection),(Just collection), Nothing))


||| queries the collection and returns a reference to a cursor which can be used to retrieve documents
||| see: http://mongoc.org/libmongoc/current/mongoc_collection_find_with_opts.html
||| @filter a valid stringified json query object
||| @options a valid stringified json options object
export
collection_find : (filter : String) -> (options : Maybe String) -> DBState State DBResult
collection_find filter options = do
   CurrentState (last_state, (Just connection), (Just collection), _) <- GetDBState
   let filter_bson = new_from_json filter
   let opts_bson = case options of
     Nothing => new_from_json "{}"
     Just options_string => new_from_json options_string
   let (DBResultCursor dbCursor) = Imports.collection_find collection (Query filter_bson) (Options opts_bson)
   pure (destroy filter_bson)
   pure (destroy opts_bson)
   PutDBState (CurrentState(last_state ++ "\n >>collection_find", (Just connection), (Just collection), (Just dbCursor)))


||| sets the mongo driver's error level
||| @error_level the error level to set
export
client_set_error_api : (error_level : Int) -> DBState State DBResult
client_set_error_api error_level = do
  CurrentState (last_state, (Just (Connection connection)), collection, cursor) <- GetDBState
  let result = Imports.set_error_api connection error_level
  PutDBState(CurrentState (last_state ++ "\n >>client_set_error_api", (Just (Connection connection)), collection, cursor))

||| iterates through a cursor and returns a Maybe DBDoc
||| see http://mongoc.org/libmongoc/current/mongoc_cursor_next.html
||| @cursor the cursor
export
cursor_next : (cursor : DBCursor) -> IO (Maybe DBDoc)
-- cursor_next (MkDBCursor cursor_handle) = do
--   Just value  <- _cursor_next cursor_handle | pure Nothing
--   next <- DB.Mongo.Bson.as_json value
--   let result = (Just (MkDBDoc next))
--   DB.Mongo.Bson.destroy value
--   pure result


||| removes documents matching the query, returns true if deletion occured
||| @collection the collection to query
||| @selector a valid stringified json query
export
collection_remove : (collection : DBCollection) -> (selector : String) -> IO (Maybe Bool)
-- collection_remove (MkDBCollection collection_handle) selector = do
--   bson_selector <- DB.Mongo.Bson.new_from_json (Just selector)
--   result <- _collection_remove collection_handle bson_selector
--   DB.Mongo.Bson.destroy bson_selector
--   pure result


||| update the first document matching the query
||| @collection the collection to search
||| @query the query to match
||| @update the update stringified json object
export
collection_find_and_modify : (collection : DBCollection) -> (query : String) -> (update : String) -> IO (Maybe Bool)
-- collection_find_and_modify (MkDBCollection collection_handle) query update = do
--   query_bson <- DB.Mongo.Bson.new_from_json (Just query)
--   update_bson <- DB.Mongo.Bson.new_from_json (Just update)
--   result <- _collection_find_and_modify collection_handle query_bson update_bson
--   DB.Mongo.Bson.destroy query_bson
--   DB.Mongo.Bson.destroy update_bson
--   pure result


||| destroys the collection reference
export
collection_destroy : DBState State ()
-- collection_destroy = do
--   collection <- GetDBCollection
--   foreign FFI_C "mongoc_" (Ptr -> IO()) (handle collection)

  -- DBCollectionBind GetCollection
  --   (\collection =>
  --      IOBindIODatabase (foreign FFI_C "mongoc_" (Ptr -> IO()) (handle collection))
  --        (\_ => PureIODatabase ()))


||| destroys the cursor reference
||| @cursor the cursor reference
export
cursor_destroy : (cursor : DBCursor) -> IO ()
-- cursor_destroy (MkDBCursor cursor_handle) = foreign FFI_C "mongoc_cursor_destroy" (Ptr -> IO()) cursor_handle


export
collection_update : (collection : DBCollection) -> (selector: String) -> (update : String) -> (flags : DBUpdateFlags) -> IO (Maybe Bool)
-- collection_update (MkDBCollection collection_handle) selector update flags = do
--   selector_bson <- DB.Mongo.Bson.new_from_json (Just selector)
--   update_bson <- DB.Mongo.Bson.new_from_json (Just update)
--   result <- _collection_update collection_handle selector_bson update_bson flags
--   DB.Mongo.Bson.destroy selector_bson
--   DB.Mongo.Bson.destroy update_bson
--   pure result
