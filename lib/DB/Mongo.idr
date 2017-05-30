module DB.Mongo
import CFFI
import DB.Mongo.Bson
import Control.Monad.State

%lib     C "bson-1.0.0"
%lib     C "mongoc-1.0.0"
%link    C "mongolib.o"
%include C "mongolib.h"

export
record DBConnection where
  constructor MkDBConnection
  connection_handle : Ptr

export
record DBCollection where
  constructor MkDBCollection
  collection_handle : Ptr

public export
State : Type
State = (String, DBConnection, DBCollection)

public export
data DBResult = DBNothing | DBPtr Ptr | DBCount Int | DBIO ()

public export
data DBState : (stateType : Type) -> (ty : Type ) -> Type where
  GetDBState : DBState stateType stateType
  PutDBState : stateType -> DBState stateType (IO DBResult)
  DBStateBind : DBState stateType a -> (a -> DBState stateType b) -> DBState stateType b
  PureDB : ty -> DBState stateType ty

namespace DBStateDoBind
  (>>=) : DBState stateType a -> (a -> DBState stateType b) -> DBState stateType b
  (>>=) = DBStateBind

mutual
  export
  Functor (DBState stateType) where
    map func x = do
      val <- x
      PureDB (func val)

  export
  Applicative (DBState stateType) where
    pure = PureDB
    (<*>) f a = do
      f' <- f
      a' <- a
      pure (f' a')



  export
  Monad (DBState stateType) where
    (>>=) = DBStateBind

export
run : DBState stateType a -> (st: stateType) -> (a, stateType)
run GetDBState state = (state, state)
run (PutDBState newState) st = (pure(DBIO ()), newState)
run (DBStateBind cmd prog) state = let (val, nextState) = run cmd state in run (prog val) nextState
run (PureDB newState) state = (newState, state)

export
initialState : State
initialState = ("IntialState", MkDBConnection null, MkDBCollection null)

_write_concern_new : IO Ptr
_write_concern_new = foreign FFI_C "mongoc_write_concern_new" (IO Ptr)

_write_concern_destroy : Ptr -> IO ()
_write_concern_destroy concern_ptr = foreign FFI_C "mongoc_write_concern_destroy" (Ptr -> IO ()) concern_ptr

_collection_remove : Ptr -> BSON -> IO (Maybe Bool)
_collection_remove collection (MkBSON selector_handle) = do
  result <- foreign FFI_C "_collection_remove" (Ptr -> Ptr -> IO Int) collection selector_handle
  if (result == 0) then pure Nothing else pure (Just True)

_collection_find_and_modify : Ptr -> BSON -> BSON -> IO (Maybe Bool)
_collection_find_and_modify collection (MkBSON query_handle) (MkBSON update_handle) = do
  result <- foreign FFI_C "_collection_find_and_modify" (Ptr -> Ptr -> Ptr -> IO Int) collection query_handle update_handle
  if (result == 0) then pure Nothing else pure (Just True)


_cursor_next : Ptr -> IO (Maybe BSON)
_cursor_next cursor = do
  doc_handle <- foreign FFI_C "_cursor_next" (Ptr -> IO Ptr) cursor
  if (doc_handle == null) then pure Nothing else pure (Just (MkBSON doc_handle))

_collection_update : Ptr -> BSON -> BSON -> Int -> IO (Maybe Bool)
_collection_update collection (MkBSON selector_handle) (MkBSON update_handle) update_flags = do
  result <- foreign FFI_C "_collection_update" (Ptr -> Ptr -> Ptr -> Int -> IO Int) collection selector_handle update_handle update_flags
  if (result == 0) then pure Nothing else pure (Just True)


export
data DBDoc : Type where
  MkDBDoc: (bson_string: String) -> DBDoc

export
data DBCursor : Type where
  MkDBCursor : (cursor : Ptr) -> DBCursor

export
DBUpdateFlags : Type
DBUpdateFlags = Int

export
MONGOC_UPDATE_NONE : DBUpdateFlags
MONGOC_UPDATE_NONE = 0

export
MONGOC_UPDATE_UPSERT : DBUpdateFlags
MONGOC_UPDATE_UPSERT = 1

export
MONGOC_UPDATE_MULTI_UPDATE : DBUpdateFlags
MONGOC_UPDATE_MULTI_UPDATE = 2


export
Show DBDoc where
  show (MkDBDoc bson_string) = bson_string

export
showState : State -> String
showState (a, b) = a


export
initState : DBState State (IO DBResult)
initState = PutDBState (pure(initialState))

_init : DBResult
_init = DBPtr (unsafePerformIO (foreign FFI_C "_init" (IO Ptr)))

||| initializes the mongo driver, must be called before starting
export
init : DBState State DBResult
init = do
    (last_state, conn, coll) <- GetDBState
    pure _init
    PutDBState (last_state ++ ">>init", conn, coll)


_cleanup : DBResult
_cleanup = DBIO ( unsafePerformIO (foreign FFI_C "mongoc_cleanup" (IO ())))

||| cleans up the driver, must be called at the end
export
cleanup : DBState State DBResult
cleanup = do
  (last_state, conn, coll) <- GetDBState
  pure _cleanup
  PutDBState ("_init >> " ++ last_state, conn, coll)

_client_new : (uri : String) -> DBResult
_client_new uri = DBPtr (unsafePerformIO (foreign FFI_C "mongoc_client_new" (String -> IO Ptr) uri))

||| creates a new client (db connection) which must be destroyed by calling client_destroy
||| @uri the mongodb compatible uri of the form mongodb://x.x.x.x:port
export
client_new : (uri : String) -> DBState State DBResult
client_new uri = do
  (last_state, _, _) <- GetDBState
  case _client_new uri of
    DBNothing => PutDBState (last_state, MkDBConnection null, MkDBCollection null)
    DBPtr connection_ptr => PutDBState (last_state, MkDBConnection connection_ptr, MkDBCollection null)


_client_destroy : (connection : DBConnection) -> DBResult
_client_destroy (MkDBConnection connection_handle) = do
  DBIO (unsafePerformIO (foreign FFI_C "mongoc_client_destroy" (Ptr -> IO ()) connection_handle))


||| destroys the db client
export
client_destroy : DBState State DBResult
client_destroy = do
  (last_state, connection, _) <- GetDBState
  case _client_destroy connection of
    _ => PutDBState (last_state ++ ">>client_destroy", MkDBConnection null, MkDBCollection null)



_client_get_collection : (connection : DBConnection) -> (db_name : String) -> (collection_name: String) -> DBResult
_client_get_collection (MkDBConnection connection_handle) db_name collection_name =
  let result = unsafePerformIO (foreign FFI_C "_client_get_collection" (Ptr -> String -> String -> IO Ptr) connection_handle db_name collection_name)
  in if result == null then DBIO() else DBPtr result

||| returns a reference to a collection in the database
||| @db_name the name of the actual database
||| @collection_name the name of the collection
export
client_get_collection : (db_name : String) -> (collection_name : String) -> DBState State DBResult
client_get_collection db_name collection_name = do
  (last_state, connection, _) <- GetDBState
  case _client_get_collection connection db_name collection_name of
      DBNothing => PutDBState(last_state, connection, MkDBCollection null)
      DBPtr collection_ptr => PutDBState(last_state ++ ">>client_get_collection", connection, MkDBCollection collection_ptr)


_collection_insert : DBCollection -> BSON -> DBResult
_collection_insert (MkDBCollection collection_handle) (MkBSON bson_document) =
    let result = unsafePerformIO (foreign FFI_C "_collection_insert" (Ptr -> Ptr -> IO Int) collection_handle bson_document) in
    if result == 0 then DBIO() else DBCount result

||| inserts a stringified json object into a , return true if successful
||| @document a valid stringified json object
export
collection_insert : (document : String) -> DBState State DBResult
collection_insert document = do
  (last_state, connection, collection) <- GetDBState
  -- bsonDoc <- new_from_json document
  pure (_collection_insert collection (new_from_json document))
  -- destroy bsonDoc
  PutDBState (last_state ++ ">>collection_insert", connection, collection)

||| queries the collection and returns a reference to a cursor which can be used to retrieve documents
||| see: http://mongoc.org/libmongoc/current/mongoc_collection_find_with_opts.html
||| @filter a valid stringified json query object
||| @options a valid stringified json options object
export
collection_find : (filter : String) -> (options : Maybe String) -> DBState State (IO DBResult)
-- collection_find (MkDBCollection collection_handle) filter options = do
--   filter_bson <- DB.Mongo.Bson.new_from_json (Just filter)
--   opts_bson <- DB.Mongo.Bson.new_from_json options
--   cursor <- _collection_find collection_handle filter_bson opts_bson
--   DB.Mongo.Bson.destroy filter_bson
--   DB.Mongo.Bson.destroy opts_bson
--   pure (MkDBCursor cursor)
-- collection_find filter options = do
--   (last_state, connection, collection) <- GetDBState
--   pure (_collection_find (collection_handle collection) (DB.Mongo.Bson.new_from_json (Just filter)) (DB.Mongo.Bson.new_from_json options))


||| sets the mongo driver's error level
||| @db_client the database client
||| @error_level the error level to set
export
client_set_error_api : (db_client : DBConnection) -> (error_level : Int) -> IO ()
-- client_set_error_api (MkDBConnection connection_handle) error_level = foreign FFI_C "mongoc_client_set_error_api" (Ptr -> Int -> IO ()) connection_handle error_level




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
