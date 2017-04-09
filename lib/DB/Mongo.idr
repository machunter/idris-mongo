module DB.Mongo
import CFFI
import DB.Mongo.Bson
import Control.Monad.State

%lib     C "bson-1.0.0"
%lib     C "mongoc-1.0.0"
%link    C "mongolib.o"
%include C "mongolib.h"



_collection_insert : Ptr -> BSON -> IO (Maybe Bool)
_collection_insert  collection (MkBSON bson_handle) = do
    result <- foreign FFI_C "_collection_insert" (Ptr -> Ptr -> IO Int) collection bson_handle
    if (result == 0) then pure Nothing else pure (Just True)

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

_collection_find : Ptr -> BSON -> BSON -> IO Ptr
_collection_find collection (MkBSON filter_handle) (MkBSON opts_handle)  =
  foreign FFI_C "_collection_find" (Ptr -> Ptr -> Ptr -> IO Ptr) collection filter_handle opts_handle

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
record DBConnection where
  constructor MkDBConnection
  handle : Ptr

export
record DBCollection where
  constructor MkDBCollection
  handle : Ptr

export
data DBConnectionState : Type -> Type where
  GetConnection : DBConnectionState DBConnection
  PutConnection : DBConnection -> DBConnectionState ()
  ConnBind : DBConnectionState a -> (a -> DBConnectionState b) -> DBConnectionState b
  IOBindDBConnection : IO a -> (a -> DBConnectionState b) -> DBConnectionState b
  PureConnectionState : ty -> DBConnectionState ty

namespace DBConnectionStateDo
  (>>=) : DBConnectionState a -> (a -> DBConnectionState b) -> DBConnectionState b
  (>>=) = ConnBind

namespace DBConnectionStateChainer
  (>>=) : IO a -> (a -> DBConnectionState b) -> DBConnectionState b
  (>>=) = IOBindDBConnection

export
data DBCollectionState : Type -> Type where
  GetCollection : DBCollectionState DBCollection
  PutCollection : DBCollection -> DBCollectionState ()
  CollectionBind : DBCollectionState a -> (a -> DBCollectionState b) -> DBCollectionState b
  IOBindDBCollection : IO a -> (a -> DBCollectionState b) -> DBCollectionState b
  DBConnectionBindDBCollection : DBConnectionState a -> (a -> DBCollectionState b) -> DBCollectionState b
  PureCollectionState : ty -> DBCollectionState ty

namespace CollectionBindDo
  (>>=) : DBCollectionState a -> (a -> DBCollectionState b) -> DBCollectionState b
  (>>=) = CollectionBind

namespace IOBindDBCollectionDo
  (>>=) : IO a -> (a -> DBCollectionState b) -> DBCollectionState b
  (>>=) = IOBindDBCollection

namespace DBConnectionBindDBCollectionDo
  (>>=) : DBConnectionState a -> (a -> DBCollectionState b) -> DBCollectionState b
  (>>=) = DBConnectionBindDBCollection


export
data IODatabase : Type -> Type where
  Bind : IODatabase a -> (a -> IODatabase b) -> IODatabase b
  DBConnectionBindIODatabaseBind : DBConnectionState a -> (a -> IODatabase b) -> IODatabase b
  DBCollectionBindIO : DBCollectionState a -> (a -> IO b) -> IODatabase b
  DBCollectionBind : DBCollectionState a -> (a -> IODatabase b) -> IODatabase b
  IOBindIODatabase : IO a -> (a -> IODatabase b) -> IODatabase b
  PureIODatabase : ty -> IODatabase ty

mutual
  Functor IODatabase where
    map func x = do
      val <- x
      pure (func val)

  Applicative IODatabase where
    pure = PureIODatabase
    (<*>) f a = do
      f' <- f
      a' <- a
      pure (f' a')

  export
  Monad IODatabase where
    (>>=) = Bind

namespace IODatabaseBindDo
  (>>=) : IODatabase a -> (a -> IODatabase b) -> IODatabase b
  (>>=) = Bind

namespace DBConnectionStateBindDo
  (>>=) : DBConnectionState a -> (a -> IODatabase b) -> IODatabase b
  (>>=) = DBConnectionBindIODatabaseBind

namespace IOBindIODatabaseDo
  (>>=) : IO a -> (a -> IODatabase b) -> IODatabase b
  (>>=) = IOBindIODatabase

namespace DBCollectionBindIODo
  (>>=) : DBCollectionState a -> (a -> IO b) -> IODatabase b
  (>>=) = DBCollectionBindIO

namespace DBCollectionBindDo
  (>>=) : DBCollectionState a -> (a -> IODatabase b) -> IODatabase b
  (>>=) = DBCollectionBind

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

||| initializes the mongo driver, must be called before starting
export
init : IO ()
init = foreign FFI_C "mongoc_init" (IO ())

||| cleans up the driver, must be called at the end
export
cleanup : IO ()
cleanup = foreign FFI_C "mongoc_cleanup" (IO ())

||| creates a new client (db connection) which must be destroyed by calling client_destroy
||| @uri the mongodb compatible uri of the form mongodb://x.x.x.x:port
export
client_new : (uri : String) -> IODatabase ()
-- client_new uri = do
--                   connection_handle <- foreign FFI_C "mongoc_client_new" (String -> IO Ptr) uri
--                   put (MkDBConnection connection_handle)
--                   pure ()
client_new uri = do
  connection_handle <- foreign FFI_C "mongoc_client_new" (String -> IO Ptr) uri
  PutConnection (MkDBConnection connection_handle)
  printLn("PutConnection")
  PureIODatabase ()


||| destroys the db client
export
client_destroy : IODatabase()
client_destroy = do
  connection <- GetConnection
  foreign FFI_C "mongoc_client_destroy" (Ptr -> IO ()) (handle connection)
  PureIODatabase ()

||| sets the mongo driver's error level
||| @db_client the database client
||| @error_level the error level to set
export
client_set_error_api : (db_client : DBConnection) -> (error_level : Int) -> IO ()
-- client_set_error_api (MkDBConnection connection_handle) error_level = foreign FFI_C "mongoc_client_set_error_api" (Ptr -> Int -> IO ()) connection_handle error_level

||| returns a reference to a collection in the database
||| @db_name the name of the actual database
||| @collection_name the name of the collection
export
client_get_collection : (db_name : String) -> (collection_name : String) -> IODatabase ()
client_get_collection db_name collection_name = do
    conn <- GetConnection
    coll <- foreign FFI_C "_client_get_collection" (Ptr -> String -> String -> IO Ptr) (handle conn) db_name collection_name
    PutCollection (MkDBCollection coll)
    PureIODatabase ()

    -- DBConnectionBindDBCollection GetConnection (\conn =>
    --   IOBindDBCollection
    --     (foreign FFI_C "_client_get_collection" (Ptr -> String -> String -> IO Ptr) (connection conn) db_name collection_name)
    --     (\x => PutCollection (MkDBCollection x))
    --   )
--    PutCollection (MkDBCollection collection_handle)

--  connection <- GetConnection
--   pure (connect_handle)
--  collection_handle <- foreign FFI_C "_client_get_collection" (Ptr -> String -> String -> IO Ptr) (connection connection_handle) db_name collection_name
--  pure (MkDBCollection collection_handle)

||| inserts a stringified json object into a , return true if successful
||| @document a valid stringified json object
export
collection_insert : (document : String) -> IODatabase (Maybe Bool)
collection_insert document = do
  d <- DB.Mongo.Bson.new_from_json (Just document)
  collection <- GetCollection
  result <-  _collection_insert (handle collection) d
  DB.Mongo.Bson.destroy d
  PureIODatabase result

||| queries the collection and returns a reference to a cursor which can be used to retrieve documents
||| see: http://mongoc.org/libmongoc/current/mongoc_collection_find_with_opts.html
||| @collection a reference to the desired
||| @filter a valid stringified json query object
||| @options a valid stringified json options object
export
collection_find : (collection : DBCollection) -> (filter : String) -> (options : Maybe String) -> IO DBCursor
collection_find (MkDBCollection collection_handle) filter options = do
  filter_bson <- DB.Mongo.Bson.new_from_json (Just filter)
  opts_bson <- DB.Mongo.Bson.new_from_json options
  cursor <- _collection_find collection_handle filter_bson opts_bson
  DB.Mongo.Bson.destroy filter_bson
  DB.Mongo.Bson.destroy opts_bson
  pure (MkDBCursor cursor)

||| iterates through a cursor and returns a Maybe DBDoc
||| see http://mongoc.org/libmongoc/current/mongoc_cursor_next.html
||| @cursor the cursor
export
cursor_next : (cursor : DBCursor) -> IO (Maybe DBDoc)
cursor_next (MkDBCursor cursor_handle) = do
  Just value  <- _cursor_next cursor_handle | pure Nothing
  next <- DB.Mongo.Bson.as_json value
  let result = (Just (MkDBDoc next))
  DB.Mongo.Bson.destroy value
  pure result

||| removes documents matching the query, returns true if deletion occured
||| @collection the collection to query
||| @selector a valid stringified json query
export
collection_remove : (collection : DBCollection) -> (selector : String) -> IO (Maybe Bool)
collection_remove (MkDBCollection collection_handle) selector = do
  bson_selector <- DB.Mongo.Bson.new_from_json (Just selector)
  result <- _collection_remove collection_handle bson_selector
  DB.Mongo.Bson.destroy bson_selector
  pure result

||| update the first document matching the query
||| @collection the collection to search
||| @query the query to match
||| @update the update stringified json object
export
collection_find_and_modify : (collection : DBCollection) -> (query : String) -> (update : String) -> IO (Maybe Bool)
collection_find_and_modify (MkDBCollection collection_handle) query update = do
  query_bson <- DB.Mongo.Bson.new_from_json (Just query)
  update_bson <- DB.Mongo.Bson.new_from_json (Just update)
  result <- _collection_find_and_modify collection_handle query_bson update_bson
  DB.Mongo.Bson.destroy query_bson
  DB.Mongo.Bson.destroy update_bson
  pure result

||| destroys the collection reference
export
collection_destroy : IODatabase ()
collection_destroy = do
  collection <- GetCollection
  foreign FFI_C "mongoc_" (Ptr -> IO()) (handle collection)
  PureIODatabase ()

  -- DBCollectionBind GetCollection
  --   (\collection =>
  --      IOBindIODatabase (foreign FFI_C "mongoc_" (Ptr -> IO()) (handle collection))
  --        (\_ => PureIODatabase ()))


||| destroys the cursor reference
||| @cursor the cursor reference
export
cursor_destroy : (cursor : DBCursor) -> IO ()
cursor_destroy (MkDBCursor cursor_handle) = foreign FFI_C "mongoc_cursor_destroy" (Ptr -> IO()) cursor_handle

export
collection_update : (collection : DBCollection) -> (selector: String) -> (update : String) -> (flags : DBUpdateFlags) -> IO (Maybe Bool)
collection_update (MkDBCollection collection_handle) selector update flags = do
  selector_bson <- DB.Mongo.Bson.new_from_json (Just selector)
  update_bson <- DB.Mongo.Bson.new_from_json (Just update)
  result <- _collection_update collection_handle selector_bson update_bson flags
  DB.Mongo.Bson.destroy selector_bson
  DB.Mongo.Bson.destroy update_bson
  pure result
