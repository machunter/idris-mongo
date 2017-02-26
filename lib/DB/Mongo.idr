module DB.Mongo
import CFFI
import DB.Mongo.Bson

%lib     C "bson-1.0.0"
%lib     C "mongoc-1.0.0"
%link    C "mongolib.o"
%include C "mongolib.h"

_collection_insert : Ptr -> BSON -> IO Int
_collection_insert  collection (MkBSON bson_handle) = foreign FFI_C "_collection_insert" (Ptr -> Ptr -> IO Int) collection bson_handle

_write_concern_new : IO Ptr
_write_concern_new = foreign FFI_C "mongoc_write_concern_new" (IO Ptr)

_write_concern_destroy : Ptr -> IO ()
_write_concern_destroy concern_ptr = foreign FFI_C "mongoc_write_concern_destroy" (Ptr -> IO ()) concern_ptr

_collection_remove : Ptr -> BSON -> IO Int
_collection_remove collection (MkBSON selector_handle) = foreign FFI_C "_collection_remove" (Ptr -> Ptr -> IO Int) collection selector_handle

_collection_update : Ptr -> BSON -> BSON -> IO Int
_collection_update collection (MkBSON query_handle) (MkBSON update_handle) =
  foreign FFI_C "_collection_find_and_modify" (Ptr -> Ptr -> Ptr -> IO Int) collection query_handle update_handle

_collection_find : Ptr -> BSON -> BSON -> IO Ptr
_collection_find collection (MkBSON filter_handle) (MkBSON opts_handle)  =
  foreign FFI_C "_collection_find" (Ptr -> Ptr -> Ptr -> IO Ptr) collection filter_handle opts_handle

_cursor_next : Ptr -> IO (Maybe BSON)
_cursor_next cursor = do
  doc_handle <- foreign FFI_C "_cursor_next" (Ptr -> IO Ptr) cursor
  if (doc_handle == null) then pure Nothing else pure (Just (MkBSON doc_handle))

export
data DBDoc : Type where
  MkDBDoc: (bson_string: String) -> DBDoc

export
data DBConnection : Type where
  MkDBConnection : (connection : Ptr) -> DBConnection

export
data DBCollection : Type where
  MkDBCollection : (collection : Ptr) -> DBCollection

export
data DBCursor : Type where
  MkDBCursor : (cursor : Ptr) -> DBCursor

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
client_new : (uri : String) -> IO DBConnection
client_new uri = do
                  connection_handle <- foreign FFI_C "mongoc_client_new" (String -> IO Ptr) uri
                  pure (MkDBConnection connection_handle)

||| destroys the db client
||| @db_client the client returned by client_new
export
client_destroy : (db_client : DBConnection) -> IO()
client_destroy (MkDBConnection connection_handle) = foreign FFI_C "mongoc_client_destroy" (Ptr -> IO ()) connection_handle

||| sets the mongo driver's error level
||| @db_client the database client
||| @error_level the error level to set
export
client_set_error_api : (db_client : DBConnection) -> (error_level : Int) -> IO ()
client_set_error_api (MkDBConnection connection_handle) error_level = foreign FFI_C "mongoc_client_set_error_api" (Ptr -> Int -> IO ()) connection_handle error_level

||| returns a reference to a collection in the database
||| @db_client the database client
||| @db_name the name of the actual database
||| @collection_name the name of the collection
export
client_get_collection : (db_client : DBConnection) -> (db_name : String) -> (collection_name : String) -> IO DBCollection
client_get_collection (MkDBConnection connection_handle) db_name collection_name = do
  collection_handle <- foreign FFI_C "_client_get_collection" (Ptr -> String -> String -> IO Ptr) connection_handle db_name collection_name
  pure (MkDBCollection collection_handle)

||| inserts a stringified json object into a collection_destroy, return true if successful
||| @collection a reference to the desired collection
||| @document a valid stringified json object
export
collection_insert : (collection : DBCollection) -> (document : String) -> IO (Maybe ())
collection_insert (MkDBCollection collection_handle) document = do
  d <- DB.Mongo.Bson.new_from_json (Just document)
  z <-  _collection_insert collection_handle d
  DB.Mongo.Bson.destroy d
  case z of
    0 => pure Nothing
    _ => pure (Just ())

||| queries the collection and returns a reference to a cursor which can be used to retrieve documents
||| see: http://mongoc.org/libmongoc/current/mongoc_collection_find_with_opts.html
||| @collection a reference to the desired collection_destroy
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
  res <- _cursor_next cursor_handle
  case res of
    Nothing => pure Nothing
    Just value => do
            next <- DB.Mongo.Bson.as_json value
            let result = (Just (MkDBDoc next))
            DB.Mongo.Bson.destroy value
            pure result

||| removes documents matching the query, returns true if deletion occured
||| @collection the collection to query
||| @selector a valid stringified json query
export
collection_remove : (collection : DBCollection) -> (selector : String) -> IO Bool
collection_remove (MkDBCollection collection_handle) selector = do
  s <- DB.Mongo.Bson.new_from_json (Just selector)
  z <- _collection_remove collection_handle s
  DB.Mongo.Bson.destroy s
  case z of
    0 => pure False
    _ => pure True

||| update the first document matching the query
||| @collection the collection to search
||| @query the query to match
||| @update the update stringified json object
export
collection_update : (collection : DBCollection) -> (query : String) -> (update : String) -> IO (Maybe Bool)
collection_update (MkDBCollection collection_handle) query update = do
  query_bson <- DB.Mongo.Bson.new_from_json (Just query)
  update_bson <- DB.Mongo.Bson.new_from_json (Just update)
  result <- _collection_update collection_handle query_bson update_bson
  DB.Mongo.Bson.destroy query_bson
  DB.Mongo.Bson.destroy update_bson
  case result of
    0 => pure Nothing
    1 => pure (Just True)

||| destroys the collection reference
||| @collection the collection reference
export
collection_destroy : (collection : DBCollection) -> IO()
collection_destroy (MkDBCollection collection_handle) = foreign FFI_C "mongoc_collection_destroy" (Ptr -> IO()) collection_handle

||| destroys the cursor reference
||| @cursor the cursor reference
export
cursor_destroy : (cursor : DBCursor) -> IO ()
cursor_destroy (MkDBCursor cursor_handle) = foreign FFI_C "mongoc_cursor_destroy" (Ptr -> IO()) cursor_handle
