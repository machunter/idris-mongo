module DB.Mongo
import CFFI
import DB.Mongo.Bson

%lib     C "bson-1.0.0"
%lib     C "mongoc-1.0.0"
%link    C "mongolib.o"
%include C "mongolib.h"

_collection_insert : Ptr -> Ptr -> IO Int
_collection_insert  collection document = foreign FFI_C "_collection_insert" (Ptr -> Ptr -> IO Int) collection document

_write_concern_new : IO Ptr
_write_concern_new = foreign FFI_C "mongoc_write_concern_new" (IO Ptr)

_write_concern_destroy : Ptr -> IO ()
_write_concern_destroy concern_ptr = foreign FFI_C "mongoc_write_concern_destroy" (Ptr -> IO ()) concern_ptr

_collection_remove : Ptr -> Ptr -> IO Int
_collection_remove collection selector = foreign FFI_C "_collection_remove" (Ptr -> Ptr -> IO Int) collection selector

_collection_update : Ptr -> Ptr -> Ptr -> IO Int
_collection_update collection query update = foreign FFI_C "_collection_find_and_modify" (Ptr -> Ptr -> Ptr -> IO Int) collection query update

export
init : IO ()
init = foreign FFI_C "mongoc_init" (IO ())

export
cleanup : IO ()
cleanup = foreign FFI_C "mongoc_cleanup" (IO ())

export
client_new : String -> IO Ptr
client_new uri = foreign FFI_C "mongoc_client_new" (String -> IO Ptr) uri

export
client_destroy : Ptr -> IO()
client_destroy handle = foreign FFI_C "mongoc_client_destroy" (Ptr -> IO ()) handle

export
client_set_error_api : Ptr -> Int -> IO ()
client_set_error_api handle version = foreign FFI_C "mongoc_client_set_error_api" (Ptr -> Int -> IO ()) handle version

export
client_get_collection : Ptr -> String -> String -> IO Ptr
client_get_collection handle db collection = foreign FFI_C "mongoc_client_get_collection" (Ptr -> String -> String -> IO Ptr) handle db collection

export
collection_insert : Ptr -> String -> IO Bool
collection_insert collection document = do
  d <- DB.Mongo.Bson.new_from_json document
  z <-  _collection_insert collection d
  DB.Mongo.Bson.destroy d
  case z of
    0 => pure False
    _ => pure True

export
collection_find : Ptr -> Ptr -> Ptr -> IO Ptr
collection_find collection filter opts  = foreign FFI_C "_collection_find" (Ptr -> Ptr -> Ptr -> IO Ptr) collection filter opts

export
cursor_next : Ptr -> IO Ptr
cursor_next cursor = foreign FFI_C "_cursor_next" (Ptr -> IO Ptr) cursor

export
collection_remove : Ptr -> String -> IO Bool
collection_remove collection selector = do
  s <- DB.Mongo.Bson.new_from_json selector
  z <- _collection_remove collection s
  case z of
    0 => pure False
    _ => pure True

export
collection_update : Ptr -> String -> String -> IO (Maybe Bool)
collection_update collection query update = do
  query_bson <- DB.Mongo.Bson.new_from_json query
  update_bson <- DB.Mongo.Bson.new_from_json update
  result <- _collection_update collection query_bson update_bson
  case result of
    0 => pure Nothing
    1 => pure (Just True)
