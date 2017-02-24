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

_collection_find : Ptr -> Ptr -> Ptr -> IO Ptr
_collection_find collection filter opts  = foreign FFI_C "_collection_find" (Ptr -> Ptr -> Ptr -> IO Ptr) collection filter opts

_cursor_next : Ptr -> IO Ptr
_cursor_next cursor = foreign FFI_C "_cursor_next" (Ptr -> IO Ptr) cursor

export
data DBDoc : Type where
  MkDBDoc: (bson_string: String) -> DBDoc

bson_string : DBDoc -> String
bson_string (MkDBDoc bson_string) = bson_string



export
Show DBDoc where
  show (MkDBDoc bson_string) = bson_string

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
cursor_next : Ptr -> IO (Maybe DBDoc)
cursor_next cursor = do
  res <- _cursor_next cursor
  if (res == null) then
    pure Nothing
  else do
        next <- DB.Mongo.Bson.as_json res
        pure (Just (MkDBDoc next))

export
collection_remove : Ptr -> String -> IO Bool
collection_remove collection selector = do
  s <- DB.Mongo.Bson.new_from_json selector
  z <- _collection_remove collection s
  DB.Mongo.Bson.destroy s
  case z of
    0 => pure False
    _ => pure True

export
collection_update : Ptr -> String -> String -> IO (Maybe Bool)
collection_update collection query update = do
  query_bson <- DB.Mongo.Bson.new_from_json query
  update_bson <- DB.Mongo.Bson.new_from_json update
  result <- _collection_update collection query_bson update_bson
  DB.Mongo.Bson.destroy query_bson
  DB.Mongo.Bson.destroy update_bson
  case result of
    0 => pure Nothing
    1 => pure (Just True)

export
collection_destroy : Ptr -> IO()
collection_destroy collection = foreign FFI_C "mongoc_collection_destroy" (Ptr -> IO()) collection

export
cursor_destroy : Ptr -> IO ()
cursor_destroy cursor = foreign FFI_C "mongoc_cursor_destroy" (Ptr -> IO()) cursor

export
collection_find : Ptr -> String -> Maybe String -> IO Ptr
collection_find collection filter opts = do
  filter_bson <- DB.Mongo.Bson.new_from_json filter
  opts_bson <- case opts of
                  Nothing => pure null
                  Just json_string => DB.Mongo.Bson.new_from_json json_string
  cursor <- _collection_find collection filter_bson opts_bson
  DB.Mongo.Bson.destroy filter_bson
  case opts of
    Nothing => pure ()
    Just _ => DB.Mongo.Bson.destroy opts_bson
  pure cursor
