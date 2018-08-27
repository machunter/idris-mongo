module DB.Mongo.CImports
import DB.Mongo.Bson
import DB.Mongo.Definitions

%lib     C "bson-1.0.0"
%lib     C "mongoc-1.0.0"
%link    C "mongolib.o"
%include C "mongolib.h"

export
client_get_collection : (connection : DBConnection) -> (db_name : String) -> (collection_name: String) -> DBResult
client_get_collection (Connection connection_handle) db_name collection_name =
  let result = unsafePerformIO (foreign FFI_C "_client_get_collection" (Ptr -> String -> String -> IO Ptr) connection_handle db_name collection_name)
  in
    if result == null
      then DBResultError
      else DBResultCollection (Collection result)

export
init : DBResult
init = let result = unsafePerformIO(foreign FFI_C "_init" (IO Ptr))
  in
    if result == null
      then DBResultError
      else DBResultPtr (pure result)

export
client_new : (uri : String) -> DBResult
client_new uri = let result = unsafePerformIO (foreign FFI_C "mongoc_client_new" (String -> IO Ptr) uri)
  in
    if result == null
      then DBResultError
      else DBResultConnection (Connection result)

export
client_destroy : (connection : DBConnection) -> DBResult
client_destroy (Connection connection_handle) = DBResultIO (foreign FFI_C "mongoc_client_destroy" (Ptr -> IO ()) connection_handle)


export
cleanup : DBResult
cleanup = DBResultIO (foreign FFI_C "mongoc_cleanup" (IO ()))

export
collection_insert : DBCollection -> BSON -> DBResult
collection_insert (Collection collection_handle) (MkBSON bson_document) =
    let result = unsafePerformIO (foreign FFI_C "_collection_insert" (Ptr -> Ptr -> IO Int) collection_handle bson_document)
      in
        if result == 0
          then DBResultError
          else DBResultCount (pure result)

export
collection_remove : Ptr -> BSON -> DBResult
collection_remove collection (MkBSON selector_handle) =
  let result = unsafePerformIO (foreign FFI_C "_collection_remove" (Ptr -> Ptr -> IO Int) collection selector_handle)
    in
      if result == 0
        then DBResultError
        else DBResultBool (Just result)

export
set_error_api : Ptr -> Int -> DBResult
set_error_api connection error_level = DBResultIO (foreign FFI_C "mongo_client_set_error_api" (Ptr -> Int -> IO ()) connection error_level)

export
write_concern_new : DBResult
write_concern_new = DBResultPtr (foreign FFI_C "mongoc_write_concern_new" (IO Ptr))

export
write_concern_destroy : Ptr -> DBResult
write_concern_destroy concern_ptr = DBResultIO (foreign FFI_C "mongoc_write_concern_destroy" (Ptr -> IO ()) concern_ptr)


export
collection_find_and_modify : Ptr -> BSON -> BSON -> DBResult
collection_find_and_modify collection (MkBSON query_handle) (MkBSON update_handle) =
  let result = unsafePerformIO (foreign FFI_C "_collection_find_and_modify" (Ptr -> Ptr -> Ptr -> IO Int) collection query_handle update_handle)
  in
    if (result == 0)
      then DBResultError
      else DBResultBool (Just result)

export
collection_find : (collection: DBCollection) -> (filter: DBQuery) -> (options: DBOptions) -> DBResult
collection_find (Collection collection) (Query (MkBSON query)) (Options (MkBSON options)) =
  let result = unsafePerformIO (foreign FFI_C "_collection_find" (Ptr -> Ptr -> Ptr -> IO Ptr) collection query null)
  in
    if result == null
      then DBResultIO (pure ())
      else DBResultCursor (Cursor result)

export
cursor_next : Ptr -> DBResult
cursor_next cursor =
  let doc_handle =  unsafePerformIO (foreign FFI_C "_cursor_next" (Ptr -> IO Ptr) cursor)
  in
    if (doc_handle == null)
      then DBResultIO (pure ())
      else DBResultBSON (Just (MkBSON doc_handle))

export
collection_update : Ptr -> BSON -> BSON -> Int -> DBResult
collection_update collection (MkBSON selector_handle) (MkBSON update_handle) update_flags =
  let result = unsafePerformIO (foreign FFI_C "_collection_update" (Ptr -> Ptr -> Ptr -> Int -> IO Int) collection selector_handle update_handle update_flags)
  in
    if (result == 0)
      then DBResultIO (pure ())
      else DBResultCount (pure result)
