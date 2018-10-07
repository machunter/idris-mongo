module DB.Mongo.CImports
import DB.Mongo.Bson
import DB.Mongo.Definitions
import Language.JSON


%lib     C "bson-1.0.0"
%lib     C "mongoc-1.0.0"
%link    C "mongolib.o"
%include C "mongolib.h"

export
client_get_collection :DBConnection -> String -> String -> DBResult
client_get_collection (Connection connection_handle) db_name collection_name =
  let result = unsafePerformIO (foreign FFI_C "_client_get_collection" (Ptr -> String -> String -> IO Ptr) connection_handle db_name collection_name)
  in
    if result == null
      then DBResultError "client_get_collection"
      else DBResultCollection (Collection result)

export
init : DBResult
init = let result = unsafePerformIO(foreign FFI_C "_init" (IO Ptr))
  in
    if result == null
      then DBResultError "init"
      else DBResultPtr result

export
client_new : String -> DBResult
client_new uri = let result = unsafePerformIO (foreign FFI_C "mongoc_client_new" (String -> IO Ptr) uri)
  in
    if result == null
      then DBResultError "client_new"
      else DBResultConnection (Connection result)

export
client_destroy : DBConnection -> DBResult
client_destroy (Connection connection_handle) = let _ = unsafePerformIO (foreign FFI_C "mongoc_client_destroy" (Ptr -> IO ()) connection_handle) in DBResultVoid


export
cleanup : DBResult
cleanup = let result = unsafePerformIO (foreign FFI_C "mongoc_cleanup" (IO ())) in DBResultVoid

export
collection_destroy : DBCollection -> DBResult
collection_destroy (Collection collection_handle) =
  let result = unsafePerformIO (foreign FFI_C "_collection_destroy" (Ptr -> IO Int) collection_handle)
    in
      if result == 0
        then DBResultError "_collection_destroy"
        else DBResultBool True

export
collection_insert : DBCollection -> BSON -> DBResult
collection_insert (Collection collection_handle) (MkBSON bson_document) =
    let result = unsafePerformIO (foreign FFI_C "_collection_insert" (Ptr -> Ptr -> IO Int) collection_handle bson_document)
      in
        if result == 0
          then DBResultError "collection_insert"
          else DBResultCount result



export
collection_remove : DBCollection -> DBQuery -> DBResult
collection_remove (Collection collection) (Query (MkBSON query)) =
  let result = unsafePerformIO (foreign FFI_C "_collection_remove" (Ptr -> Ptr -> IO Int) collection query)
    in
      if result == 0
        then DBResultError "collection_remove"
        else DBResultBool True

export
set_error_api : DBConnection -> Int -> DBResult
set_error_api (Connection connection) error_level = let _ = unsafePerformIO (foreign FFI_C "mongo_client_set_error_api" (Ptr -> Int -> IO ()) connection error_level) in DBResultVoid

export
write_concern_new : DBResult
write_concern_new = DBResultPtr (unsafePerformIO (foreign FFI_C "mongoc_write_concern_new" (IO Ptr)))

export
write_concern_destroy : DBWriteConcern -> DBResult
write_concern_destroy (WriteConcern concern_ptr) = let _ = unsafePerformIO (foreign FFI_C "mongoc_write_concern_destroy" (Ptr -> IO ()) concern_ptr)  in DBResultVoid


export
collection_find_and_modify : DBCollection -> BSON -> BSON -> DBResult
collection_find_and_modify (Collection collection_handle) (MkBSON query_handle) (MkBSON update_handle) =
  let result = unsafePerformIO (foreign FFI_C "_collection_find_and_modify" (Ptr -> Ptr -> Ptr -> IO Int) collection_handle query_handle update_handle)
  in
    if (result == 0)
      then DBResultError "collection_find_and_modify"
      else DBResultCount result

export
collection_find : (collection: DBCollection) -> (filter: DBQuery) -> (options: DBOptions) -> DBResult
collection_find (Collection collection) (Query (MkBSON query)) (Options (MkBSON options)) =
  let result = unsafePerformIO (foreign FFI_C "_collection_find" (Ptr -> Ptr -> Ptr -> IO Ptr) collection query null)
  in
    if result == null
      then DBResultVoid
      else DBResultCursor (Cursor result)

export
collection_count_documents: (collection: DBCollection) -> (filter: DBQuery) -> DBResult
collection_count_documents (Collection collection) (Query (MkBSON query)) =
  let result = unsafePerformIO (foreign FFI_C "_mongoc_collection_count_documents" (Ptr -> Ptr -> IO Int) collection query)
  in
    if result == -1
      then DBResultError "collection_count_documents"
      else DBResultCount result

export
cursor_next : DBCursor -> DBResult
cursor_next (Cursor cursor_handle) =
  let doc_handle =  unsafePerformIO (foreign FFI_C "_cursor_next" (Ptr -> IO Ptr) cursor_handle)
  in
    if (doc_handle == null)
      then DBResultVoid
      else case parse (as_json(MkBSON doc_handle)) of
        Nothing => DBResultError "Couldn't convert result into JSON"
        Just json => DBResultJSON json

export
collection_update : DBCollection -> BSON -> BSON -> Int -> DBResult
collection_update (Collection collection_handle) (MkBSON selector_handle) (MkBSON update_handle) update_flags =
  let result = unsafePerformIO (foreign FFI_C "_collection_update" (Ptr -> Ptr -> Ptr -> Int -> IO Int) collection_handle selector_handle update_handle update_flags)
  in
    if (result == 0)
      then DBResultVoid
      else DBResultCount result
