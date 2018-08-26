module DB.Mongo.MongoCImports
import DB.Mongo.Bson
import DB.Mongo.Definitions

%lib     C "bson-1.0.0"
%lib     C "mongoc-1.0.0"
%link    C "mongolib.o"
%include C "mongolib.h"

public export
_client_get_collection : (connection : DBConnection) -> (db_name : String) -> (collection_name: String) -> DBResult
_client_get_collection (Connection connection_handle) db_name collection_name =
  let result = unsafePerformIO (foreign FFI_C "_client_get_collection" (Ptr -> String -> String -> IO Ptr) connection_handle db_name collection_name)
  in
    if result == null
      then DBResultError
      else DBResultCollection (Collection result)

public export
_init : DBResult
_init = let result = unsafePerformIO(foreign FFI_C "_init" (IO Ptr))
  in
    if result == null
      then DBResultError
      else DBResultPtr (pure result)

public export
_client_new : (uri : String) -> DBResult
_client_new uri = let result = unsafePerformIO (foreign FFI_C "mongoc_client_new" (String -> IO Ptr) uri)
  in
    if result == null
      then DBResultError
      else DBResultConnection (Connection result)

public export
_client_destroy : (connection : DBConnection) -> DBResult
_client_destroy (Connection connection_handle) = DBResultIO (foreign FFI_C "mongoc_client_destroy" (Ptr -> IO ()) connection_handle)


public export
_cleanup : DBResult
_cleanup = DBResultIO (foreign FFI_C "mongoc_cleanup" (IO ()))

public export
_collection_insert : DBCollection -> BSON -> DBResult
_collection_insert (Collection collection_handle) (MkBSON bson_document) =
    let result = unsafePerformIO (foreign FFI_C "_collection_insert" (Ptr -> Ptr -> IO Int) collection_handle bson_document)
      in
        if result == 0
          then DBResultError
          else DBResultCount (pure result)

public export
_collection_remove : Ptr -> BSON -> DBResult
_collection_remove collection (MkBSON selector_handle) =
  let result = unsafePerformIO (foreign FFI_C "_collection_remove" (Ptr -> Ptr -> IO Int) collection selector_handle)
    in
      if result == 0
        then DBResultError
        else DBResultBool (Just result)


public export
_write_concern_new : DBResult
_write_concern_new = DBResultPtr (foreign FFI_C "mongoc_write_concern_new" (IO Ptr))

public export
_write_concern_destroy : Ptr -> DBResult
_write_concern_destroy concern_ptr = DBResultIO (foreign FFI_C "mongoc_write_concern_destroy" (Ptr -> IO ()) concern_ptr)


public export
_collection_find_and_modify : Ptr -> BSON -> BSON -> DBResult
_collection_find_and_modify collection (MkBSON query_handle) (MkBSON update_handle) =
  let result = unsafePerformIO (foreign FFI_C "_collection_find_and_modify" (Ptr -> Ptr -> Ptr -> IO Int) collection query_handle update_handle)
  in
    if (result == 0)
      then DBResultError
      else DBResultBool (Just result)

public export
_collection_find : (collection: DBCollection) -> (filter: DBQuery) -> (options: DBOptions) -> DBResult
_collection_find (Collection collection) (Query (MkBSON query)) (Options (MkBSON options)) =
  let result = unsafePerformIO (foreign FFI_C "_collection_find" (Ptr -> Ptr -> Ptr -> IO Ptr) collection query options)
  in
    if result == null
      then DBResultIO (pure ())
      else DBResultCursor (Cursor result)

public export
_cursor_next : Ptr -> DBResult
_cursor_next cursor =
  let doc_handle =  unsafePerformIO (foreign FFI_C "_cursor_next" (Ptr -> IO Ptr) cursor)
  in
    if (doc_handle == null)
      then DBResultIO (pure ())
      else DBResultBSON (Just (MkBSON doc_handle))

public export
_collection_update : Ptr -> BSON -> BSON -> Int -> DBResult
_collection_update collection (MkBSON selector_handle) (MkBSON update_handle) update_flags =
  let result = unsafePerformIO (foreign FFI_C "_collection_update" (Ptr -> Ptr -> Ptr -> Int -> IO Int) collection selector_handle update_handle update_flags)
  in
    if (result == 0)
      then DBResultIO (pure ())
      else DBResultCount (pure result)
