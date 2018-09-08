module DB.Mongo.Bson
import CFFI

%lib     C "bson-1.0.0"
%lib     C "mongoc-1.0.0"
%link    C "mongolib.o"
%include C "mongolib.h"


_as_json : Ptr -> Ptr -> String
_as_json bson length = unsafePerformIO (foreign FFI_C "_bson_as_json" (Ptr -> Ptr -> IO String) bson length)

export
record BSON where
  constructor MkBSON
  bson_handle : Ptr


||| creates a bson object from a json string
||| @json_string the stringified JSON string
export
new_from_json : (json_string : String) -> BSON
new_from_json json_string = MkBSON (unsafePerformIO (foreign FFI_C "_bson_new_from_json" (String -> IO Ptr) json_string))


||| deletes a bson object
||| @bson the bson object to destroy
export
destroy : (bson : BSON) -> ()
destroy (MkBSON bson_handle) = if (bson_handle == null)
                      then ()
                      else unsafePerformIO (foreign FFI_C "_bson_destroy" (Ptr -> IO ()) bson_handle)

||| allocates a new bson object
export
new : BSON
new = MkBSON (unsafePerformIO  (foreign FFI_C "bson_new" (IO Ptr)))

||| converts a bson object into a json String
||| @bson the bson object to convert in a json string
export
as_json : (bson : BSON) -> String
as_json (MkBSON bson_handle) = _as_json bson_handle null
