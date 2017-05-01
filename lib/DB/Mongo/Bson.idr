module DB.Mongo.Bson
import CFFI

%lib     C "bson-1.0.0"
%lib     C "mongoc-1.0.0"
%link    C "mongolib.o"
%include C "mongolib.h"

_new_from_json : String -> IO Ptr
_new_from_json json_string = foreign FFI_C "_bson_new_from_json" (String -> IO Ptr) json_string

_as_json : Ptr -> Ptr -> IO String
_as_json bson length = foreign FFI_C "bson_as_json" (Ptr -> Ptr -> IO String) bson length

public export
data BSON : Type where
  MkBSON: (bson: Ptr) -> BSON



||| creates a bson object from a json string
||| @json_string the stringified JSON string
export
new_from_json : (json_string : Maybe String) -> IO BSON
new_from_json json_string = case json_string of
                                  Nothing => pure (MkBSON null)
                                  Just value => do
                                                  x <- _new_from_json value
                                                  pure (MkBSON x)

||| deletes a bson object
||| @bson the bson object to destroy
export
destroy : (bson : BSON) -> IO ()
destroy (MkBSON bson_handle) = if (bson_handle == null)
                      then pure ()
                      else foreign FFI_C "bson_destroy" (Ptr -> IO ()) bson_handle

||| allocates a new bson object
export
new : IO BSON
new = do
  x <- foreign FFI_C "bson_new" (IO Ptr)
  pure (MkBSON x)

||| converts a bson object into a json String
||| @bson the bson object to convert in a json string
export
as_json : (bson : BSON) -> IO String
as_json (MkBSON bson_handle) = _as_json bson_handle null
