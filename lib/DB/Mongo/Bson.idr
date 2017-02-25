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



||| creates a bson object from a json string
||| @json_string the stringified JSON string
export
new_from_json : (json_string : Maybe String) -> IO Ptr
new_from_json json_string = case json_string of
                                  Nothing => pure null
                                  Just value => _new_from_json value

||| deletes a bson object
||| @bson_object the bson object to destroy
export
destroy : (bson_object : Ptr) -> IO ()
destroy bson_object = if (bson_object == null)
                      then pure ()
                      else foreign FFI_C "bson_destroy" (Ptr -> IO ()) bson_object

||| allocates a new bson object
export
new : IO Ptr
new = foreign FFI_C "bson_new" (IO Ptr)

||| converts a bson object into a json String
||| @bson_object the bson object to convert in a json string
export
as_json : (bson_object : Ptr) -> IO String
as_json bson_object = _as_json bson_object null
