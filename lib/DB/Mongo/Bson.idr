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



export
new_from_json : String -> IO Ptr
new_from_json json_string = _new_from_json json_string


export
destroy : Ptr -> IO ()
destroy bson = foreign FFI_C "bson_destroy" (Ptr -> IO ()) bson

export
new : IO Ptr
new = foreign FFI_C "bson_new" (IO Ptr)

export
as_json : Ptr -> IO String
as_json bson = _as_json bson null
