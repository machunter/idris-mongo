module DB.Mongo.Definitions
import DB.Mongo.Bson

public export
data DBCollection = Collection Ptr

public export
data DBCursor = Cursor Ptr

public export
data DBConnection = Connection Ptr

public export
data DBQuery = Query BSON

public export
data DBOptions = Options BSON



public export
data State = CurrentState (String, Maybe DBConnection, Maybe DBCollection, Maybe DBCursor)

public export
data DBResult =
  DBResultVoid |
  DBResultPtr Ptr |
  DBResultCount Int |
  DBResultCollection DBCollection |
  DBResultConnection DBConnection |
  DBResultCursor DBCursor |
  DBResultBool (Maybe bool) |
  DBResultBSON (Maybe BSON) |
  DBResultError String

export
data DBDoc = MkDBDoc String

export
data DBUpdateFlags = MkDBUpdateFlags Int

export
MONGOC_UPDATE_NONE : DBUpdateFlags
MONGOC_UPDATE_NONE = MkDBUpdateFlags 0

export
MONGOC_UPDATE_UPSERT : DBUpdateFlags
MONGOC_UPDATE_UPSERT = MkDBUpdateFlags 1

export
MONGOC_UPDATE_MULTI_UPDATE : DBUpdateFlags
MONGOC_UPDATE_MULTI_UPDATE = MkDBUpdateFlags 2

export
Show DBDoc where
  show (MkDBDoc bson_string) = bson_string
