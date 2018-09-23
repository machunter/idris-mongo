module DB.Mongo.Definitions
import DB.Mongo.Bson
import Language.JSON

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
data DBWriteConcern = WriteConcern Ptr

public export
record DBState where
  constructor MkDBState
  connection : Maybe DBConnection
  collection : Maybe DBCollection
  cursor : Maybe DBCursor

public export
data DebugMode = DebugModeOn | DebugModeOff

public export
CallTrace : Type
CallTrace = List String

public export
data State = CurrentState (Maybe CallTrace, Maybe DBConnection, Maybe DBCollection, Maybe DBCursor)

public export
data DBResult =
  DBResultVoid |
  DBResultPtr Ptr |
  DBResultCount Int |
  DBResultCollection DBCollection |
  DBResultConnection DBConnection |
  DBResultCursor DBCursor |
  DBResultBool Bool |
  DBResultJSON JSON |
  DBResultError String

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
