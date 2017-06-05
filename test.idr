public export
data DBResult = DBNothing (IO ()) | DBPtr (IO Ptr) | DBCount (IO Int) | DBIO (IO ())
