#include "mongolib.h"



int _collection_insert(mongoc_collection_t *collection, const bson_t *document) {
  bson_error_t error;
  mongoc_write_concern_t *write_concern = mongoc_write_concern_new ();
  bool result = mongoc_collection_insert (collection, MONGOC_INSERT_NONE, document,write_concern, &error);
  mongoc_write_concern_destroy(write_concern);
  if (result) {
    return 1;
  } else {
    return 0;
  }
}


bson_t* _bson_new_from_json (char *json_string) {
  bson_error_t error;
  bson_t *bson = bson_new_from_json ( (uint8_t *)json_string, strlen(json_string), &error);
  return bson;
}


mongoc_cursor_t* _collection_find(mongoc_collection_t *collection, const bson_t * filters,const bson_t * opts){
  mongoc_read_prefs_t * read_prefs = mongoc_read_prefs_new(MONGOC_READ_NEAREST);
  mongoc_cursor_t* cursor = mongoc_collection_find_with_opts(collection, filters, opts, read_prefs);
  mongoc_read_prefs_destroy(read_prefs);

 return cursor;
}


bson_t * _cursor_next(mongoc_cursor_t* cursor) {
  bson_t *bson = bson_new();

  bool result = mongoc_cursor_next (cursor, (const bson_t **) &bson);
  char *str = bson_as_json (bson, NULL);
  printf ("%s\n", str);
  bson_free (str);

  if (result) {
    return bson;
  } else {
    return NULL;
  }
}
