#include "mongolib.h"



int _collection_insert(mongoc_collection_t *collection, const bson_t *document) {
  printf ("%s \n", "_collection_insert");
  bson_error_t error;
  mongoc_write_concern_t *write_concern = mongoc_write_concern_new ();
  mongoc_write_concern_set_w(write_concern, MONGOC_WRITE_CONCERN_W_DEFAULT);
  bool result = mongoc_collection_insert (collection, MONGOC_INSERT_NONE, document,write_concern, &error);
  mongoc_write_concern_destroy(write_concern);
  if (result) {
    return 1;
  } else {
    fprintf (stderr, "ERROR: %d.%d: %s\n", error.domain, error.code, error.message);
    return 0;
  }
}


bson_t* _bson_new_from_json (char *json_string) {
  printf ("%s \n", "_bson_new_from_json");
  bson_error_t error;
  bson_t *bson = bson_new_from_json ( (uint8_t *)json_string, strlen(json_string), &error);
  return bson;
}


mongoc_cursor_t* _collection_find(mongoc_collection_t *collection, const bson_t * filters, const bson_t * opts){

  mongoc_read_prefs_t * read_prefs = mongoc_read_prefs_new(MONGOC_READ_NEAREST);
  mongoc_cursor_t* cursor = mongoc_collection_find_with_opts(collection, filters, opts, read_prefs);
  mongoc_read_prefs_destroy(read_prefs);
  return cursor;
}


int _collection_remove(mongoc_collection_t *collection, const bson_t *selector) {
  bson_error_t error;
  mongoc_write_concern_t *write_concern = mongoc_write_concern_new();
  mongoc_write_concern_set_w(write_concern, MONGOC_WRITE_CONCERN_W_DEFAULT);
  bool result = mongoc_collection_remove(collection, MONGOC_REMOVE_NONE, selector, write_concern, &error);
  mongoc_write_concern_destroy(write_concern);
  if (result) {
    return 1;
  } else {
    fprintf (stderr, "ERROR: %d.%d: %s\n", error.domain, error.code, error.message);
    return 0;
  }
}

int _collection_find_and_modify(mongoc_collection_t *collection, const bson_t *query, const bson_t *update) {
  bson_error_t error;

  bool result = mongoc_collection_find_and_modify(collection, query, NULL, update, NULL, false, true, false, NULL, &error);
  if (!result) {
    fprintf (stderr, "ERROR: %d.%d: %s\n", error.domain, error.code, error.message);
    return 0;
  }
  return 1;
}

bool _collection_update(mongoc_collection_t *collection, const bson_t *selector, const bson_t *update, int update_flags) {
  bson_error_t error;
  mongoc_write_concern_t *write_concern = mongoc_write_concern_new();
  mongoc_write_concern_set_w(write_concern, MONGOC_WRITE_CONCERN_W_DEFAULT);
  bool result = mongoc_collection_update (collection, update_flags, selector, update, write_concern, &error);

  mongoc_write_concern_destroy(write_concern);
  if (result) {
    return 1;
  } else {
    fprintf (stderr, "ERROR: %d.%d: %s\n", error.domain, error.code, error.message);
    return 0;
  }
}



mongoc_collection_t * _client_get_collection (mongoc_client_t *client, const char *db, const char *collection) {
  printf ("%s \n", "_client_get_collection");

  mongoc_collection_t * collection_ptr = mongoc_client_get_collection (client,db,collection);
  return collection_ptr;
}

bson_t * _cursor_next(mongoc_cursor_t* cursor) {
  bson_t *bson = bson_new();

  bool result = mongoc_cursor_next (cursor, (const bson_t **) &bson);

  if (result) {
    return bson;
  } else {
    return NULL;
  }
}

bson_t* _init(void) {
  printf ("%s \n", "_init");
  mongoc_init();
  return NULL;
}
