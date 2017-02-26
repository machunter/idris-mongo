#include <bson.h>
#include <mongoc.h>

int _collection_insert(mongoc_collection_t *collection, const bson_t *document);

mongoc_cursor_t* _collection_find(mongoc_collection_t *collection, const bson_t * filter, const bson_t * opts);

int _collection_remove(mongoc_collection_t *collection, const bson_t *selector);

int _collection_find_and_modify(mongoc_collection_t *collection, const bson_t *query, const bson_t *update);


bson_t* _bson_new_from_json (char *json_string);

bson_t * _cursor_next(mongoc_cursor_t* cursor);

mongoc_collection_t * _client_get_collection (mongoc_client_t *client, const char *db, const char *collection);
