#include <bson.h>
#include <mongoc.h>

int _collection_insert(mongoc_collection_t *collection, const bson_t *document);

mongoc_cursor_t* _collection_find(mongoc_collection_t *collection, const bson_t * filter, const bson_t * opts);

bson_t* _bson_new_from_json (char *json_string);

bson_t * _cursor_next(mongoc_cursor_t* cursor);
