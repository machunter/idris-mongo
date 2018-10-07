#include <bson.h>
#include <mongoc.h>

// init driver
bson_t* _init (void);

void _cleanup(void);


// collection bound
mongoc_collection_t * _client_get_collection(mongoc_client_t *client, const char *db, const char *collection);

int _collection_destroy(mongoc_collection_t *collection);

int _collection_insert(mongoc_collection_t *collection, const bson_t *document);

mongoc_cursor_t* _collection_find(mongoc_collection_t *collection, const bson_t * filter, const bson_t * opts);

int _collection_remove(mongoc_collection_t *collection, const bson_t *selector);

int _collection_find_and_modify(mongoc_collection_t *collection, const bson_t *query, const bson_t *update);

bool _collection_update(mongoc_collection_t *collection, const bson_t *selector, const bson_t *update, int update_flags);

int _mongoc_collection_count_documents(mongoc_collection_t *collection, const bson_t *filter);

// cursor bound
bson_t * _cursor_next(mongoc_cursor_t* cursor);


// bson stuff
bson_t* _bson_new_from_json (char *json_string);

void _bson_destroy(bson_t* bson);

char* _bson_as_json(const bson_t *bson, size_t *length);
