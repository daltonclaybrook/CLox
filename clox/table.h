#ifndef clox_table_h
#define clox_table_h

#include "common.h"
#include "value.h"

/**
 An implementation of "Open addressing" or "Closed hashing."
 Notes:
 * Uses "linear probing," which is a kind of "probe sequence."
 * Uses the FNV-1a hash function (http://www.isthe.com/chongo/tech/comp/fnv/)
 * More learning materials: “double hashing”, “cuckoo hashing”, “Robin Hood hashing”
 */

typedef struct {
    ObjString* key;
    Value value;
} Entry;

typedef struct {
    int count;
    int capacity;
    Entry* entries;
} Table;

void initTable(Table* table);
void freeTable(Table* table);
bool tableGet(Table* table, ObjString* key, Value* value);
bool tableSet(Table* table, ObjString* key, Value value);
bool tableDelete(Table* table, ObjString* key);
void tableAddAll(Table* from, Table* to);
ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash);

#endif
