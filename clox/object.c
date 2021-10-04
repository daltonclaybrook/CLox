#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType) \
    (type*)allocateObject(sizeof(type), objectType)

static Obj* allocateObject(size_t size, ObjType type) {
    Obj* object = (Obj*)reallocate(NULL, 0, size);
    object->type = type;

    object->next = vm.objects;
    vm.objects = object;
    return object;
}

ObjString* stringWithUninitializedChars(int stringLength) {
    size_t size = sizeof(ObjString) + sizeof(char) * (stringLength + 1);
    ObjString* object = (ObjString*)allocateObject(size, OBJ_STRING);
    object->length = stringLength;
    return object;
}

ObjString* copyString(const char* chars, int length) {
    size_t size = sizeof(ObjString) + sizeof(char) * (length + 1);
    ObjString* object = (ObjString*)allocateObject(size, OBJ_STRING);
    object->length = length;

    memcpy(object->chars, chars, length);
    object->chars[length] = '\0';
    return object;
}

void printObject(Value value) {
    switch (OBJ_TYPE(value)) {
        case OBJ_STRING:
            printf("%s", AS_CSTRING(value));
            break;
    }
}
