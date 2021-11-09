#ifndef clox_vm_h
#define clox_vm_h

#include "object.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

/// Represents a single, ongoing function call
typedef struct {
    ObjFunction* function;
    uint8_t* ip;
    Value* slots;
} CallFrame;

typedef struct {
    /// The stack on which call frameworks are stored
    CallFrame frames[FRAMES_MAX];
    /// The current height of the call stack
    int frameCount;
    /// The stack on which expression operands and local variables are stored
    Value stack[STACK_MAX];
    /// A pointer to the top value on the stack
    Value* stackTop;
    /// The collection of global variables in use
    Table globals;
    /// The collection of all unique strings in use. The VM uses "String Interning" to ensure that only
    /// one copy of each unique string is created and stored.
    /// https://en.wikipedia.org/wiki/String_interning
    Table strings;
    /// A linked-list of all objects in memory. This is used for garbage collection.
    Obj* objects;
} VM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

extern VM vm;

void initVM(void);
void freeVM(void);
InterpretResult interpret(const char* source);
void push(Value value);
Value pop(void);

#endif
