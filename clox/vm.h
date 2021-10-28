#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "table.h"
#include "value.h"

#define STACK_MAX 256

typedef struct {
    /// The current chunk of bytecode being evaluated
    Chunk* chunk;
    /// Instruction pointer
    uint8_t* ip;
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
