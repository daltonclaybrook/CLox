#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

typedef enum {
    /// Push a constant onto the stack
    OP_CONSTANT,
    /// Push `nil` onto the stack (Special case of `OP_CONSTANT`)
    OP_NIL,
    /// Push `true` onto the stack (Special case of `OP_CONSTANT`)
    OP_TRUE,
    /// Push `false` onto the stack (Special case of `OP_CONSTANT`)
    OP_FALSE,
    /// Compare two values and return a boolean
    OP_EQUAL,
    /// Compare two values and return a boolean
    OP_GREATER,
    /// Compare two values and return a boolean
    OP_LESS,
    /// Add two values
    OP_ADD,
    /// Subtract a value from another
    OP_SUBTRACT,
    /// Multiply two values
    OP_MULTIPLY,
    /// Divide a value from another
    OP_DIVIDE,
    /// "Not" a value using the `!` operator
    OP_NOT,
    /// Negate a value
    OP_NEGATE,
    /// Return from the current function
    OP_RETURN,
} OpCode;

typedef struct {
    int count;
    int capacity;
    uint8_t* code;
    int* lines;
    ValueArray constants;
} Chunk;

void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte, int line);
int addConstant(Chunk* chunk, Value value);

#endif
