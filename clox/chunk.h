#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

typedef enum {
    /// Load a constant for use
    OP_CONSTANT,
    /// Add two values
    OP_ADD,
    /// Subtract a value from another
    OP_SUBTRACT,
    /// Multiply two values
    OP_MULTIPLY,
    /// Divide a value from another
    OP_DIVIDE,
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
