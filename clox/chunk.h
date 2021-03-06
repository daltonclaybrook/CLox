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
    /// Pop a value off of the stack, effectively discarding it
    OP_POP,
    /// Load the value of a local variable from the stack and push it onto the stack
    OP_GET_LOCAL,
    /// Get a value of the top of the stack and set is as the value of a local variable (also on the stack)
    OP_SET_LOCAL,
    /// Get the value of a named variable from the globals table and push it onto the stack
    OP_GET_GLOBAL,
    /// Define a global variable whose identifier is stored in the constants table
    OP_DEFINE_GLOBAL,
    /// Set the value of a global variable
    OP_SET_GLOBAL,
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
    /// Print a value
    OP_PRINT,
    /// Jump to a different instruction by offsetting the `ip` by the amount of the 16-bit operand
    OP_JUMP,
    /// Jump to a different instruction by offsetting the `ip` by the amount of the 16-bit operand if the
    /// stack top contains a falsey value.
    OP_JUMP_IF_FALSE,
    /// Similar to `OP_JUMP` except used to jump backwards rather than forwards.
    OP_LOOP,
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
