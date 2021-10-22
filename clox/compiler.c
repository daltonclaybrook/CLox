#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

/**
 This is an implementation of a "Pratt Parser," which is different from the Recursive Descent Parser we
 implemented in jlox.
 */

/// A collection of useful state representing the progress of the parser
typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

/// A sorted hierarchy of precedence values used when parsing expressions.
///
/// When parsing an expression with an operator, the parser uses this hierarchy to determine the extent of the
/// subexpression to use as the subsequent operand.
/// For example, consider the expression `1 + 2 * 3`. The `2 * 3` portion of the full expression needs to be grouped
/// and evaluated first in order to become the right operand to the plus expression. After we've parsed the "plus"
/// token, we use this precedence hierarchy to effectively say, "keep parsing tokens to use in the right operand
/// so long as the token precedence continues to be higher than `PREC_TERM`, which is the precedence of the
/// plus operator.
typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT,  // =
    PREC_OR,          // or
    PREC_AND,         // and
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_UNARY,       // ! -
    PREC_CALL,        // . ()
    PREC_PRIMARY
} Precedence;

/// A function pointer used to store parse functions in a table for quick lookup
typedef void (*ParseFn)(bool canAssign);

/// These rules are mapped to tokens and describe how a token should be parsed
typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

/// A struct used to associate the name of a local variable to it's depth on the stack
typedef struct {
    Token name;
    int depth;
} Local;

/// The current state of compilation, for example, the list of local variables and the current
/// scope depth.
typedef struct {
    Local locals[UINT8_COUNT];
    int localCount;
    int scopeDepth;
} Compiler;

Parser parser;
Compiler* current = NULL;
Chunk* compilingChunk;

static Chunk* currentChunk() {
    return compilingChunk;
}

/// Report a compilation error to `stderr` and enter "panic mode." Panic mode is use to prevent a cascade
/// of syntax errors from being reported once the first one is found. After parsing a declaration, if panic mode
/// is set, the compiler will "synchronize" its state by parsing and discarding tokens until it reaches a
/// "synchronization point." At this point, compilation proceeds and more errors may be reported.
static void errorAt(Token* token, const char* message) {
    if (parser.panicMode) return;
    parser.panicMode = true;
    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // Nothing.
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

/// Convenience function for reporting an error with the previously parsed token
static void error(const char* message) {
    errorAt(&parser.previous, message);
}

/// Convenience function for reporting an error with the current token
static void errorAtCurrent(const char* message) {
    errorAt(&parser.current, message);
}

/// Set the `current` token as the `previous` token, and scan the next token into the `current` field. If the
/// scanned token is an "error" token—meaning that the scanner encountered an error of its own—the
/// function continues on to the next token.
static void advance() {
    parser.previous = parser.current;

    for (;;) {
        parser.current = scanToken();
        if (parser.current.type != TOKEN_ERROR) break;

        errorAtCurrent(parser.current.start);
    }
}

/// Assert that the current token has the provided type, then advance, or emit the provided error message
static void consume(TokenType type, const char* message) {
    if (parser.current.type == type) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

/// Returns true if the current token has a type equal to the provided type
static bool check(TokenType type) {
    return parser.current.type == type;
}

/// Returns true and advances if the current token type equals the provided type, otherwise, returns false
/// and does not advance.
static bool match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
}

/// Append one byte of bytecode onto the current chunk. This might be an opcode or an operand to the opcode.
static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}

/// Append two bytes of bytecode onto the current chunk. This is usually an opcode + its operand,
/// e.g. `OP_SET_LOCAL <var>`
static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

/// Append the `OP_RETURN` opcode onto the current chunk
static void emitReturn() {
    emitByte(OP_RETURN);
}

/// Adds a value to the constants table and returns its index. The index is used in the bytecode to
/// represent the constant.
static uint8_t makeConstant(Value value) {
    int constant = addConstant(currentChunk(), value);
    if (constant > UINT8_MAX) {
        error("Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t)constant;
}

/// Emit the necessary bytecode to push the provided value onto the stack
static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
}

/// Initialize the provided compiler struct
static void initCompiler(Compiler* compiler) {
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    current = compiler;
}

/// Emit the necessary bytecode to terminate the chunk.
static void endCompiler() {
    emitReturn();
#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(currentChunk(), "code");
    }
#endif
}

/// Increment the current scope depth for local variables on the stack
static void beginScope() {
    current->scopeDepth++;
}

/// Decrement the current scope depth for local variables on the stack
static void endScope() {
    current->scopeDepth--;

    while (current->localCount > 0 &&
           current->locals[current->localCount - 1].depth > current->scopeDepth) {
        emitByte(OP_POP);
        current->localCount--;
    }
}

// MARK: - Function declaration prototypes

static void expression(void);
static void statement(void);
static void declaration(void);
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

/// Adds the provided identifier to the constants table by copying the identifier name and returns the
/// index of the identifier in the constants table.
static uint8_t identifierConstant(Token* name) {
    return makeConstant(OBJ_VAL((Obj*)copyString(name->start,
                                                 name->length)));
}

/// Check if two identifiers are equal
static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

/// Find the index of a local variable on the stack matching the provided name if it exists.
/// Otherwise, return -1, indicating that no match was found.
static int resolveLocal(Compiler* compiler, Token* name) {
    for (int i = compiler->localCount - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name)) {
            if (local->depth == -1) {
                error("Can't read local variable in its own initializer.");
            }
            return i;
        }
    }

    return -1;
}

/// Add an uninitialized local variable to the locals array. Note: this function does not emit bytecode, it merely
/// affects compiler state.
static void addLocal(Token name) {
    if (current->localCount == UINT8_COUNT) {
        error("Too many local variables in function.");
        return;
    }

    Local* local = &current->locals[current->localCount++];
    local->name = name;
    // -1 is a sentinel value to indicate that the variable is uninitialized.
    local->depth = -1;
}

/// When the previously parsed token is the identifier of a variable declaration, use this function to
/// add the variable to the locals array. If another variable already exists in the locals array with the
/// same scope depth, an error is emitted.
static void declareVariable() {
    if (current->scopeDepth == 0) return;

    Token* name = &parser.previous;
    for (int i = current->localCount - 1; i >= 0; i--) {
        Local* local = &current->locals[i];
        if (local->depth != -1 && local->depth < current->scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local->name)) {
            error("Already a variable with this name in this scope.");
        }
    }

    addLocal(*name);
}

/// Parse the identifier of a variable declaration and store it appropriate, either in the locals array,
/// or in the constants table if it is a global variable.
static uint8_t parseVariable(const char* errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);

    declareVariable();
    if (current->scopeDepth > 0) return 0;

    return identifierConstant(&parser.previous);
}

/// Mark the local variable at the top of the stack as initialized by setting the correct scope depth.
/// This is used to prevent a variable from being accessed as part of its own variable expression,
/// e.g. `var foo = foo;`
static void markInitialized() {
    current->locals[current->localCount - 1].depth = current->scopeDepth;
}

/// After a variable expression has been parsed, mark the variable as "initialized" by setting it's depth field
/// if it is a local variable, or by emitting the bytecode to define a global variable.
static void defineVariable(uint8_t global) {
    if (current->scopeDepth > 0) {
        // If we're defining a local variable, we don't need to emit any bytecode,
        // but we do need to mark it as initialized by setting an appropriate scope
        // depth.
        markInitialized();
        return;
    }

    emitBytes(OP_DEFINE_GLOBAL, global);
}

/// Emit bytecode for the binary operator by evaluating its righthand operand. This function uses the precedence
/// rule of the operator to determine the extent of the expression to parse into the right operand.
static void binary(bool canAssign) {
    TokenType operatorType = parser.previous.type;
    ParseRule* rule = getRule(operatorType);
    parsePrecedence((Precedence)(rule->precedence + 1));

    switch (operatorType) {
        case TOKEN_BANG_EQUAL:    emitBytes(OP_EQUAL, OP_NOT); break;
        case TOKEN_EQUAL_EQUAL:   emitByte(OP_EQUAL); break;
        case TOKEN_GREATER:       emitByte(OP_GREATER); break;
        case TOKEN_GREATER_EQUAL: emitBytes(OP_LESS, OP_NOT); break;
        case TOKEN_LESS:          emitByte(OP_LESS); break;
        case TOKEN_LESS_EQUAL:    emitBytes(OP_GREATER, OP_NOT); break;
        case TOKEN_PLUS:    emitByte(OP_ADD); break;
        case TOKEN_MINUS:   emitByte(OP_SUBTRACT); break;
        case TOKEN_STAR:    emitByte(OP_MULTIPLY); break;
        case TOKEN_SLASH:   emitByte(OP_DIVIDE); break;
        default: return; // Unreachable.
    }
}

/// Emit bytecode for a keyword literal. Number and string literals are handled by different functions.
static void literal(bool canAssign) {
    switch (parser.previous.type) {
        case TOKEN_FALSE: emitByte(OP_FALSE); break;
        case TOKEN_NIL: emitByte(OP_NIL); break;
        case TOKEN_TRUE: emitByte(OP_TRUE); break;
        default: return; // Unreachable.
    }
}

/// Parse the complete expression up to the right parenthesis, then consume the parenthesis.
static void grouping(bool canAssign) {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

/// Add a number literal to the constants table and emit bytecode to push the value onto the stack
static void number(bool canAssign) {
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

/// Add a string literal to the constants table and emit bytecode to push the value onto the stack
static void string(bool canAssign) {
    emitConstant(OBJ_VAL((Obj*)copyString(parser.previous.start + 1,
                                    parser.previous.length - 2)));
}

/// Emit bytecode to get or set a variable. The variable might be local or global, so this function
/// determines which. If global, an entry is added to the constants table with the variable's
/// name, and the index of the constant is emitted as the argument to the bytecode instruction.
static void namedVariable(Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(current, &name);
    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else {
        arg = identifierConstant(&name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(setOp, (uint8_t)arg);
    } else {
        emitBytes(getOp, (uint8_t)arg);
    }
}

/// Emit bytecode to access as variable. This might represent an assignment or a getter based on whether the
/// next token is a '=' and if assignment is allowed in the current expression.
static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

/// Emit bytecode for a unary operator after parsing the operand.
static void unary(bool canAssign) {
    TokenType operatorType = parser.previous.type;

    // Compile the operand.
    parsePrecedence(PREC_UNARY);

    // Emit the operator instruction.
    switch (operatorType) {
        case TOKEN_BANG: emitByte(OP_NOT); break;
        case TOKEN_MINUS: emitByte(OP_NEGATE); break;
        default: return; // Unreachable.
    }
}

/// A mapping of parse rules for each token. Tokens may have 0, 1, or 2 parse rules given where the token
/// can appear inside of statements/expressions. Tokens may also have a "precedence" value that indicates
/// to the parser how to parse the operand[s] of an operator.
ParseRule rules[] = {
    [TOKEN_LEFT_PAREN]    = {grouping, NULL,   PREC_NONE},
    [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE},
    [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_DOT]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
    [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
    [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
    [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
    [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
    [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
    [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
    [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},
    [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
    [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
    [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
    [TOKEN_AND]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
    [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
    [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
    [TOKEN_OR]            = {NULL,     NULL,   PREC_NONE},
    [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
    [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_THIS]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
    [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};

/// Parse and emit bytecode for the next portion of an expression given the provided precedence value.
static void parsePrecedence(Precedence precedence) {
    advance();
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;
    if (prefixRule == NULL) {
        error("Expect expression.");
        return;
    }

    bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixRule(canAssign);

    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        error("Invalid assignment target.");
    }
}

/// Convenience for looking up a rule for a provided token type.
static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

/// Parse an expression with the lowest precedence
static void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}

/// Parse and emit bytecode for every declaration in a block until a '}' is encountered.
static void block() {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

/// Parse emit bytecode for a variable declaration
static void varDeclaration() {
    uint8_t global = parseVariable("Expect variable name.");

    if (match(TOKEN_EQUAL)) {
        expression();
    } else {
        emitByte(OP_NIL);
    }
    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

    defineVariable(global);
}

/// Parse and emit bytecode for an expression statement, which is an expression followed by a semicolon.
/// Expressions have a "stack effect" of 1, and statements have a stack effect of 0, so after parsing an expression,
/// we emit an `OP_POP` instruction to pop the value off of the stack.
static void expressionStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(OP_POP);
}

/// Parse and emit bytecode for an expression, then emit an instruction to print the value on the top of the stack.
static void printStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

/// After parsing a declaration, if panic mode is set, the compiler will "synchronize" its state by
/// parsing and discarding tokens until it reaches a "synchronization point." At this point, compilation
/// proceeds and more errors may be reported.
static void synchronize() {
    parser.panicMode = false;

    while (parser.current.type != TOKEN_EOF) {
        if (parser.previous.type == TOKEN_SEMICOLON) return;
        switch (parser.current.type) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
                return;

            default:
                ; // Do nothing.
        }

        advance();
    }
}

/// Parse and emit bytecode for a declaration, which can be a variable declaration or a statement.
static void declaration() {
    if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    if (parser.panicMode) synchronize();
}

/// Parse and emit bytecode for a statement
static void statement() {
    if (match(TOKEN_PRINT)) {
        printStatement();
    } else if (match(TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else {
        expressionStatement();
    }
}

/// Initialize a compiler and compile every declaration in the provided source string until an EOF token is
/// reached.
bool compile(const char* source, Chunk* chunk) {
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler);
    compilingChunk = chunk;

    parser.hadError = false;
    parser.panicMode = false;

    // The "previous" token is the one we are compiling at any given moment,
    // so we need to advance before we continue.
    advance();

    while (!match(TOKEN_EOF)) {
        declaration();
    }

    endCompiler();
    return !parser.hadError;
}
