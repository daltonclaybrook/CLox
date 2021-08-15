//
//  linked_list.c
//  CLox
//
//  Created by Dalton Claybrook on 8/13/21.
//

#include <stdlib.h>
#include <string.h>
#include "linked_list.h"
#include "exceptions.h"

// MARK: - Declarations

typedef struct Node Node;
Node * GetNodeAtIndex(LinkedList *list, int index);

const int LinkedListIndexNotFound = -1;

struct Node {
    const char *string;
    Node *previous;
    Node *next;
};

struct LinkedList {
    Node *first;
    Node *last;
    int count;
};

// MARK: - Implementation

LinkedList * LinkedListCreate(void) {
    return (LinkedList *)calloc(1, sizeof(LinkedList));
}

void LinkedListDestroy(LinkedList *list) {
    Node *currentNode = list->first;
    for (int i=0; i<list->count; i++) {
        Node *nextNode = currentNode->next;
        free((void *)currentNode->string);
        free(currentNode);
        currentNode = nextNode;
    }
    free(list);
}

int LinkedListGetCount(LinkedList *list) {
    return list->count;
}

void LinkedListInsertStringCopy(LinkedList *list, const char *string, int index) {
    if (index < 0 || index > list->count) {
        return raise_error("Provided index is out of bounds");
    }

    size_t len = strlen(string);
    char *newString = malloc(len * sizeof(char));
    strcpy(newString, string);

    Node *newNode = calloc(1, sizeof(Node));
    newNode->string = newString;

    // If list is empty, set the new node as first & last, then return.
    if (list->count == 0) {
        list->first = newNode;
        list->last = newNode;
        list->count++;
        return;
    }

    // Find the current node at the provided index
    Node *existingNode = GetNodeAtIndex(list, index);

    // Update fields on new node
    newNode->next = existingNode;
    if (existingNode == NULL) {
        newNode->previous = list->last;
    } else {
        newNode->previous = existingNode->previous;
    }

    // Update fields on surrounding nodes
    if (newNode->previous != NULL) {
        newNode->previous->next = newNode;
    } else {
        list->first = newNode;
    }

    if (newNode->next != NULL) {
        newNode->next->previous = newNode;
    } else {
        list->last = newNode;
    }

    // Increment the list count after inserting
    list->count++;
}

int LinkedListFirstIndexOfString(LinkedList *list, const char *string) {
    if (list->count == 0) {
        return LinkedListIndexNotFound;
    }

    Node *node = list->first;
    for (int i=0; i<list->count; i++) {
        int result = strcmp(string, node->string);
        if (result == 0) {
            return i;
        }
        node = node->next;
    }

    return LinkedListIndexNotFound;
}

void LinkedListDeleteString(LinkedList *list, int index) {
    if (index < 0 || index >= list->count) {
        return raise_error("Provided index is out of bounds");
    }

    Node *node = GetNodeAtIndex(list, index);
    if (node->next != NULL) {
        node->next->previous = node->previous;
    } else {
        list->last = node->previous;
    }

    if (node->previous != NULL) {
        node->previous->next = node->next;
    } else {
        list->first = node->next;
    }

    list->count--;

    // Free the memory
    free((void *)node->string);
    free(node);
}

const char * LinkedListGetString(LinkedList *list, int index) {
    if (index < 0 || index >= list->count) {
        raise_error("Provided index is out of bounds");
        return NULL;
    }
    return GetNodeAtIndex(list, index)->string;
}

// MARK: - Private helpers

Node * GetNodeAtIndex(LinkedList *list, int index) {
    Node *node = list->first;
    for (int i=0; i<index; i++) {
        node = node->next;
    }
    return node;
}
