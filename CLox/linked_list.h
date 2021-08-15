//
//  linked_list.h
//  CLox
//
//  Created by Dalton Claybrook on 8/13/21.
//

#ifndef linked_list_h
#define linked_list_h

#include <stdio.h>

typedef struct LinkedList LinkedList;
extern const int LinkedListIndexNotFound;

LinkedList * LinkedListCreate(void);
void LinkedListDestroy(LinkedList *list);

int LinkedListGetCount(LinkedList *list);
int LinkedListFirstIndexOfString(LinkedList *list, const char *string);
const char * LinkedListGetString(LinkedList *list, int index);

void LinkedListInsertStringCopy(LinkedList *list, const char *string, int index);
void LinkedListDeleteString(LinkedList *list, int index);

#endif /* linked_list_h */
