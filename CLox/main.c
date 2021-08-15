//
//  main.c
//  CLox
//
//  Created by Dalton Claybrook on 8/13/21.
//

#include <stdio.h>
#include <stdlib.h>
#include "linked_list.h"

void print_all_strings(LinkedList *list);

int main(int argc, const char * argv[]) {
    LinkedList *list = LinkedListCreate();
    printf("count: %i\n", LinkedListGetCount(list));

    LinkedListInsertStringCopy(list, "This is the first string!", 0);
    print_all_strings(list);

    LinkedListInsertStringCopy(list, "This is the second string!", 1);
    print_all_strings(list);

    LinkedListInsertStringCopy(list, "This is inserted in the middle!", 1);
    print_all_strings(list);

    printf("deleting a string...\n");
    LinkedListDeleteString(list, 0);
    print_all_strings(list);

    printf("destroying...\n");
    LinkedListDestroy(list);

    return 0;
}

void print_all_strings(LinkedList *list) {
    int count = LinkedListGetCount(list);
    printf("printing %i strings...\n", count);
    for (int i=0; i<count; i++) {
        printf("%s\n", LinkedListGetString(list, i));
    }
    printf("\n");
}
