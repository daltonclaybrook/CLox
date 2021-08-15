//
//  exceptions.c
//  CLox
//
//  Created by Dalton Claybrook on 8/13/21.
//

#include <stdio.h>
#include <stdlib.h>
#include "exceptions.h"

void raise_error(const char *message) {
    fprintf(stderr, "%s\n", message);
    exit(1);
}
