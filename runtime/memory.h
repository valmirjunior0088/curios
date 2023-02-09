#ifndef MEMORY_H
#define MEMORY_H

#include "walloc.h"

void *alloc(size_t);
size_t allocations();

void dealloc(void *);
size_t deallocations();

#endif