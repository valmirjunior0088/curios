#include "walloc.h"

static size_t alloc_count = 0;

void *alloc(size_t size) {
    alloc_count += 1;

    return malloc(size);
}

size_t allocations() {
    return alloc_count;
}

static size_t dealloc_count = 0;

void dealloc(void* pointer) {
    dealloc_count += 1;

    return free(pointer);
}

size_t deallocations() {
    return dealloc_count;
}
