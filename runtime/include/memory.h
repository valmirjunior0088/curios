#ifndef MEMORY_H
#define MEMORY_H

typedef __SIZE_TYPE__ size_t;

void *alloc(size_t size);
void dealloc(void *pointer);

#endif
