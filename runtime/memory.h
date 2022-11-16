#ifndef MEMORY_H
#define MEMORY_H

#define NULL ((void *) 0)

typedef __SIZE_TYPE__ size_t;
typedef __UINTPTR_TYPE__ uintptr_t;
typedef __UINT8_TYPE__ uint8_t;

void* alloc(size_t);
void dealloc(void *);

#endif