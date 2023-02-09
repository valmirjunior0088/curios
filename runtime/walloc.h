#ifndef WALLOC_H
#define WALLOC_H

typedef __SIZE_TYPE__ size_t;
typedef __UINTPTR_TYPE__ uintptr_t;
typedef __UINT8_TYPE__ uint8_t;

#define NULL ((void *) 0)

void *malloc(size_t);
void free(void *);

#endif