#ifndef OBJECT_H
#define OBJECT_H

#include "memory.h"

struct object;
struct object *new(size_t capacity, size_t trunk_size);
void enter(struct object *object);
void leave(struct object *object);
void set(struct object *object, size_t index, struct object *child);
struct object *get(struct object *object, size_t index);
void *trunk(struct object *object);

#endif