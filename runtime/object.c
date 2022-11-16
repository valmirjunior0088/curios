#include "object.h"
#include "memory.h"

#define OBJECT_SIZE (sizeof(struct object))
#define POINTER_SIZE (sizeof(struct object *))

struct object {
    size_t reference_count;
    size_t capacity;
    struct object *children[];
};

struct object *new(size_t capacity, size_t trunk_size) {
    struct object *object = alloc(OBJECT_SIZE + POINTER_SIZE * capacity + trunk_size);
    object->reference_count = 1;
    object->capacity = capacity;

    return object;
}

void enter(struct object *object) {
    if (object == NULL) {
        return;
    }

    if (object->reference_count == 0) {
        return;
    }

    object->reference_count++;
}

void leave(struct object *object) {
    if (object == NULL) {
        return;
    }

    if (object->reference_count == 0) {
        return;
    }

    object->reference_count--;

    if (object->reference_count == 0) {
        for (size_t index = 0; index < object->capacity; index++) {
            leave(object->children[index]);
        }

        dealloc(object);
    }
}

void set(struct object *object, size_t index, struct object *child) {
    object->children[index] = child;
}

struct object *get(struct object *object, size_t index) {
    return object->children[index];
}

void *trunk(struct object *object) {
    return object->children + object->capacity;
}
