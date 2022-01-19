#include "object.h"

#include "memory.h"
#include "io.h"

#define NULL ((void *) 0)
#define OBJECT_SIZE (sizeof(struct object))
#define OBJECT_REF_SIZE (sizeof(struct object *))

enum type {
  INT32,
  FLT32,
  CLOSURE,
};

struct int32 {
  int value;
};

struct flt32 {
  float value;
};

struct closure {
  abstraction abstraction;
  int capacity;
  struct object *values[];
};

union payload {
  struct int32 int32;
  struct flt32 flt32;
  struct closure closure;
};

struct object {
  int reference_count;
  enum type type;
  union payload payload;
};

void object_enter(struct object *object) {
  if (object == NULL) {
    return;
  }

  object->reference_count++;
}

void object_leave(struct object *object) {
  if (object == NULL) {
    return;
  }

  object->reference_count--;

  if (object->reference_count > 0) {
    return;
  }

  switch (object->type) {
    case INT32:
      dealloc(object);
      break;
    
    case FLT32:
      dealloc(object);
      break;
    
    case CLOSURE:
      for (int index = 0; index < object->payload.closure.capacity; index++) {
        object_leave(object->payload.closure.values[index]);
      }

      dealloc(object);
      break;
  }
}

struct object *object_null() {
  return NULL;
}

struct object *object_int32(int value) {
  struct object *object = alloc(OBJECT_SIZE);
  object->reference_count = 1;
  object->type = INT32;
  object->payload.int32.value = value;

  return object;
}

struct object *object_int32_sum(struct object *left, struct object *right) {
  return object_int32(
    left->payload.int32.value + right->payload.int32.value
  );
}

struct object *object_flt32(float value) {
  struct object *object = alloc(OBJECT_SIZE);
  object->reference_count = 1;
  object->type = FLT32;
  object->payload.flt32.value = value;

  return object;
}

struct object *object_flt32_sum(struct object *left, struct object *right) {
  return object_flt32(
    left->payload.flt32.value + right->payload.flt32.value
  );
}


struct object *object_closure_0(
  abstraction abstraction
) {
  struct object *object = alloc(OBJECT_SIZE + OBJECT_REF_SIZE * 0);
  object->reference_count = 1;
  object->type = CLOSURE;
  object->payload.closure.abstraction = abstraction;
  object->payload.closure.capacity = 0;

  return object;
}

struct object *object_closure_1(
  abstraction abstraction,
  struct object *value_0
) {
  struct object *object = alloc(OBJECT_SIZE + OBJECT_REF_SIZE * 1);
  object->reference_count = 1;
  object->type = CLOSURE;
  object->payload.closure.abstraction = abstraction;
  object->payload.closure.capacity = 1;
  object->payload.closure.values[0] = value_0;

  return object;
}

struct object *object_closure_2(
  abstraction abstraction,
  struct object *value_0,
  struct object *value_1
) {
  struct object *object = alloc(OBJECT_SIZE + OBJECT_REF_SIZE * 2);
  object->reference_count = 1;
  object->type = CLOSURE;
  object->payload.closure.abstraction = abstraction;
  object->payload.closure.capacity = 2;
  object->payload.closure.values[0] = value_0;
  object->payload.closure.values[1] = value_1;

  return object;
}

struct object *object_closure_3(
  abstraction abstraction,
  struct object *value_0,
  struct object *value_1,
  struct object *value_2
) {
  struct object *object = alloc(OBJECT_SIZE + OBJECT_REF_SIZE * 3);
  object->reference_count = 1;
  object->type = CLOSURE;
  object->payload.closure.abstraction = abstraction;
  object->payload.closure.capacity = 3;
  object->payload.closure.values[0] = value_0;
  object->payload.closure.values[1] = value_1;
  object->payload.closure.values[2] = value_2;

  return object;
}

struct object *object_closure_4(
  abstraction abstraction,
  struct object *value_0,
  struct object *value_1,
  struct object *value_2,
  struct object *value_3
) {
  struct object *object = alloc(OBJECT_SIZE + OBJECT_REF_SIZE * 4);
  object->reference_count = 1;
  object->type = CLOSURE;
  object->payload.closure.abstraction = abstraction;
  object->payload.closure.capacity = 4;
  object->payload.closure.values[0] = value_0;
  object->payload.closure.values[1] = value_1;
  object->payload.closure.values[2] = value_2;
  object->payload.closure.values[3] = value_3;

  return object;
}

struct object *object_closure_5(
  abstraction abstraction,
  struct object *value_0,
  struct object *value_1,
  struct object *value_2,
  struct object *value_3,
  struct object *value_4
) {
  struct object *object = alloc(OBJECT_SIZE + OBJECT_REF_SIZE * 5);
  object->reference_count = 1;
  object->type = CLOSURE;
  object->payload.closure.abstraction = abstraction;
  object->payload.closure.capacity = 5;
  object->payload.closure.values[0] = value_0;
  object->payload.closure.values[1] = value_1;
  object->payload.closure.values[2] = value_2;
  object->payload.closure.values[3] = value_3;
  object->payload.closure.values[4] = value_4;

  return object;
}

struct object *object_closure_6(
  abstraction abstraction,
  struct object *value_0,
  struct object *value_1,
  struct object *value_2,
  struct object *value_3,
  struct object *value_4,
  struct object *value_5
) {
  struct object *object = alloc(OBJECT_SIZE + OBJECT_REF_SIZE * 6);
  object->reference_count = 1;
  object->type = CLOSURE;
  object->payload.closure.abstraction = abstraction;
  object->payload.closure.capacity = 6;
  object->payload.closure.values[0] = value_0;
  object->payload.closure.values[1] = value_1;
  object->payload.closure.values[2] = value_2;
  object->payload.closure.values[3] = value_3;
  object->payload.closure.values[4] = value_4;
  object->payload.closure.values[5] = value_5;

  return object;
}

struct object *object_closure_7(
  abstraction abstraction,
  struct object *value_0,
  struct object *value_1,
  struct object *value_2,
  struct object *value_3,
  struct object *value_4,
  struct object *value_5,
  struct object *value_6
) {
  struct object *object = alloc(OBJECT_SIZE + OBJECT_REF_SIZE * 7);
  object->reference_count = 1;
  object->type = CLOSURE;
  object->payload.closure.abstraction = abstraction;
  object->payload.closure.capacity = 7;
  object->payload.closure.values[0] = value_0;
  object->payload.closure.values[1] = value_1;
  object->payload.closure.values[2] = value_2;
  object->payload.closure.values[3] = value_3;
  object->payload.closure.values[4] = value_4;
  object->payload.closure.values[5] = value_5;
  object->payload.closure.values[6] = value_6;

  return object;
}

struct object *object_closure_8(
  abstraction abstraction,
  struct object *value_0,
  struct object *value_1,
  struct object *value_2,
  struct object *value_3,
  struct object *value_4,
  struct object *value_5,
  struct object *value_6,
  struct object *value_7
) {
  struct object *object = alloc(OBJECT_SIZE + OBJECT_REF_SIZE * 8);
  object->reference_count = 1;
  object->type = CLOSURE;
  object->payload.closure.abstraction = abstraction;
  object->payload.closure.capacity = 8;
  object->payload.closure.values[0] = value_0;
  object->payload.closure.values[1] = value_1;
  object->payload.closure.values[2] = value_2;
  object->payload.closure.values[3] = value_3;
  object->payload.closure.values[4] = value_4;
  object->payload.closure.values[5] = value_5;
  object->payload.closure.values[6] = value_6;
  object->payload.closure.values[7] = value_7;

  return object;
}

struct object *object_closure_9(
  abstraction abstraction,
  struct object *value_0,
  struct object *value_1,
  struct object *value_2,
  struct object *value_3,
  struct object *value_4,
  struct object *value_5,
  struct object *value_6,
  struct object *value_7,
  struct object *value_8
) {
  struct object *object = alloc(OBJECT_SIZE + OBJECT_REF_SIZE * 9);
  object->reference_count = 1;
  object->type = CLOSURE;
  object->payload.closure.abstraction = abstraction;
  object->payload.closure.capacity = 9;
  object->payload.closure.values[0] = value_0;
  object->payload.closure.values[1] = value_1;
  object->payload.closure.values[2] = value_2;
  object->payload.closure.values[3] = value_3;
  object->payload.closure.values[4] = value_4;
  object->payload.closure.values[5] = value_5;
  object->payload.closure.values[6] = value_6;
  object->payload.closure.values[7] = value_7;
  object->payload.closure.values[8] = value_8;

  return object;
}

struct object *object_apply(struct object *function, struct object *argument) {
  return function->payload.closure.abstraction(
    function->payload.closure.values,
    argument
  );
}

void object_debug(struct object *object) {
  io_string("{ ");

  if (object == NULL) {
    io_string("NULL");
  }

  else {
    switch (object->type) {
      case INT32:
        io_string("INT32 ");
        io_int(object->payload.int32.value);
        break;

      case FLT32:
        io_string("FLT32 ");
        io_float(object->payload.flt32.value);
        break;
      
      case CLOSURE:
        io_string("CLOSURE [ ");

        for (int index = 0; index < object->payload.closure.capacity; index++) {
          object_debug(object->payload.closure.values[index]);
        }

        io_string(" ]");
        break;
    }
  }

  io_string(" }");
}
