#ifndef OBJECT_H
#define OBJECT_H

struct object;

typedef struct object *(*abstraction)(struct object *[], struct object *);

void object_enter(struct object *);
void object_leave(struct object *);

struct object *object_int32(int);
struct object *object_int32_sum(struct object *, struct object *);

struct object *object_flt32(float);
struct object *object_flt32_sum(struct object *, struct object *);

struct object *object_closure_0(
    abstraction
);
struct object *object_closure_1(
    abstraction,
    struct object *
);
struct object *object_closure_2(
    abstraction,
    struct object *,
    struct object *
);
struct object *object_closure_3(
    abstraction,
    struct object *,
    struct object *,
    struct object *
);
struct object *object_closure_4(
    abstraction,
    struct object *,
    struct object *,
    struct object *,
    struct object *
);
struct object *object_closure_5(
    abstraction,
    struct object *,
    struct object *,
    struct object *,
    struct object *,
    struct object *
);
struct object *object_closure_6(
    abstraction,
    struct object *,
    struct object *,
    struct object *,
    struct object *,
    struct object *,
    struct object *
);
struct object *object_closure_7(
    abstraction,
    struct object *,
    struct object *,
    struct object *,
    struct object *,
    struct object *,
    struct object *,
    struct object *
);
struct object *object_closure_8(
    abstraction,
    struct object *,
    struct object *,
    struct object *,
    struct object *,
    struct object *,
    struct object *,
    struct object *,
    struct object *
);
struct object *object_closure_9(
    abstraction,
    struct object *,
    struct object *,
    struct object *,
    struct object *,
    struct object *,
    struct object *,
    struct object *,
    struct object *,
    struct object *
);
struct object *object_apply(struct object *, struct object *);

void object_debug(struct object *);

#endif
