#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>

union slang_value;

typedef union slang_ptr {
  int64_t integer;
  union slang_value *value;
} slang_ptr;

typedef struct {
  slang_ptr left;
  slang_ptr right;
} slang_pair;

typedef struct {
  uint64_t position;
  slang_ptr value;
} slang_union;

typedef struct {
  slang_ptr (*f)(slang_ptr, slang_ptr *);
  slang_ptr *env;
} slang_lambda;

typedef union slang_value {
  slang_pair pair;
  slang_union in;
  slang_ptr ref;
  slang_lambda lambda;
} slang_value;

slang_ptr alloc() {
  slang_ptr built = {.value = malloc(sizeof(slang_value))};
  return built;
}

slang_ptr make_inl(slang_ptr inner) {
  slang_union inl = {.position = 0, .value = inner};
  slang_ptr built = alloc();
  built.value->in = inl;
  return built;
}

slang_ptr make_inr(slang_ptr inner) {
  slang_union inr = {.position = 1, .value = inner};
  slang_ptr built = alloc();
  built.value->in = inr;
  return built;
}

slang_ptr make_pair(slang_ptr left, slang_ptr right) {
  slang_pair pair = {.left = left, .right = right};
  slang_ptr built = alloc();
  built.value->pair = pair;
  return built;
}

slang_ptr fst(slang_ptr pair) { return pair.value->pair.left; }

slang_ptr snd(slang_ptr pair) { return pair.value->pair.right; }

slang_ptr make_ref(slang_ptr inner) {
  slang_ptr built = alloc();
  built.value->ref = inner;
  return built;
}

slang_ptr deref(slang_ptr ref) { return ref.value->ref; }

void assign(slang_ptr ref, slang_ptr value) { ref.value->ref = value; }

slang_ptr make_closure(slang_ptr (*f)(slang_ptr, slang_ptr *), size_t envc,
                       ...) {
  slang_ptr built = alloc();
  slang_ptr *env = calloc(sizeof(slang_ptr), envc);
  slang_lambda lambda = {.f = f, .env = env};
  built.value->lambda = lambda;
  va_list args;
  va_start(args, envc);
  for (size_t i = 0; i < envc; i++)
    env[i] = va_arg(args, slang_ptr);
  va_end(args);
  return built;
}

slang_ptr make_recursive_closure(slang_ptr (*f)(slang_ptr, slang_ptr *),
                                 size_t envc, ...) {
  slang_ptr built = alloc();
  slang_ptr *env = calloc(sizeof(slang_ptr), envc + 1);
  env[0] = built;
  slang_lambda lambda = {.f = f, .env = env};
  built.value->lambda = lambda;
  va_list args;
  va_start(args, envc);
  for (size_t i = 0; i < envc; i++)
    env[i + 1] = va_arg(args, slang_ptr);
  va_end(args);
  return built;
}

slang_ptr apply(slang_ptr closure, slang_ptr arg) {
  slang_lambda lambda = closure.value->lambda;
  return lambda.f(arg, lambda.env);
}

slang_ptr check_case(slang_ptr in, slang_ptr lambda_left,
                     slang_ptr lambda_right) {
  if (in.value->in.position == 0)
    return apply(lambda_left, in.value->in.value);
  else
    return apply(lambda_right, in.value->in.value);
}
