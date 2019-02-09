#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
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

slang_ptr entry();

slang_ptr alloc() {
  return (slang_ptr)(slang_value *)malloc(sizeof(slang_value));
}

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

slang_ptr what() {
  int64_t got = 0;
  printf("> ");
  int result = scanf("%lld", &got);
  if (result == EOF) {
    fprintf(stderr, "stdin died :(\n");
    exit(1);
  }
  if (result == 0) {
    while (fgetc(stdin) != '\n')
      ;
  }
  return (slang_ptr)got;
}

int main() {
  printf("%lld\n", entry());
  return 0;
}
