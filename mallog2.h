
extern "C" void *(*mallocp)(size_t) __attribute__((visibility("default")));
extern "C" void (*freep)(void*) __attribute__((visibility("default")));

extern "C" void mallog2_init() __attribute__((visibility("default")));

