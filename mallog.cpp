#include <stdio.h>
#include <dlfcn.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <inttypes.h>
#include <string.h>
#include <assert.h>

#include "mallog2.h"

void *(*mallocp)(size_t) = 0;
void (*freep)(void*) = 0;

void init() __attribute__((constructor));
void fini() __attribute__((destructor));

void init()
{
	if (mallocp || freep)
	{
		fprintf(stderr, "Error: init() called twice\n");
		exit(1);
	}
	mallocp = (void*(*)(size_t))dlsym(RTLD_NEXT, "malloc");
	freep = (void(*)(void*))dlsym(RTLD_NEXT, "free");
	dlopen("./mallog2.so", RTLD_NOW | RTLD_DEEPBIND);
	printf("Now logging allocations...\n");
	printf("malloc: %p (was %p)\n", (void*)malloc, (void*)mallocp);
	printf("free: %p (was %p)\n", (void*)free, (void*)freep);
}

void fini()
{
	// No-op
}
