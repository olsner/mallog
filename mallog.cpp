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

static int g_logfd = 0;
static bool g_during_init = 0;

static void *(*g_mallocp)(size_t) = 0;
static void *(*g_reallocp)(void*, size_t) = 0;
static void (*g_freep)(void*) = 0;

void init() __attribute__((constructor));
void fini() __attribute__((destructor));

static void openlog();
static void* log_malloc(void* ptr, size_t size);
static void log_free(void* ptr);

static void writ(const void* p, size_t size)
{
	if (!g_logfd)
	{
		openlog();
		if (!g_logfd)
			return;
	}
	int fd = g_logfd;
	while (size)
	{
		ssize_t written = write(fd, p, size);
		if (written == -1)
		{
			if (errno == EINTR)
				continue;
			else
			{
				perror("malloc: write");
				_exit(1);
			}
		}
		else
		{
			p = (char*)p + written;
			size -= written;
		}
	}
}

#define TEMP_BUFFER_SIZE 16384
static char g_temp_buffer[TEMP_BUFFER_SIZE];
static size_t g_temp_length;

static void flush()
{
	if (g_temp_length)
	{
		writ(g_temp_buffer, g_temp_length);
		g_temp_length = 0;
	}
}

static void wri(const void* p, size_t size)
{
	while (size)
	{
		size_t to_write = TEMP_BUFFER_SIZE - g_temp_length;
		if (to_write)
		{
			if (to_write > size)
				to_write = size;
			memcpy(g_temp_buffer + g_temp_length, p, to_write);
			g_temp_length += to_write;
			size -= to_write;
			p = (char*)p + to_write;
		}
		else
		{
			flush();
		}
	}
}

#define wr(p,c) do { wri((p),(c)*sizeof(uintptr_t)); } while (0)

static void log_free(void* ptr)
{
	uintptr_t log[2] = { 0, uintptr_t(ptr) };
	wr(log, 2);
}

static void* log_malloc(void* ptr, size_t size)
{
	uintptr_t log[2] = { (size << 1) | 1, uintptr_t(ptr) };
	wr(log, 2);
	return ptr;
}

static void openlog()
{
	char buffer[64];
	snprintf(buffer, sizeof(buffer), "/tmp/mallog.%d.dat", getpid());
	//printf("Logging allocations to %s...\n", buffer);

	int fd = open(buffer, O_APPEND | O_CLOEXEC | O_CREAT | O_WRONLY, 0660);
	if (fd == -1)
	{
		perror("mallog: open log");
		_exit(1);
	}
	else
	{
		g_logfd = fd;
	}

	// Output some stats about the current process to log?
}

#define GRAB(name, type) \
	void* name##p = dlsym(RTLD_NEXT, #name); \
	assert(name##p && name##p != name); \
	g_##name##p = (type)name##p

void init()
{
	g_during_init = true;

	GRAB(malloc, void*(*)(size_t));
	GRAB(free, void(*)(void*));
	GRAB(realloc, void*(*)(void*, size_t));

	g_during_init = false;
}

void fini()
{
	if (g_logfd)
	{
		flush();
		while (close(g_logfd) && errno != EBADF)
			perror("mallog: close g_logfd");
		g_logfd = 0;
	}
}

void *malloc(size_t size)
{
	if (g_during_init)
	{
		return NULL;
	}

	if (!g_mallocp)
	{
		// This may recursively call malloc, but that's OK because we check if
		// we're init init above.
		init();
	}

	return log_malloc(g_mallocp(size), size);
}

void* calloc(size_t n, size_t sz)
{
	size_t size = n * sz;
	void* ptr = malloc(size);
	if (ptr) memset(ptr, 0, size);
	return ptr;
}

void* realloc(void* ptr, size_t new_size)
{
	void* ret = g_reallocp(ptr, new_size);
	if (ret)
	{
		log_free(ptr);
		log_malloc(ret, new_size);
	}
	return ret;
}

void free(void *ptr)
{
	if (ptr)
	{
		log_free(ptr);
		g_freep(ptr);
	}
}
