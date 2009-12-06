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

#define BOOT_HEAP_SIZE 32
static char g_bootheap[BOOT_HEAP_SIZE] __attribute__((aligned(8)));
static char* g_bootfree = 0;
static char *const g_bootend = g_bootheap + BOOT_HEAP_SIZE;

void *(*g_mallocp)(size_t) = 0;
void (*g_freep)(void*) = 0;

void init() __attribute__((constructor));
void fini() __attribute__((destructor));

static void openlog();

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

void init()
{
	g_bootfree = g_bootheap;
	void* mallocp = dlsym(RTLD_NEXT, "malloc");
	void* freep = dlsym(RTLD_NEXT, "free");
	assert(mallocp && mallocp != malloc);
	assert(freep && freep != free);
	g_mallocp = (void*(*)(size_t))mallocp;
	g_freep = (void(*)(void*))freep;
	g_bootfree = 0;
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
	if (g_bootfree)
	{
		void* ret = g_bootfree;
		g_bootfree += size;
		assert(g_bootheap <= g_bootfree && g_bootfree <= g_bootend);
		//printf("Boot malloc: %lu (now %lu used)\n", (long)size, long(g_bootfree - g_bootheap));
		return ret;
	}

	if (!g_mallocp)
	{
		// This may recursively call malloc, but that's OK because it will also
		// set up a temporary heap that will be used by the check above.
		init();
	}

	void *ptr = g_mallocp(size);
	uintptr_t log[2] = { (size << 1) | 1, uintptr_t(ptr) };
	wr(log, 2);
	return ptr;
}

void* calloc(size_t n, size_t sz)
{
	size_t size = n*sz;
	/*if (size / n != sz || size / sz != n)
		return NULL;*/
	void* ptr = malloc(size);
	memset(ptr, 0, size);
	return ptr;
}

void free(void *ptr)
{
	if (!ptr)
		return;

	if (ptr >= g_bootheap && ptr <= g_bootend)
		return;

	uintptr_t log[2] = { 0, uintptr_t(ptr) };
	wr(log, 2);
	g_freep(ptr);
}
