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
#include <sys/mman.h>
#include <assert.h>
#include <time.h>

static int REPS = 100000;

static void usage(const char* pname)
{
	fprintf(stderr, "Usage: %s LOGFILE\n", pname);
	fprintf(stderr, "\nWhere LOGFILE points to a mallog log processed by Convert.hs\n");
}

static void free_array(void** array, size_t size)
{
	for (void**p = array + size; p >= array; p--)
	{
		if (*p)
		{
			free(*p);
			*p = 0;
		}
	}
}

int64_t timestamp_ns()
{
	timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return int64_t(ts.tv_sec) * 1000000000 + ts.tv_nsec;
}

template <typename word>
void run(void** array, size_t array_size, word* data, size_t count)
{
	size_t allocated = 0;
	count >>= 1;
	//int64_t t1 = timestamp_ns();
	while (count--)
	{
		word index = *data++;
		word size = *data++;
		assert(index <= array_size);
		if (size)
		{
			//printf("array[%ld]: %p\n", (long)index, array[index]);
			assert(!array[index]);
			array[index] = malloc(size);
			allocated++;
			//printf("malloc(%lx, %lx): %p\n", (long)index, (long)size, array[index]);
		}
		else
		{
			//printf("free(%lx): %p\n", (long)index, array[index]);
			//assert(array[index]);
			if (array[index]) allocated--;
			free(array[index]);
			array[index] = NULL;
		}
	}
	//int64_t t2 = timestamp_ns();
	free_array(array, array_size);
	//int64_t t3 = timestamp_ns();
	//printf("Bench: %ldns, free: %ldns (%lu of %lu pointers remained)\n", t2-t1, t3-t2, allocated, array_size+1);
}

int main(int argc, char* argv[])
{
	if (argc < 2 || argc > 3 || (argc == 3 && !atoi(argv[2])))
	{
		usage(argv[0]);
		exit(1);
	}

	if (argc >= 3)
	{
		REPS = atoi(argv[2]);
	}

	const char* file = argv[1];
	int fd = open(file, O_RDONLY);
	if (fd < 0)
	{
		perror("open");
		exit(1);
	}

	struct stat statbuf;
	int res = fstat(fd, &statbuf);
	if (res != 0)
	{
		perror("stat");
		exit(1);
	}

	void* buffer = mmap(NULL, (size_t)statbuf.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
	if (buffer == MAP_FAILED)
	{
		perror("mmap");
		exit(1);
	}

	uint64_t size = *(uint64_t*)buffer;
	void** array = (void**)calloc(size + 1, sizeof(void*));
	if (!array)
	{
		fprintf(stderr, "OOM allocating array (%ld pointers)\n", (long)size);
		exit(1);
	}
	buffer = (uint64_t*)buffer+1;
	if (size == (uint32_t)size)
	{
		for (int i = 0; i < REPS; i++)
		{
			run(array, size, (uint32_t*)buffer, (statbuf.st_size - 8) / sizeof(uint32_t));
		}
	}
	else
	{
		for (int i = 0; i < REPS; i++)
		{
			run(array, size, (uint64_t*)buffer, (statbuf.st_size - 8) / sizeof(uint64_t));
		}
	}
}
