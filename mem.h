#include <inttypes.h>

#ifndef GC_INFO
	#define GC_INFO(...) (gc_info && printf(__VA_ARGS__))
#endif

#ifndef true
	#define true 1
#endif

#ifndef false
	#define false 0
#endif

typedef char bool;
int64_t* init_heap(int64_t h_size);
int64_t allocateChunk(short size);
void addToFreeList(int64_t start, short size);
void printFreeList(void);
bool validateAddr(int64_t addr);
