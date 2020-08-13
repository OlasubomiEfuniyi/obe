#include <inttypes.h>

#define heap_size 8000000000

int64_t* init_heap(void);
int64_t allocateChunk(short size);
void addToFreeList(int64_t start, short size);
void printFreeList(void);
