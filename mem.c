/* This file includes code for allocating a chunk of memory and freeing a chunk of memory */
#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include "mem.h"

void memError(const char* msg);

struct Chunk {
        int64_t start; //The start address of the chunk of memory on the heap
        short size; //The number of contiguous bytes that make up this chunk of memory. Should always be a multiple of 8
        struct Chunk* next; //A pointer to the next free chunk of Memory
};
struct Chunk* free_list = NULL;
int64_t* heap = NULL;

/* This function creates the heap and returns a pointer to it */
int64_t* init_heap() {
	heap = (int64_t*) malloc(heap_size);
	if(heap == NULL) {
		memError("Could not malloc a heap");
	}
	
	return heap;
}

/* Exit with an error */
void memError(const char* msg) {
	printf("The memory manager failed: %s\n", msg);
	exit(1);
}

/* Add a new chunk to the free list */
void  addToFreeList(int64_t start, short size) {
        //Add the new chunk to the beginning of the free list
        struct Chunk* chunk = free_list;

        free_list = (struct Chunk*) malloc(sizeof(struct Chunk));
        if(free_list == NULL) { //check if malloc failed
                memError("Could not malloc chunk for free list");
        }

        free_list -> start = start;
        free_list -> size = size;
        free_list -> next = chunk;
}

/* Print the chunks in the free list */
void printFreeList() {
        struct Chunk* current = free_list;

        while(current != NULL) {
                printf("Start: %" PRId64 ", Size: %d\n", (current -> start), (current -> size));
                current = (current -> next);
        }
}

