/* This file includes code for allocating a chunk of memory and freeing a chunk of memory */
#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "mem.h"

void memError(const char* msg);

struct Chunk {
        int64_t start; //The start address of the chunk of memory on the heap
        short size; //The number of contiguous bytes that make up this chunk of memory. Should always be a multiple of 8
        struct Chunk* next; //A pointer to the next free chunk of Memory
};
struct Chunk* free_list = NULL;
int64_t* heap = NULL;
int64_t next_free_pos_in_heap = 0;
int64_t end_address = -1;

/* This function creates the heap and returns a pointer to it */
int64_t* init_heap() {
	heap = (int64_t*) malloc(heap_size);
	if(heap == NULL) {
		memError("Could not malloc a heap");
	}
	
	next_free_pos_in_heap = (int64_t) heap; //When just initialized, the next free position on the heap is at the beginning of the heap	
	end_address = (((int64_t) heap) + heap_size); //The first address that exceeds the upper bound of the heap

	return heap;
}

/* Allocate space on the heap for a chunk of the given size */
int64_t allocateChunk(short size) {
	assert((size % 8)  == 0); //Make sure the addresses are kept as multiple of 8 bytes
	int64_t chunk = 0;

	//Determine if there is enough space between the next free position on the heap
	//and the end of the heap to satisfy the request
	if((next_free_pos_in_heap + ((int64_t) size)) <= end_address) {
		//There is enough space. If equal, the next free position on the heap will be out of bounds
		chunk = next_free_pos_in_heap;
		next_free_pos_in_heap += size;
			
	} else { //We do not have enough space. Scan the free list
		struct Chunk* current = free_list;
		struct Chunk* prev = NULL;

		while(current != NULL) { //Find the first chunk on the free list that can satisfy the request
			if((current -> size) >= size) {
				break;
			}

			prev = current; 
			current = current -> next;
			
		}

		if(current != NULL) { //A chunk was found on the free list that can satisfy the request
			chunk = (current -> start);

			//Remove current from the free list
			if(prev == NULL) { //Curret is the head of the free list
				free_list = free_list -> next;
			} else {
				prev -> next = current -> next;
				free(current);
			}
		} else {
			memError("Unable to allocate memory on the heap");
		}
	}

	
	return chunk;
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

