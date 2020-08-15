/* This file includes code for allocating a chunk of memory and freeing a chunk of memory */
#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "mem.h"


void memError(const char* msg);
void compact(int64_t num_garbage_bytes);

struct Chunk {
        int64_t start; //The start address of the chunk of memory on the heap
        short size; //The number of contiguous bytes that make up this chunk of memory. Should always be a multiple of 8
        struct Chunk* next; //A pointer to the next free chunk of Memory
};

struct Chunk* free_list = NULL;

extern bool gc_info;
int64_t bytes_allocated = 0;
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
		GC_INFO("Scanning free list for more space\n");
		
		struct Chunk* current = free_list;
		struct Chunk* prev = NULL;
		//Keep track of the number of bytes that are free. If we cannot find a single contiguous
		//chunk on the free list large enough to satisfy the request, but there are enough smaller chunks
		//spread around that can together satisfy the request, we will compact.
		int64_t num_bytes_seen = 0; 
		
		while(current != NULL) { //Find the first chunk on the free list that can satisfy the request
			num_bytes_seen += (int64_t)(current->size);

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
		} else if(num_bytes_seen >= size) { //The request can be satisfied after compaction
			compact(num_bytes_seen);	
			memError("The request can be satisfied after compaction");
		} else {
			if(gc_info == true) {
				printFreeList();
			}
			memError("Unable to allocate memory on the heap");
		}
	}

	bytes_allocated += (int64_t) size;
	
	if(gc_info == true) {
		printf("Bytes allocated: %" PRId64 "\n", bytes_allocated);
	}

	return chunk;
}

void compact(int64_t num_garbage_bytes) {
	size_t temp_heap_size = heap_size - num_garbage_bytes; 
	assert(temp_heap_size >= 0);

	if(temp_heap_size > 0) {//Then there is something to be kept
		char* temp_heap = (char*)malloc(temp_heap_size);
		if(temp_heap == NULL) {
			memError("Unable to allocate temporary heap for compaction");
		}
		char* curr_temp_heap = temp_heap;
		char* curr_heap = (char*)heap;
		struct Chunk* curr_free_list = free_list;

		while(curr_free_list != NULL) {
			//Continue to copy non garbage contiguous chunks into temp_heap until all garbage has been ignored
			size_t bytesToCopy = (size_t)((curr_free_list -> start) - (int64_t)curr_heap);
		
			memcpy((void*)curr_temp_heap, (void*)curr_heap, bytesToCopy);

			curr_temp_heap += bytesToCopy; //Point to next free position on temp_heap
			curr_heap += (bytesToCopy + (curr_free_list -> size)); //point to the next byte after garbage
			curr_free_list = curr_free_list -> next;
		}
		
		//Nothing is left on the free list, so everything we encounter from now on is to be kept		
		size_t bytesToCopy =  (size_t)(end_address - (int64_t)curr_heap);
	
		memcpy((void*)curr_temp_heap, (void*)curr_heap, bytesToCopy);

		curr_temp_heap += bytesToCopy;
		curr_heap += bytesToCopy;

		assert((int64_t)curr_temp_heap == ((int64_t)temp_heap + (int64_t)temp_heap_size));
		assert((int64_t)curr_heap == ((int64_t)heap + heap_size));	

		//Copy over the temp heap into the heap
		memcpy((void*)heap, (void*) temp_heap, temp_heap_size);
		next_free_pos_in_heap = (int64_t)heap + temp_heap_size;
		
		free(temp_heap);
	} else {
		//There is nothing to be kept. The entire heap is available again.
		next_free_pos_in_heap = (int64_t)heap;
	}
}

/* Exit with an error */
void memError(const char* msg) {
	printf("The memory manager failed: %s\n", msg);
	exit(1);
}

/* Add a new chunk to the free list in ascending order of start address. Keeping the free
 list in ascending order helps with compaction */
void  addToFreeList(int64_t start, short size) {
        struct Chunk* current = free_list;
	struct Chunk* prev = NULL;

	if(free_list == NULL) { //The free list is empty
		free_list = (struct Chunk*) malloc(sizeof(struct Chunk));
        	if(free_list == NULL) { //check if malloc failed
                	memError("Could not malloc chunk for free list");
       		}

		free_list -> start = start;
        	free_list -> size = size;
        	free_list -> next = NULL;
	} else { //Traverse the free list to find where to place the new entry
		struct Chunk* chunk = malloc(sizeof(struct Chunk));
		if(chunk ==  NULL) {
			memError("Could not malloc chunk for free list");
		}

		chunk -> start = start;
		chunk -> size = size;

		while(current != NULL) {
			assert((current -> start) != start); //Can't already be on the free list if you are just being freed
			if((current -> start) > start) {
				break; //The new entry should come between prev and current
			}
			prev = current;
			current = (current -> next);
		}
		
		if(current == NULL) {
			//Insert new chunk at the end of the free_list
			prev -> next = chunk;
			chunk -> next = NULL;
		} else if (prev == NULL) {
			//Insert new chunk at the beginning of the free_list
			chunk->next = free_list;
			free_list = chunk;
		} else  {
			//Insert new chunk in between two entries in the free list
			prev -> next = chunk;
			chunk -> next = current;
		}

	}

}

/* Print the chunks in the free list */
void printFreeList() {
        struct Chunk* current = free_list;

        while(current != NULL) {
                printf("Start: %" PRId64 ", Size: %d\n", (current -> start), (current -> size));
                current = (current -> next);
        }
}

