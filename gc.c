/* This file contains code that relates to the garbage collector and implicit memory management */

#include "runtime.h"
#include "mem.h"

void garbageCollect(int64_t ref);
void decrementRefCount(int64_t ref);
void gcError(const char* msg);

extern bool gc_info;

/* Add some or all of the memory associated with ref to the free list */
void garbageCollect(int64_t ref) {      
        GC_INFO("About to garbage collect ");
        if(gc_info == true) {
                printValue(ref);
                printf("\n");
        }
        
        switch(ref & result_type_mask) {
                case type_bignum:
                        //Bignums do not have inner chunks so there is no need to attempt to recursively garbage collect
                        //Place the 24 bytes used by the bignum back on the free list
                        addToFreeList((ref ^ type_bignum), 24);
			
			break;
                case type_list:
                        {
                        int64_t* ref_p = (int64_t*) (ref ^ type_list); //Get pointer to the ref count of the list
                        int64_t head = *(ref_p + 1); //get tagged pointer to the head of the list
                        decrementRefCount(head);
                        int64_t tail = *(ref_p + 2); //get tagged pointer to the tail of the list or get the empty list
			
			//If the tail of the list is another list, decrement its ref count
			//possibly triggering its garbage collection. Otherwise, it is the
			//empty list which has no ref count. It is automatically garbage.
                       	 decrementRefCount(tail);

                        //Place the 24 bytes used by the cons back on the free list
                        addToFreeList((ref ^ type_list), 24);
			
			}
                        break;
                case type_pair:
                        {
                        int64_t* ref_p = (int64_t*) (ref ^ type_pair); //Get pointer to the ref count of the pair
                        int64_t first = *(ref_p + 1); //get tagged pointer to the first of the pair
                        decrementRefCount(first);
                        int64_t second = *(ref_p + 2); //get tagged pointer to the second of the pair
                        decrementRefCount(second);
                        //Place the 24 bytes used by the cons back on the free list
			addToFreeList((ref ^ type_pair), 24);
			
                        }
                        break;
                case type_range:
                        {
                        int64_t* ref_p = (int64_t*) (ref ^ type_range);
                        
                        int64_t beginning = *(ref_p + 1); //get the tagged pointer to the beginning of the range
                        decrementRefCount(beginning);

                        int64_t end = *(ref_p + 2); //get the tagged pointer to the end of the range
                        decrementRefCount(end);
                                                
                        int64_t step = *(ref_p + 3); //get the tagged pointer to the step value of the range
                        decrementRefCount(step);
                        //Add the 32 contiguous bytes that made up the range to the free list 
			addToFreeList((ref ^ type_range), 32);
			
                      	}
                        break;
                case type_box:
                        {
                        int64_t* ref_p = (int64_t*) (ref ^ type_box);
                        int64_t value = *(ref_p + 1); //get the tagged pointer to the value in the box
                        decrementRefCount(value);
                        //Add the 16 contiguous bytes that made up the box to the free list
			addToFreeList((ref ^ type_box), 16);
			
                        }
                        break;
		case type_string:
			{
			int64_t* ref_p = (int64_t*) (ref ^ type_string);
			int64_t len_in_bytes = *(ref_p + 1);
			addToFreeList((int64_t)ref_p, len_in_bytes);
			}
			break;
                default:
                        gcError("Attempt to garbage collect a value that is not a chunk");
        }
}

void decrementRefCount(int64_t ref) {
        int64_t type = -1; //Assum by defualt that ref is an immediate value;
        switch(ref & result_type_mask) {
                case type_bignum:
                        type = type_bignum;
                        break;
                case type_list:
                        type = type_list;
                        break;
                case type_pair:
                        type = type_pair;
                        break;
                case type_range:
                        type = type_range;
                        break;
                case type_box:
                        type = type_box;
                        break;
		case type_string:
			type = type_string;
			break;
                default:
                        break;

        }


        //Since all chunks have their reference count as their first 8 bytes,
        //they can share the same code for decrementing their ref count
        //and possibly triggering garbage collection.
        if(type != -1) { //Check that ref is a pointer to a chunk
                int64_t* ref_p = (int64_t*) (ref ^ type);
                if(validateAddr(ref ^ type)) {
			*ref_p = *ref_p - 1; //Decrement the ref count of the chunk
                	if(*ref_p == 0) {
                        	GC_INFO("Will garbage collect: ");
                        	if(gc_info == true) {
                                	printValue(ref);
                                	printf("\n");
                        	}
                        	garbageCollect(ref);
			}
                }
        }

}

void gcError(const char* msg) {
	fprintf(stderr, "Garbage collector failed: %s\n", msg);
	exit(1);
}
