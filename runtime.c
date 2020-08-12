/* This file contains code for starting the program, printing its results and pefroming some routines required by the running program */
#include "runtime.h"

int64_t entry(void* heap, void* heap_start_addr, int64_t h_size);
void printResult(int64_t value);
void printBignum(int64_t value);
void printList(int64_t value);
void printPair(int64_t value);
int64_t compValue(int64_t value1, int64_t value2);
int comBignum(int64_t arg0, int64_t arg1);
int error(void);

bool gc_info = false; //Set to true if garbage collection  information should be printed with chunks and upon triggering garbage collection. false otherwise.

/* From this method, the compiled program is initiated by calling the entry function in assembly.
The entry function is linked with this RTS 
We use the GMP Library to support arbitrary precision integers */
int main(int argc, char** argv) {
	void* heap = NULL;
	heap = malloc(heap_size);

	if(heap == NULL) {
		fprintf(stderr, "could not allocate heap space");
		exit(1);
	}
	
	//Check if printing gc info is desired
	if(argc > 1 && (strcmp(*(argv + 1), "gc") == 0)) {
		gc_info = true;
	}

	/* Entry may either return a 64 bit value via a register or a pointer to a result */
	int64_t  value = entry(heap, heap, heap_size);
	
	printResult(value);

	GC_INFO("Free List:\n");
	if(gc_info == true) {
		printFreeList();
	}
	
	free(heap);
	return 0;
}

void my_mpz_init(int64_t x) { 
	mpz_init(*((mpz_t*) x));
}

void my_mpz_add(int64_t rop, int64_t op1, int64_t op2) {
	assert(rop % 8 == 0);
	assert(op1 % 8 == 0);
	assert(op2 % 8 == 0);	

	mpz_add(*((mpz_t *)rop), *((mpz_t*)(((int64_t*)op1) + 1)), *((mpz_t*)(((int64_t *)op2) + 1)));
}

/* Increment the untagged pointer to a bignum, storing the result in res */
void increment(int64_t arg, int64_t res) {
	mpz_add_ui(*((mpz_t*)res), *((mpz_t*)(((int64_t *) arg) + 1)), 1);
}

void my_mpz_sub(int64_t rop, int64_t op1, int64_t op2) {
	assert(rop % 8 == 0);
	assert(op1 % 8 == 0);
	assert(op2 % 8 == 0);	

	mpz_sub(*((mpz_t *)rop), *((mpz_t*)(((int64_t *)op1) + 1)), *((mpz_t*)(((int64_t *)op2) + 1)));
}

/* Decrement the untagged pointer to a bignum, placing the result in res*/
void decrement(int64_t arg, int64_t res) {
	int64_t* argument = (int64_t* )arg;
	mpz_sub_ui(*((mpz_t*)res), *((mpz_t*)(argument + 1)), 1);
}
/** Compile a bignum into a GMP structure pointer.
    bn_str_ptr is a pointer to the string representation of the bignum.
    bn_struct_ptr is a pointer to the location on the heap where the
    struct will be saved.
    Return the size of the GMP struct representing the big num
*/

int64_t compileBignum(int64_t bn_str_ptr, int64_t bn_struct_ptr) {	
	int flag = 0;

	//Setup the GMP structure
	mpz_t bn;

	mpz_init(bn);
	mpz_set_ui(bn, 0);
//
	//Copy the string representation of the bignum into a seperate buffer
	int len = (int) strlen((char*)bn_str_ptr); 
	char* str = malloc(len + 1);

	if(str == NULL) {
		runtimeSystemError();
	}

	strncpy(str, (char*)bn_str_ptr, len + 1);
 	
	//Use the string value of the bignum to setup the GMP struct
	flag = mpz_set_str(bn, str, 10);
	if(flag != 0) {
		runtimeSystemError();
	}

	//Put bn on the heap starting from the address in bn_struct_ptr
	memcpy((void *)bn_struct_ptr, (void *)&bn, sizeof(bn));
	return sizeof(bn); 
}

/* Compare two bignums  */
int64_t compBignum(int64_t arg0, int64_t arg1) {
	/* Return the result of comparing the two bignums using the mpz_cmp function from the GMP library */	
	int64_t* argument0 = (int64_t *)arg0;
	int64_t* argument1 = (int64_t *)arg1;

	int64_t comp_res =  mpz_cmp(*((mpz_t*)(argument0 + 1)), *((mpz_t*)(argument1 + 1)));
	return comp_res;
}

/* Compare a bignum with a signed integer */
int64_t compBignumSI(int64_t arg0, int64_t arg1) {
	/* Return the result of comparing the bignum with the signed integer using the mpz_cmp_si function from the 
	GMP library */
	int64_t* argument0 = (int64_t*)arg0;

	int64_t comp_res = mpz_cmp_si(*((mpz_t *)(argument0 + 1)), arg1);
	return comp_res;
}

/* Determine if two lists are equal by comparing successive pairs of elements. 
Returns 0 if they are equal and -1 otherwise*/
int64_t listEqual(int64_t arg0, int64_t arg1) {
	int64_t* a0 = (int64_t*)(arg0 ^ type_list);
	int64_t* a1 = (int64_t*)(arg1 ^ type_list);
	
	if((arg0 == type_empty_list && arg1 != type_empty_list) || 
		(arg0 != type_empty_list && arg1 == type_empty_list)) {
		return -1;
	} else if(arg0 == type_empty_list && arg1 == type_empty_list) {
		return 0;
	} else if(compValue(*(a0 + 1), *(a1 + 1)) != 0) { //The head of the list is at offset 1
		return -1;
	} else {
		return listEqual(*(a0 + 2), *(a1 + 2));  //The tail of the list is at offset 2
	}
}

/* Determine if two pairs are equal by comparing successive pairs of elements.
Returns -1 if not equal and 0 if it is. */
int64_t pairEqual(int64_t arg0, int64_t arg1) { 
	int64_t* a0 = (int64_t*)(arg0 ^ type_pair);
	int64_t* a1 = (int64_t*)(arg1 ^ type_pair);
	
	if(compValue(*(a0 + 1), *(a1 + 1)) != 0) { //The first element of the pair is at offset 1
		return -1;
	} 

	if(((((*(a0 + 2)) & result_type_mask) == type_pair) && 
		(((*(a1 + 2)) & result_type_mask) != type_pair)) ||
	   ((((*(a0 + 2)) & result_type_mask) != type_pair) && 
		(((*(a1 + 2)) & result_type_mask) == type_pair))) { //The second element of the pair is at offset 2
		return -1;
	} else if(((((*(a0 + 2)) & result_type_mask) != type_pair) && 
		(((*(a1 + 2)) & result_type_mask) != type_pair))) { //The second element of the pair is at offset 2 
		return compValue(*(a0 + 2), *(a1 + 2));
	} else {
		return pairEqual(*(a0 + 2), *(a1 + 2)); //The second element of the pair is at offset 2
	}
}

/* print the result of evaluating an expression */
void printResult(int64_t value) {
	/* Print the result */
	printValue(value);
	printf("\n");
}

/* Print a value. If printEmpty is true, print the empty list symbol, otherwise do not */
void printValue(int64_t value) {
	int64_t* addr;

	switch(value & result_type_mask) {
	case type_bignum:			
		printBignum(value);	
		break;
	case type_list:
		printf("(");
		printList(value);
		printf(")");
		break;
	case type_pair:
		printf("(");
		printPair(value);
		printf(")");
		break;
	case type_imm:
		switch(value) {
			case type_empty_list:
				printf("()");
				break;
			case type_true:
				printf("#t");
				break;
			case type_false:
				printf("#f");
				break;
			default:
				error();	
		}
		break;
	case type_range:
		GC_INFO("Ref Count: %" PRId64 " Value: ", *((int64_t *)(value ^ type_range)));
		printf("(");
		addr = ((int64_t *) (value ^ type_range)) + 1;
		printBignum(*addr);
		printf("..=");
		printBignum(*(addr + 1));
		printf(" ");
		printBignum(*(addr + 2));
		printf(")");
		break;
	case type_box:
		GC_INFO("Ref Count: %" PRId64 " Value: ", *((int64_t*)(value ^ type_box)));
		printf("#&");
		printValue(*(((int64_t*)(value ^ type_box) + 1)));

		break;
	default:
		error();
	}

}

/* Print a list of values */
void printList(int64_t value) {
	int64_t* start_addr = (int64_t *)(value ^ type_list);
	if(*start_addr != type_empty_list) {
		GC_INFO("[Ref count: %" PRId64 "]", *start_addr);
		start_addr++;
		printValue(*start_addr);
		value = *(start_addr + 1);
		if ((value & result_type_mask) == type_list) {
			//Print a space if the next thing to print is not the empty list
			if((*((int64_t* )(value ^ type_list)) & result_type_mask) != type_empty_list) {
				printf(" ");
			}
			printList(value);
		}
	}
}

/* Print a pair of values */
void  printPair(int64_t value) {
	int64_t* start_addr  = (int64_t *)(value ^ type_pair);
		GC_INFO("[Ref count: %" PRId64 "]", *start_addr);
		start_addr++;
		printValue(*start_addr);
		value = *(start_addr + 1);
		//Check if the next value is another pair or the final value in
		//a chain of pairs
		if((value & result_type_mask) == type_pair) {
			printf(" ");
			printPair(value);
		} else {
			printf(" . ");
			printValue(value);
		}
	

}

/* Given a pointer to a gmp struct, print the bignum it represents */
void printBignum(int64_t value) {
	//Clear the tagging to get the address
	int64_t* result = (int64_t*)(value ^ type_bignum);
 
	GC_INFO("Ref Count: %" PRId64 " Value: ", *result);
	mpz_t* gmp = (mpz_t*)(result + 1);
	
	mpz_out_str(stdout,10,*gmp);
}


/* Fully rotate the string in place */
void rotateString(char* str) {
	int64_t len = strlen(str);
	char temp;
	
	//Note: While swapping the terminating null character(s) is/are left alone
	if(str[0] != '-') { //We are dealing with a non negative number
		for(int i = 0; i < (len/2); i++) {
			//Swap the ith digit with the (i + 1) to the last digit
			temp = str[len - (i + 1)]; // Save the (i + 1) to  the last digit
			str[len - (i + 1)] = str[i];
			str[i] = temp;
		}		
	} else { //We are dealing with a negative number
		for(int i = 1; i <= ((len - 1) /2); i++) {
			//Swap the ith digit with the ith to the last digit
			temp = str[len - i]; // Save the ith to  the last digit
			str[len - i] = str[i];
			str[i] = temp;
		}	

	}
}

/* Determine which comparison function to use based on the type of value to be compared */
int64_t compValue(int64_t value1, int64_t value2) {
	switch(value1 & result_type_mask) {
		case type_bignum:
			//untag pointers to bignum as expected by compBignum
			return compBignum((value1 ^ type_bignum), (value2 ^ type_bignum));
		case type_list:
			return listEqual(value1, value2);
		case type_pair:
			return pairEqual(value1, value2);
		case type_imm:
		switch(value1) {
			case type_empty_list:
			case type_true:
			case type_false:
				if(value1 == value2) {
					return type_true;
				} else {
					return type_false;
				}
			default:
				runtimeSystemError();	
		}
		break;
		default:
			error();
	}
}

/* Signal an error while executing the program */
int error() {
	printf("err\n");
	exit(0); //The program being executed errored out, but the runtime system did not
}

/* Signal an error due to a full heap upon attemtpting to access the invalid position now being pointed to*/
int nomem() {
	printf("err: Heap is Full\n");
	exit(0); //The program being executed errored out, but the runtime system did not
}

/* Signal an error in the runtime system */
int runtimeSystemError(void) {
	exit(1);
}

