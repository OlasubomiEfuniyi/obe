#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>
#include <gmp.h>
#include <assert.h>
#include <string.h>

#define result_mask 0b111
#define result_shift 3
#define type_integer 0b000
#define type_bignum 0b001
#define type_true 0b010
#define type_false 0b011
#define type_empty_list 0b111
#define type_list  0b100
#define type_pair 0b101

#define heap_size 10000000

#define true 1
#define false 0

typedef char bool;
int64_t entry(void* heap);
void printBignum(int64_t value);
void printValue(int64_t value);
void printList(int64_t value);
void printPair(int64_t value);
int64_t compValue(int64_t value1, int64_t value2);
int comBignum(int64_t arg0, int64_t arg1);
int error(void);
int runtimeSystemError(void);

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

	/* Entry may either return a 64 bit value via a register or a pointer to a result */
	int64_t  value = entry(heap);
	
	/* Print the result */
	printValue(value);
	printf("\n");

	free(heap);
	return 0;
}


/* Add two big nums and return the length of the result string  */
int64_t  addBignum(int64_t arg0, int64_t  arg1, int64_t arg2) {
	int flag; //To hold the result of parsing the return value as a base 10 number.
	mpz_t result1;
	mpz_t result2;
	mpz_t addition_result;
	

	if(((arg1 & result_mask) == type_bignum) && ((arg2 & result_mask) == type_bignum)) { //We are addign two bignums
		/* Initialize the number */
		mpz_init(result1);
		mpz_init(addition_result);

		mpz_set_ui(addition_result, 0);
		mpz_set_ui(result1, 0);

		arg1 = (arg1 ^ type_bignum);

		//Parse the value as a base 10 number, reading it directly from the heap
		//from the beginning of the string to the null character
		flag = mpz_set_str(result1, ((char*) arg1), 10);
		assert(flag == 0); //If the flag is not 0, then the operation failed

		
		mpz_init(result2);
		mpz_set_ui(result2, 0);

		//Clear the tagging to get the address. The location where the result will be placed is untagged
		arg2 = (arg2 ^ type_bignum);

		flag = mpz_set_str(result2, ((char*) arg2), 10);
		assert(flag == 0); //If the flag is not 0, then the operation failed  
		
		//Perform addition
		mpz_add(addition_result, result1, result2);
	} else if (((arg1 & result_mask) == type_bignum) && ((arg2 & result_mask) == type_integer)) { //We are adding an integer to a bignum
		/* Initialize the number */
		mpz_init(result1);
		mpz_init(addition_result);

		mpz_set_ui(addition_result, 0);
		mpz_set_ui(result1, 0);

		arg1 = (arg1 ^ type_bignum);

		
		mpz_init(result2);
		mpz_set_ui(result2, 0);

		
		//Parse the value as a base 10 number, reading it directly from the heap
		//from the beginning of the string to the null character
		flag = mpz_set_str(result1, ((char*) arg1), 10);
		assert(flag == 0); //If the flag is not 0, then the operation failed


		mpz_set_si(result2, (arg2 >> result_shift));

		mpz_add(addition_result, result1, result2);
	} else if (((arg1 & result_mask) == type_integer) && ((arg2 & result_mask) == type_bignum)) { //We are adding an integer to a bignum
		/* Initialize the number */
		mpz_init(result1);
		mpz_init(addition_result);

		mpz_set_ui(addition_result, 0);
		mpz_set_si(result1, (arg1 >> result_shift));

		mpz_init(result2);
		mpz_set_ui(result2, 0);

		arg2 = (arg2 ^ type_bignum);

		//Parse the value as a base 10 number, reading it directly from the heap
		//from the beginning of the string to the null character
		flag = mpz_set_str(result2, ((char*) arg2), 10);
		assert(flag == 0); //If the flag is not 0, then the operation failed


		mpz_add(addition_result, result1, result2);
	} else {
		error();
	}

	//Get the string representation of the result on the heap, as this is how bignums are stored in obe
	char* res_str = mpz_get_str(((char *) arg0), 10, addition_result);
	
	int64_t len =  strlen(res_str) + 1; //Include the null char
	int64_t padding = ((len % 8) == 0) ? 0 : (8 - (len % 8));

	return (len + padding);
}

/* Add two big nums and return the length of the result string  */
int64_t  subBignum(int64_t arg0, int64_t  arg1, int64_t arg2) {
	int flag; //To hold the result of parsing the return value as a base 10 number.
	mpz_t result1;
	mpz_t result2;
	mpz_t addition_result;
	

	if(((arg1 & result_mask) == type_bignum) && ((arg2 & result_mask) == type_bignum)) { //We are addign two bignums
		/* Initialize the number */
		mpz_init(result1);
		mpz_init(addition_result);

		mpz_set_ui(addition_result, 0);
		mpz_set_ui(result1, 0);

		arg1 = (arg1 ^ type_bignum);

		//Parse the value as a base 10 number, reading it directly from the heap
		//from the beginning of the string to the null character
		flag = mpz_set_str(result1, ((char*) arg1), 10);
		assert(flag == 0); //If the flag is not 0, then the operation failed

		
		mpz_init(result2);
		mpz_set_ui(result2, 0);

		//Clear the tagging to get the address. The location where the result will be placed is untagged
		arg2 = (arg2 ^ type_bignum);

		flag = mpz_set_str(result2, ((char*) arg2), 10);
		assert(flag == 0); //If the flag is not 0, then the operation failed  
		
		//Perform subtraction
		mpz_sub(addition_result, result1, result2);
			
	} else if (((arg1 & result_mask) == type_bignum) && ((arg2 & result_mask) == type_integer)) { //We are adding an integer to a bignum
		/* Initialize the number */
		mpz_init(result1);
		mpz_init(addition_result);

		mpz_set_ui(addition_result, 0);
		mpz_set_ui(result1, 0);

		arg1 = (arg1 ^ type_bignum);

		
		mpz_init(result2);
		mpz_set_ui(result2, 0);

		
		//Parse the value as a base 10 number, reading it directly from the heap
		//from the beginning of the string to the null character
		flag = mpz_set_str(result1, ((char*) arg1), 10);
		assert(flag == 0); //If the flag is not 0, then the operation failed


		mpz_set_si(result2, (arg2 >> result_shift));

		mpz_sub(addition_result, result1, result2);
	
	} else if (((arg1 & result_mask) == type_integer) && ((arg2 & result_mask) == type_bignum)) { //We are adding an integer to a bignum
		/* Initialize the number */
		mpz_init(result1);
		mpz_init(addition_result);

		mpz_set_ui(addition_result, 0);
		mpz_set_si(result1, (arg1 >> result_shift));

		mpz_init(result2);
		mpz_set_ui(result2, 0);

		arg2 = (arg2 ^ type_bignum);

		//Parse the value as a base 10 number, reading it directly from the heap
		//from the beginning of the string to the null character
		flag = mpz_set_str(result2, ((char*) arg2), 10);
		assert(flag == 0); //If the flag is not 0, then the operation failed


		mpz_sub(addition_result, result1, result2);
	} else {
		error();
	}


	//Get the string representation of the result on the heap, as this is how bignums are stored in obe
	char* res_str = mpz_get_str(((char *) arg0), 10, addition_result);
	
	int64_t len =  strlen(res_str) + 1; //Include the null char
	int64_t padding = ((len % 8) == 0) ? 0 : (8 - (len % 8));

	return (len + padding);
}

/* Compare two bignums  */
int64_t compBignum(int64_t arg0, int64_t arg1) {
	mpz_t result1;
	mpz_t result2;
	int flag = 0;

	mpz_init(result1);
	mpz_init(result2);

	mpz_set_ui(result1, 0);
	mpz_set_ui(result2, 0);

	/* Make sure that both arguments are pointers to bingums */
	if(((arg0 & result_mask) != type_bignum)  || ((arg1 & result_mask) != type_bignum)) {
		error();
	}


	/* Untag the arguments */
	arg0 = arg0 ^ type_bignum;
	arg1 = arg1 ^ type_bignum;
	
	/* Set the number using its string representation on the heap */
	flag = mpz_set_str(result1, (char *) arg0, 10);
	assert(flag == 0);

	flag = mpz_set_str(result2, (char *) arg1, 10);
	assert(flag == 0);

	/* Return the result of comparing the two bignums using the mpz_cmp function from the GMP library */	
	int comp_res =  mpz_cmp(result1, result2);

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
	} else if(compValue(*a0, *a1) != 0) {
		return -1;
	} else {
		return listEqual(*(a0 + 1), *(a1 + 1));
	}
}

/* Determine if two pairs are equal by comparing successive pairs of elements.
Returns -1 if not equal and 0 if it is. */
int64_t pairEqual(int64_t arg0, int64_t arg1) { 
	int64_t* a0 = (int64_t*)(arg0 ^ type_pair);
	int64_t* a1 = (int64_t*)(arg1 ^ type_pair);
	
	//Cpmpare the first value of each pair
	if(compValue(*a0, *a1) != 0) {
		return -1;
	} 

	if(((((*(a0 + 1)) & result_mask) == type_pair) && 
		(((*(a1 + 1)) & result_mask) != type_pair)) ||
	   ((((*(a0 + 1)) & result_mask) != type_pair) && 
		(((*(a1 + 1)) & result_mask) == type_pair))) {
		return -1;
	} else if(((((*(a0 + 1)) & result_mask) != type_pair) && 
		(((*(a1 + 1)) & result_mask) != type_pair))) { 
		return compValue(*(a0 + 1), *(a1 + 1));
	} else {
		return pairEqual(*(a0 + 1), *(a1 + 1));
	}
}
/* Print a value. If printEmpty is true, print the empty list symbol, otherwise do not */
void printValue(int64_t value) {
	switch(value & result_mask) {
	case type_integer:
		printf("%" PRId64, value >> result_shift);
		break;
	case type_bignum:		
		printBignum(value);	
		break;
	case type_list:
		printf("'(");
		printList(value);
		printf(")");
		break;
	case type_pair:
		printf("'(");
		printPair(value);
		printf(")");
		break;
	case type_empty_list:
		printf("'()");
		break;
	case type_true:
		printf("#t");
		break;
	case type_false:
		printf("#f");
		break;	
	}

}

/* Print a list of values */
void printList(int64_t value) {
	int64_t* start_addr = (int64_t *)(value ^ type_list);
	if(*start_addr != type_empty_list) {
		printValue(*start_addr);
		value = *(start_addr + 1);
		if ((value & result_mask) == type_list) {
			//Print a space if the next thing to print is not the empty list
			if((*((int64_t* )(value ^ type_list)) & result_mask) != type_empty_list) {
				printf(" ");
			}
			printList(value);
		} else if((value & result_mask) == type_empty_list) {
			//Do nothing
		} else {

		}
	}
}

/* Print a pair of values */
void  printPair(int64_t value) {
	int64_t* start_addr  = (int64_t *)(value ^ type_pair);

		printValue(*start_addr);
		value = *(start_addr + 1);

		//Check if the next value is another pair or the final value in
		//a chain of pairs
		if((value & result_mask) == type_empty_list) {
			//Do nothing
		}
		else if((value & result_mask) == type_pair) {
			printf(" ");
			printPair(value);
		} else {
			printf(" . ");
			printValue(value);
		}
	

}

/* Given a pointer to a bignum string on the heap, print the value of the bignum */
void printBignum(int64_t value) {
	int flag; //To hold the result of parsing the return value as a base 10 number.
	mpz_t result;

	/* Initialize thei number */
	mpz_init(result);
	mpz_set_ui(result, 0);


	//Clear the tagging to get the address
	value = (value ^ type_bignum);

	//printf("%s\n", ((char*) value));
	//Parse the value as a base 10 number, reading it directly from the heap
	//from the beginning of the string to the null character
	flag = mpz_set_str(result, ((char*) value), 10);
	assert(flag == 0); //If the flag is not 0, then the operation failed 	

	mpz_out_str(stdout,10,result);
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
	switch(value1 & result_mask) {
		case type_bignum:
			return compBignum(value1, value2);
		case type_list:
			return listEqual(value1, value2);
		case type_pair:
			return pairEqual(value1, value2);
		case type_true:
		case type_false:
		case type_empty_list:
			if(value1 == value2) {
				return type_true;
			} else {
				return type_false;
			}
		default:
			error();
	}
}

/* Signal an error while executing the program */
int error() {
	printf("err\n");
	exit(0); //The program being executed errored out, but the runtime system did not
}

/* Signal an error in the runtime system */
int runtimeSystemError(void) {
	exit(1);
}
