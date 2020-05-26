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

#define heap_size 10000000

int64_t entry(void* heap);
void printBignum(int64_t value);
int error(void);

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
	
	switch(value & result_mask) {
	case type_integer:
		printf("%" PRId64 "\n", value >> result_shift);
		break;
	case type_bignum:		
		printBignum(value);	
		break;
	case type_true:
		printf("#t\n");
		break;
	case type_false:
		printf("#f\n");
		break;	
	}

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
	printf("\n");
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

/* Signal an error while executing the program */
int error() {
	printf("err\n");
	exit(0); //The program being executed errored out, but the runtime system did not
}
