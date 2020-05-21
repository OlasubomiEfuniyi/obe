#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>
#include <gmp.h>
#include <assert.h>


#define result_mask 1
#define result_shift 1
#define type_integer 0
#define type_bignum 1
#define heap_size 10000000

int64_t entry(void* heap);

/* From this method, the compiled program is initiated by calling the entry function in assembly.
The entry function is linked with this RTS 
We use the GMP Library to support arbitrary precision integers */
int main(int argc, char** argv) {
	void* heap = NULL;
	heap = malloc(heap_size);

	if(heap == NULL) {
		fprintf(stderr, "could not allocate heap space");
		exit(-1);
	}

	/* Entry may either return a 64 bit value via a register or a pointer to a result */
	int64_t  value = entry(heap);
	int flag; //To hold the result of parsing the return value as a base 10 number.
	mpz_t result;

	switch(value & result_mask) {
	case type_integer:
		printf("%" PRId64 "\n", value >> result_shift);
		break;
	case type_bignum:		
		/* Initialize thei number */
		mpz_init(result);
		mpz_set_ui(result, 0);

		//Clear the tagging to get the address
		value = (value ^ type_bignum);

		//Parse the value as a base 10 number, reading it directly from the heap
		//from the beginning of the string to the null character
		flag = mpz_set_str(result, ((char*) value), 10);
		assert(flag == 0); //If the flag is not 0, then the operation failed 	
		mpz_out_str(stdout,10,result);
		printf("\n");
		break;
	} 
	

	free(heap);
	return 0;
}
