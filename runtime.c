#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>

int64_t entry(void);

/* From this method, the compiled program is initiated by calling the entry function in assembly.
The entry function is linked with this RTS*/
int main(int argc, char** argv) {
	int64_t result = entry();
	printf("%" PRId64 "\n", result); 
	return 0;
}
