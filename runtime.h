#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>
#include <gmp.h>
#include <assert.h>
#include <string.h>

#define result_shift 3
#define imm_shift (result_shift + 3)
#define result_type_mask ((1 << result_shift) - 1)
#define imm_type_mask ((1 << imm_shift) - 1)
#define clear_tag 0xfffffffffffffff8

#define type_imm 0b000
#define type_box 0b001
#define type_bignum 0b010
#define type_list  0b011
#define type_pair 0b100
#define type_range 0b101

#define type_true ((0b000 << result_shift))
#define type_false ((0b001 << result_shift))
#define type_empty_list ((0b010 << result_shift))

#define true 1
#define false 0
#define GC_INFO(...) (gc_info && printf(__VA_ARGS__))
 
typedef char bool;

//Functions to be shared by garbage collector and rts
void printValue(int64_t value);
