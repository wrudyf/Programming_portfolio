   .data
# uint arrays, each terminated by -1 (which is not part of array)
data0:
   .word 1, 2, 3, 4, -1
data1:
   .word 2, 3, 4, 5, -1
data2:
   .word 5, 4, 3, 2,  -1
data3:
   .word 200456, 3345056, 1, 2, 1, 2, -1
overflow:
   .word 1, 1, 1, 1, 2147483646, -1

   .text

# main(): ##################################################
#   process_array(data0)
#   process_array(data1)
#   process_array(data2)
#   process_array(data3)
#   process_array(overflow)
#
main:
   # PROLOGUE
   subu $sp, $sp, 8        # expand stack by 8 bytes
   sw   $ra, 8($sp)        # push $ra (ret addr, 4 bytes)
   sw   $fp, 4($sp)        # push $fp (4 bytes)
   addu $fp, $sp, 8        # set $fp to saved $ra

   la   $a0, data0
   jal  process_array
   la   $a0, data1
   jal  process_array
   la   $a0, data2
   jal  process_array
   la   $a0, data3
   jal  process_array
   la   $a0, overflow
   jal  process_array

   # EPILOGUE
   move $sp, $fp           # restore $sp
   lw   $ra, ($fp)         # restore saved $ra
   lw   $fp, -4($sp)       # restore saved $fp
   j    $ra                # return to kernel
## end main ################################################

# process_array(uint* arr): #################################
#   print_array(arr)
#   reverse_prefix_sum(arr)
#   print_array(arr)
#
process_array:
   # PROLOGUE
   subu $sp, $sp, 8        # expand stack by 8 bytes
   sw   $ra, 8($sp)        # push $ra (ret addr, 4 bytes)
   sw   $fp, 4($sp)        # push $fp (4 bytes)
   addu $fp, $sp, 8        # set $fp to saved $ra

   subu $sp, $sp, 4        # save s0 on stack before using it
   sw   $s0, 4($sp)

   move $s0, $a0           # use s0 to save a0
   jal  print_array
   move $a0, $s0
   jal  reverse_prefix_sum
   move $a0, $s0
   jal  print_array

   lw   $s0, -8($fp)       # restore s0 from stack

   # EPILOGUE
   move $sp, $fp           # restore $sp
   lw   $ra, ($fp)         # restore saved $ra
   lw   $fp, -4($sp)       # restore saved $fp
   j    $ra                # return to kernel
## end process_array #######################################

# print_array(uint arr): ########################################
#   uint x
#   while (-1 != (x = *arr++)):
#     printf("%d ", x)
#   printf("\n")
#
print_array:
   # use t0 to hold arr. use t1 to hold *arr
   move $t0, $a0
print_array_while:
   lw   $t1, ($t0)
   beq  $t1, -1, print_array_endwhile
   move $a0, $t1           # print_int(*arr)
   li   $v0, 1
   syscall
   li   $a0, 32            # print_char(' ')
   li   $v0, 11
   syscall
   addu $t0, $t0, 4
   b    print_array_while
print_array_endwhile:
   li   $a0, 10            # print_char('\n')
   li   $v0, 11
   syscall
   jr   $ra
## end print_array #########################################
#name: rudy f
#uid: 114096296
#umd directory id: rfuente5

reverse_prefix_sum:
	
	#PROLOGUE
	subu		$sp, $sp, 8
	sw		$ra, 8($sp)
	sw		$fp, 4($sp)
	addu		$fp, $sp, 8
	
	#BODY
	subu		$sp, $sp, 8	#grow stack for rsp(arr + 1) and *arr
	
	li		$s2, 0		#initialize int r with 0
	lw		$s3, ($a0)	#set s3 equal to first int in array
	sw		$s3, 4($sp)
	
	beq		$s3, -1, retz	#go to return zero if *arr == -1

	add		$a0, $a0, 4	#increase addr by 4 to get next element
					#in array
	
	jal		reverse_prefix_sum
					#do reverse_prefix_sum(arr + 1)

	sw		$v0, 8($sp)
	lw		$s4, -8($fp)
	lw		$s3, 4($sp)
	
	addu		$s2, $s4, $s3	#r = rsp(arr + 1) + (int)*arr

	sub		$a0, $a0, 4	#get back to first element in addr


#	add		$s2, $s2, $s3

	sw		$s2 0($a0)	#*arr = r
	
	move		$v0, $s2	#return (r)
	j		return
	
retz:
	li		$v0, 0
	j		return

return:	
	#EPILOGUE
	move		$sp, $fp
	lw		$ra, ($fp)
	lw		$fp, -4($sp)
	jr		$ra
	
