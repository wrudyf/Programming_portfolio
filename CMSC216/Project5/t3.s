   .data

# array terminated by 0 (which is not part of the array)
xarr:
   .word 1
   .word 12
   .word 225
   .word 169
   .word 16
   .word 25
   .word 100
   .word 81
   .word 99
   .word 121
   .word 144
   .word 0 

   .text

# main(): ##################################################
#   uint* j = xarr
#   while (*j != 0):
#     printf(" %d\n", isqrt(*j))
#     j++
#
main:
   # PROLOGUE
   subu $sp, $sp, 8        # expand stack by 8 bytes
   sw   $ra, 8($sp)        # push $ra (ret addr, 4 bytes)
   sw   $fp, 4($sp)        # push $fp (4 bytes)
   addu $fp, $sp, 8        # set $fp to saved $ra

   subu $sp, $sp, 8        # save s0, s1 on stack before using them
   sw   $s0, 8($sp)        # push $s0
   sw   $s1, 4($sp)        # push $s1

   la   $s0, xarr          # use s0 for j. init to xarr
main_while:
   lw   $s1, ($s0)         # use s1 for *j
   beqz $s1, main_end      # if *j == 0 go to main_end
   move $a0, $s1           # result (in v0) = isqrt(*j)
   jal  isqrt              # 
   move $a0, $v0           # print_int(result)
   li   $v0, 1
   syscall
   li   $a0, 10            # print_char('\n')
   li   $v0, 11
   syscall
   addu $s0, $s0, 4        # j++
   b    main_while
main_end:
   lw   $s0, -8($fp)       # restore s0
   lw   $s1, -12($fp)      # restore s1

   # EPILOGUE
   move $sp, $fp           # restore $sp
   lw   $ra, ($fp)         # restore saved $ra
   lw   $fp, -4($sp)       # restore saved $fp
   j    $ra                # return to kernel
# end main #################################################
#name: rudy f
#uid: 114096296
#umd directory id: rfuente5

isqrt:	
	#PROLOGUE
	subu		$sp, $sp, 8
	sw		$ra, 8($sp)
	sw		$fp, 4($sp)
	addu		$fp, $sp, 8

	#BODY

	bge		$a0, 2, recur	#if n >= 2, do some recursion
	move		$v0, $a0	#return n if n < 2
	j		return

recur:	
	subu		$sp, $sp, 8	#grow stack by 8 bytes for 2 ints
	sw		$a0, 4($sp)	#save value of arg n on stack

	div		$a0, $a0, 4	#divide n by 4
	jal		isqrt		#call isqrt with new value of n

	sw		$v0, 8($sp)	#save isqrt(n/4) on stack
	lw		$s3, -8($fp)	#small = isqrt ( n/4 )
	mul		$s3, $s3, 2	#small = isqrt(n/4) * 2

	add		$s4, $s3, 1	#large = small + 1

	move		$s6, $s4	#copy large to s6	
	
	mul		$s4, $s4, $s4	#large = large * large

	lw		$s5, 4($sp)	#getting value of n in a registry
	bgt		$s4, $s5, rets	#if large * large > n go to ret small

	move		$v0, $s6	#return large	
	j		return
	
rets:
	move 		$v0, $s3	#return small
	
	
	
return:	
	#EPILOGUE
	move		$sp, $fp
	lw		$ra, ($fp)
	lw		$fp, -4($sp)
	jr		$ra
